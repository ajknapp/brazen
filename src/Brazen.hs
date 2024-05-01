{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Brazen where

-- import Control.Monad.Codensity
-- import Control.Monad.ST
-- import Control.Monad.State

import Brazen.AD
import Brazen.Numeric
import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Category
import Control.Lens
import Data.HKD
import Data.Int
import Data.Proxy
import Data.Word
import Foreign (Ptr)
import Foreign.C
import GHC.Generics
import GHC.TypeLits
import Janus.Command.Array
import Janus.Command.Format
import Janus.Command.IO
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Bits
import Janus.Expression.Cast
import Janus.Expression.Ord
import Linear
import qualified Numeric.AD as AD
import Prelude hiding (id, (.))
-- import Janus.Command.Cond
-- import Janus.Expression.Eq

data Joint f g a = Joint {_parameters :: f a, _observations :: g a}
  deriving (Generic)

$(makeLenses ''Joint)

data TwoSamplePrior f a = TwoSamplePrior {_mu1, _mu2 :: f a}
  deriving (Generic)

$(makeLenses ''TwoSamplePrior)

data TwoSampleLikelihood f a = TwoSampleLikelihood {_obsGroup1, _obsGroup2 :: f a}
  deriving (Generic)

$(makeLenses ''TwoSampleLikelihood)

data HMCState s e a = HMCState {_hmcState :: s, _hmcLP :: Maybe (DVar e a)}

$(makeLenses ''HMCState)

-- newtype HMC m e s c a b = HMC {getHMC :: StateArrow (HMCState s e c) (RAD m e c) a b}
newtype HMC m e s c a b = HMC {getHMC :: RAD m e c (a, HMCState s e c) (b, HMCState s e c)}
  deriving (Category, Arrow, ArrowChoice) via StateArrow (HMCState s e c) (RAD m e c)

normal :: (CmdRAD m e a, Floating (e a)) => Getting (DVar e a) s (DVar e a) -> HMC m e s a (DVar e a, DVar e a) (DVar e a)
normal l = HMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  -- 0.5*(theta-mu)^2/sigma2
  let half = auto 0.5
  x <- subA -< (theta, mu)
  y <- mulA -< (x, x)
  z <- divA -< (y, sigma2)
  z' <- mulA -< (half, z)
  -- log (2*pi*sigma2)
  let twopi = auto $ 2 * pi
  twopis2 <- mulA -< (twopi, sigma2)
  ltwopis2 <- logA -< twopis2
  z'' <- mulA -< (half, ltwopis2)
  lp <- addA -< (z', z'')
  case hmc ^. hmcLP of
    Nothing -> do returnA -< (theta, hmc & hmcLP ?~ lp)
    Just lp' -> do
      lp'' <- addA -< (lp, lp')
      returnA -< (theta, hmc & hmcLP ?~ lp'')

twoSampleModel ::
  (CmdRAD m e a, Floating (e a), KnownNat n) =>
  HMCModel m e (TwoSamplePrior (DVar e)) (TwoSampleLikelihood (DTensor '[n] e)) a
twoSampleModel = proc _ -> do
  mu1' <- normal (parameters . mu1) -< (auto 0, auto 1)
  mu2' <- normal (parameters . mu2) -< (auto 0, auto 1)
  x1' <- iidNormal (observations . obsGroup1) -< (mu1', auto 1)
  x2' <- iidNormal (observations . obsGroup2) -< (mu2', auto 1)
  returnA -< Joint (TwoSamplePrior mu1' mu2') (TwoSampleLikelihood x1' x2')

twoSamplePrior ::
  (CmdRAD m e a, Floating (e a)) =>
  Getting (DVar e a) s (TwoSamplePrior (DVar e) a) ->
  HMC m e s a () (TwoSamplePrior (DVar e) a)
twoSamplePrior l = proc _ -> do
  mu1' <- normal (l . mu1) -< (auto 0, auto 1)
  mu2' <- normal (l . mu2) -< (auto 0, auto 1)
  returnA -< TwoSamplePrior mu1' mu2'

twoSampleLikelihood ::
  (CmdRAD m e a, Floating (e a), KnownNat n) =>
  Getting (DTensor '[n] e a) s (TwoSampleLikelihood (DTensor '[n] e) a) ->
  HMC m e s a (TwoSamplePrior (DVar e) a) (TwoSampleLikelihood (DTensor '[n] e) a)
twoSampleLikelihood l = proc (TwoSamplePrior mu1' mu2') -> do
  x1' <- iidNormal (l . obsGroup1) -< (mu1', auto 1)
  x2' <- iidNormal (l . obsGroup2) -< (mu2', auto 1)
  returnA -< TwoSampleLikelihood x1' x2'

type HMCModel m e f g a = HMC m e (Joint f g a) a () (Joint f g a)

twoSample ::
  (CmdRAD m e a, Floating (e a), KnownNat n) =>
  HMCModel m e (TwoSamplePrior (DVar e)) (TwoSampleLikelihood (DTensor '[n] e)) a
twoSample = proc _ -> do
  theta <- twoSamplePrior parameters -< ()
  x <- twoSampleLikelihood observations -< theta
  returnA -< Joint theta x

iidNormal :: (CmdRAD m e a, Floating (e a), KnownNat n) => Getting (DVector n e a) s (DVector n e a) -> HMC m e s a (DVar e a, DVar e a) (DVector n e a)
iidNormal l = HMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNMapSum normal' (AD.grad normal') -< V3 (DTBroadcast mu) (DTBroadcast sigma2) theta
  case hmc ^. hmcLP of
    Nothing -> returnA -< (theta, hmc & hmcLP ?~ lp)
    Just lp' -> do
      lp'' <- addA -< (lp, lp')
      returnA -< (theta, hmc & hmcLP ?~ lp'')
  where
    normal' (V3 mu' sigma2' x') = let y = x' - mu' in 0.5 * (y * y / sigma2' - log (2 * pi * sigma2'))

runHMC :: (CmdRAD m e a, Floating (e a)) => HMC m e s a s b -> RAD m e a s (b, DVar e a)
runHMC (HMC lp) = proc s -> do
  (b, hmc') <- lp -< (s, HMCState s Nothing)
  case hmc' ^. hmcLP of
    Nothing -> returnA -< (b, auto 0)
    Just lp' -> returnA -< (b, lp')

updatePosition ::
  ( CmdRef m e Int64,
    CmdWhile m e,
    ExpOrd e Int64,
    CmdStorable m e a,
    Num (e a),
    Num (e Int64)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  e a ->
  Int ->
  m ()
updatePosition pos mom eps n = do
  rangeM 0 (fromIntegral n) $ \k -> do
    qk <- peekElemOff pos k
    pk <- peekElemOff mom k
    pokeElemOff pos (qk + eps * pk) k

updateMomentum ::
  ( CmdRef m e Int64,
    CmdRef m e a,
    CmdWhile m e,
    ExpOrd e Int64,
    CmdStorable m e a,
    Floating (e a),
    Num (e Int64)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  e a ->
  Int ->
  m (e a)
updateMomentum g u eps n = do
  norm' <- newRef 0
  gu <- newRef 0
  rangeM 0 (fromIntegral n) $ \k -> do
    gk <- peekElemOff g k
    uk <- peekElemOff u k
    modifyRef norm' (+ gk * gk)
    modifyRef gu (+ gk * uk)
  gnorm2 <- readRef norm'
  gu' <- readRef gu
  gnorm' <- letM $ sqrt gnorm2
  delta <- letM $ eps * gnorm' / fromIntegral (n - 1)
  zeta <- letM $ exp $ negate delta
  ue <- letM $ negate gu' / gnorm'
  writeRef norm' 0
  rangeM 0 (fromIntegral n) $ \k -> do
    gk <- peekElemOff g k
    uk <- peekElemOff u k
    uk' <- letM $ gk * (zeta - 1) * (1 + zeta + ue * (1 - zeta)) / gnorm' + uk * (2 * zeta)
    pokeElemOff u uk' k
    modifyRef norm' (+ uk' * uk')
  norm2' <- readRef norm'
  norm'' <- letM $ sqrt norm2'
  rangeM 0 (fromIntegral n) $ \k -> do
    uk <- peekElemOff u k
    pokeElemOff u (uk / norm'') k
  letM $ delta - log 2 + log (1 + ue + (1 - ue) * zeta * zeta)

data Foo e a f = Foo {_foo1, _foo2 :: f (Var e a)}
  deriving (Generic)

instance FFunctor (Foo e a) where ffmap = ffmapDefault

instance FFoldable (Foo e a) where ffoldMap = ffoldMapDefault

instance FTraversable (Foo e a) where ftraverse = gftraverse

instance FZip (Foo e a) where fzipWith = gfzipWith

instance FRepeat (Foo e a) where frepeat = gfrepeat

instance Flat (Foo e a Primal)

fieldNames :: Foo e a (Const String)
fieldNames = Foo (Const "mu1") (Const "mu2")

openSampleFiles :: (CmdIO m e, CmdString m e, FTraversable f) => f (Const String) -> m (f (Const (e (Ptr CFile))))
openSampleFiles = ftraverse $ \(Const x) -> fmap Const $ withString (x <> ".csv") $ \fp ->
  withString "w" $ \mode -> fopen fp mode

writeSampleHeaders ::
  (FFoldable t, FZip t, CmdPutString m e) =>
  t (Const String) ->
  t (Const (e (Ptr CFile))) ->
  m ()
writeSampleHeaders fields fps =
  ftraverse_ (\(Const x) -> x) $
    fzipWith (\(Const field) (Const fp) -> Const $ withString (field <> "\n") $ hputString fp) fields fps

writeSamples ::
  (CmdFormat m e a, CmdStorable m e a, FZip t, FFoldable t) =>
  t (Const (e (Ptr CFile))) ->
  t (Const (e (Ptr a))) ->
  m ()
writeSamples fps samples =
  ftraverse_ getConst $
    fzipWith (\(Const fp) (Const sample) -> Const $ peek sample >>= \a -> hformat fp a "\n") fps samples

closeSampleFiles :: (CmdIO m e, FFoldable f) => f (Const (e (Ptr CFile))) -> m ()
closeSampleFiles = ftraverse_ (\(Const fp) -> fclose fp)

ptrProxy :: e (Ptr a) -> Proxy a
ptrProxy _ = Proxy

dvar :: Var e a -> Var e a -> DVar e a
dvar = DVar

instance (ExpSized e a, ExpPtr e a) => Tangential Foo e a where
  unpack tape = Foo (PrimalV $ Var tape) (PrimalV $ Var $ tape `ptrAdd` sizeOf (ptrProxy tape))

data HMCState' e a = HMCState'
  { _hmcPos, _hmcMom :: e (Ptr a),
    _hmcDim :: Int
  }

foreign import ccall rand :: IO CInt

randf :: IO Float
randf = do
  r <- rand
  pure $ fromIntegral r / fromIntegral (maxBound :: CInt)

randn :: IO (Identity Float)
randn = do
  u1 <- randf
  u2 <- randf
  pure . pure $ sqrt (-2 * log u1) * cos (2 * pi * u2)

virial ::
  ( CmdRef m e a,
    CmdRef m e Int64,
    CmdWhile m e,
    ExpOrd e Int64,
    CmdStorable m e a,
    Fractional (e a),
    Integral (e Int64)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  e Int64 ->
  m (e a)
virial x u g n = do
  t1 <- newRef 0
  t2 <- newRef 0
  t3 <- newRef 0
  rangeM 0 n $ \i -> do
    xi' <- peekElemOff x i
    ui' <- peekElemOff u i
    gi' <- peekElemOff g i
    modifyRef t1 (+ (xi' * gi'))
    modifyRef t2 (+ (xi' * ui'))
    modifyRef t3 (+ (ui' * gi'))
  (t1', t2', t3') <- (,,) <$> readRef t1 <*> readRef t2 <*> readRef t3
  letM $ 1 - (t1' - t2' * t3') / fromIntegral (n - 1)

foobar :: IO ()
foobar = do
  let eps = 2.5e-2
      halfeps = 0.5 * eps
      lambda_c = 0.1931833275037836
      epslam = eps * lambda_c
      rho = eps * (1 - 2 * lambda_c)
      n = 2
  fps <- openSampleFiles fieldNames
  writeSampleHeaders fieldNames fps
  fv <- withString "virial.csv" $ \file ->
    withString "w" $ \mode -> do
      fv' <- fopen file mode
      withString "virial\n" $ \header -> hputString fv' header
      pure fv'
  mom <- ptrCast <$> calloc 2 (sizeOf (Proxy @Float))
  ivirial <- newRef 0
  trajLen <- newRef (0 :: Identity Int64)
  a' <- randn
  b' <- randn
  mn <- letM $ sqrt $ a' * a' + b' * b'
  pokeElemOff mom (a' / mn) 0
  pokeElemOff mom (b' / mn) 1
  withGrad @_ @IO @Identity @Float (arr (\(Foo (Dual x dx) (Dual y dy)) -> (dvar x dx, dvar y dy)) >>> op2 (\x y -> (0.5 * (x * x + y * y), \dz -> (x * dz, y * dz)))) $ \tape grad -> do
    let Tape tape' dtape' = tape
    a <- randn
    b <- randn
    pokeElemOff tape' a 0
    pokeElemOff tape' b 1
    _ <- grad
    rangeM (0 :: Identity Int64) 1000000 $ \_ -> do
      -- minimal norm step
      _ <- updateMomentum dtape' mom epslam n
      updatePosition tape' mom halfeps n
      _ <- grad
      _ <- updateMomentum dtape' mom rho n
      updatePosition tape' mom halfeps n
      _ <- grad
      _ <- updateMomentum dtape' mom epslam n
      -- save state
      -- whenM_ (i `mod` 2 `eq` 0) $ do
      do  writeSamples fps $ Foo (Const tape') (Const (tape' `ptrAdd` sizeOf (Proxy @Float)))
      -- virial
      v <- virial tape' mom dtape' (fromIntegral n)
      modifyRef ivirial (+ v)
      iv <- readRef ivirial
      trajLen' <- readRef trajLen
      writeRef trajLen (trajLen' + 1)
      hformat fv iv "\n"
      -- bounce
      let nu = 1 / sqrt (fromIntegral n * 48)
      u0' <- peekElemOff mom 0
      u1' <- peekElemOff mom 1
      u0 <- (+ u0') . (* nu) <$> randn
      u1 <- (+ u1') . (* nu) <$> randn
      unorm <- letM $ sqrt (u0 * u0 + u1 * u1)
      pokeElemOff mom (u0 / unorm) 0
      pokeElemOff mom (u1 / unorm) 1
      writeRef ivirial 0
      writeRef trajLen 0
  closeSampleFiles fps
  _ <- fclose fv
  pure ()

pcgNext ::
  ( CmdRef m e Word64,
    CmdRef m e Word32,
    Num (e Word64),
    Num (e Int),
    ExpBits e Word64,
    ExpBits e Word32,
    ExpIntegralCast e Word64 Word32,
    ExpIntegralCast e Word64 Int
  ) =>
  Ref m e Word64 ->
  e Word64 ->
  m (e Word32)
pcgNext state inc = do
  oldstate <- readRef state
  writeRef state (oldstate * 6364136223846793005 + (inc `ior` 1))
  xorshifted <- letM . toIntegral $ ((oldstate `rshift` 18) `ieor` oldstate) `rshift` 27
  rot <- letM $ oldstate `rshift` 59
  letM $ (xorshifted `rshift` toIntegral rot) `ior` (xorshifted `lshift` toIntegral (negate rot `iand` 31))
