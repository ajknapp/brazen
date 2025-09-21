{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brazen where

import Brazen.AD
import Brazen.Distributions
import Brazen.FieldNames
import Brazen.Shared
import Brazen.Tune
import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Functor.Product
import Data.HKD
import Data.Int
import Data.Proxy
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word
import Foreign (Ptr)
import Foreign.C
import Foreign.Marshal.Alloc
import GHC.Generics
import GHC.TypeLits
import Janus.Backend.C
import Janus.Command.Array
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.IO
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Bits
import Janus.Expression.Bool
import Janus.Expression.Cast
import Janus.Expression.Eq
import Janus.Expression.Inject
import Janus.Expression.Ord
import Janus.FFI.Arg
import Janus.Typed
import Prelude hiding (id, (.))

updatePosition ::
  ( JanusTyped e a,
    JanusTyped e Int64,
    CmdRange m e,
    CmdRef m e,
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
  ( JanusTyped e a,
    JanusTyped e Int64,
    CmdFormat m e a,
    CmdRange m e,
    CmdRef m e,
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
  ue <- letM $ gu' / gnorm'
  zeta <- letM $ exp $ negate delta
  writeRef norm' 0
  rangeM 0 (fromIntegral n) $ \k -> do
    gk <- peekElemOff g k
    uk <- peekElemOff u k
    uk' <- letM $ negate gk * (1 - zeta) * (1 + zeta + ue * (1 - zeta)) / gnorm' + 2 * zeta * uk
    pokeElemOff u uk' k
    modifyRef norm' (+ uk' * uk')
  norm2' <- readRef norm'
  norm'' <- letM $ sqrt norm2'
  rangeM 0 (fromIntegral n) $ \k -> do
    uk <- peekElemOff u k
    pokeElemOff u (uk / norm'') k
  letM . (* fromIntegral (n-1)) $ delta - log 2 + log (1 + ue + (1 - ue) * zeta * zeta)

minimalNormStep ::
  ( JanusTyped e a,
    JanusTyped e Int64,
    CmdRange m e,
    CmdRef m e,
    CmdWhile m e,
    ExpOrd e Int64,
    CmdStorable m e a,
    CmdFormat m e a,
    Floating (e a),
    Num (e Int64)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  Int ->
  m b ->
  e a ->
  m b
minimalNormStep tape dtape mom n grad eps = do
  let lambda_c = 0.1931833275037836
  halfeps <- letM $ 0.5 * eps
  epslam <- letM $ lambda_c * eps
  rho <- letM $ eps * (1 - 2 * lambda_c)
  _ <- updateMomentum dtape mom epslam n
  updatePosition tape mom halfeps n
  _ <- grad
  _ <- updateMomentum dtape mom rho n
  updatePosition tape mom halfeps n
  res <- grad
  _ <- updateMomentum dtape mom epslam n
  pure res

leapfrogStep ::
  ( JanusTyped e a,
    JanusTyped e Int64,
    CmdRange m e,
    CmdRef m e,
    CmdWhile m e,
    ExpOrd e Int64,
    CmdStorable m e a,
    CmdFormat m e a,
    Floating (e a),
    Num (e Int64)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  Int ->
  m (e a, b, c) ->
  e a ->
  m (e a, e a, b, c)
leapfrogStep tape dtape mom n grad eps = do
  halfeps <- letM $ 0.5 * eps
  k1 <- updateMomentum dtape mom halfeps n
  updatePosition tape mom eps n
  (pe,b,c) <- grad
  k2 <- updateMomentum dtape mom halfeps n
  ke <- letM $ k1 + k2
  pure (ke, pe, b, c)

openSampleFiles ::
  (Applicative m, CmdIO m e, CmdString m e, FTraversable f) =>
  f (FieldName e) ->
  m (f (Const (e (Ptr CFile))))
openSampleFiles = ftraverse $ \x -> withString (fieldName x <> ".csv") $ \fp ->
  withString "w" (fmap Const . fopen fp)

writeSampleHeaders ::
  ( FZip t,
    FFoldable t,
    CmdCond m e,
    CmdFormat m e Int64,
    CmdPutString m e,
    CmdRef m e,
    CmdRange m e,
    CmdWhile m e,
    JanusTyped e Bool,
    JanusTyped e Int64,
    ExpOrd e Int64,
    Num (e Int64),
    ExpBool e
  ) =>
  t (Const (e (Ptr CFile))) ->
  t (FieldName e) ->
  m ()
writeSampleHeaders fps names = ftraverse_ writeSampleHeaders' (fzipWith Pair names fps)
  where
    writeSampleHeaders' ::
      forall m e b.
      (JanusTyped e Bool, JanusTyped e Int64, ExpOrd e Int64, CmdCond m e, CmdFormat m e Int64, CmdPutString m e, CmdRef m e, CmdWhile m e, Num (e Int64), ExpBool e, CmdRange m e) =>
      Product (FieldName e) (Const (e (Ptr CFile))) b ->
      m ()
    writeSampleHeaders' (Pair (FieldVar str) (Const fp)) = withString (str <> "\n") $ hputString fp
    writeSampleHeaders' (Pair (FieldTensor str sh) (Const fp)) = do
      delim <- newRef @m @e true
      withString str $ \str' -> withString "," $ \comma -> withString "_" $ \underscore ->
        withString "\n" $ \newline -> do
          iterateTensorBounds @m @e sh $ \idx -> do
            delim' <- readRef delim
            let writeIdx :: TensorIndex sh e -> m ()
                writeIdx Z = pure ()
                writeIdx (i :. ixs) = do
                  hputString fp underscore
                  hformat fp i ""
                  writeIdx ixs
            ifThenElseM_ delim' (hputString fp str' >> writeIdx idx >> writeRef delim false) $ do
              hputString fp comma
              hputString fp str'
              writeIdx idx
          hputString fp newline

writeSamples ::
  ( FFoldable t,
    FZip t,
    Monad m,
    CmdFormat m e a,
    CmdStorable m e a,
    CmdCond m e,
    CmdPutString m e,
    CmdRange m e,
    CmdRef m e,
    ExpOrd e Int64,
    Num (e Int64),
    ExpBool e,
    JanusTyped e Int64,
    JanusTyped e Bool
  ) =>
  t (Const (e (Ptr CFile))) ->
  t (Primal e a) ->
  m ()
writeSamples fps samples = ftraverse_ writeSamples' (fzipWith Pair fps samples)
  where
    writeSamples' ::
      forall m e a b.
      ( Monad m,
        CmdFormat m e a,
        CmdStorable m e a,
        CmdCond m e,
        CmdPutString m e,
        CmdRange m e,
        CmdRef m e,
        ExpOrd e Int64,
        Num (e Int64),
        ExpBool e,
        JanusTyped e Int64,
        JanusTyped e Bool
      ) =>
      Product (Const (e (Ptr CFile))) (Primal e a) b ->
      m ()
    writeSamples' (Pair (Const fp) (PrimalC v)) = hformat fp v "\n"
    writeSamples' (Pair (Const fp) (PrimalV v)) = peek v >>= \v' -> hformat fp v' "\n"
    writeSamples' (Pair (Const fp) (PrimalT t)) = do
      let Tensor bds _ = t
      r <- newRef @m @e true
      iterateTensorBounds bds $ \idx -> do
        a <- readTensor t idx
        r' <- readRef r
        ifThenElseM_ r' (hformat fp a "" >> writeRef r false) (withString "," (hputString fp) >> hformat fp a "")
      withString "\n" $ hputString fp

closeSampleFiles :: (Applicative m, CmdIO m e, FFoldable f) => f (Const (e (Ptr CFile))) -> m ()
closeSampleFiles = ftraverse_ (\(Const fp) -> fclose fp)

ptrProxy :: e (Ptr a) -> Proxy a
ptrProxy _ = Proxy

data MCLMCState' e a = HMCState'
  { _hmcPos, _hmcMom :: e (Ptr a),
    _hmcDim :: Int
  }

class (Fractional (e a), Monad m) => CmdRand m e a where
  randf :: m (e a)

foreign import ccall "rand" c_rand :: IO CInt

instance (Fractional a) => CmdRand IO Identity a where
  randf = do
    r <- c_rand
    pure $ fromIntegral r / fromIntegral (maxBound :: CInt)

instance (ExpFloatingCast JanusC CInt a, Fractional (JanusC a)) => CmdRand JanusCM JanusC a where
  randf = do
    let crand :: JanusCM (JanusC CInt)
        crand = janusCFFICall (Just "stdlib.h") "rand"
    r <- crand
    pure $ toFloating r / (fromIntegral (maxBound :: CInt) :: JanusC a)

randn :: forall m e a. (JanusTyped e a, CmdRef m e, CmdRand m e a, Floating (e a)) => m (e a)
randn = do
  u1 <- randf
  u2 <- randf
  letM @_ @e $ sqrt (- (2 * log u1)) * cos (2 * pi * u2)

virial ::
  ( JanusTyped e a,
    JanusTyped e Int64,
    CmdRef m e,
    CmdRange m e,
    ExpOrd e Int64,
    CmdStorable m e a,
    Fractional (e a),
    Num (e Int64),
    ExpFloatingCast e Int64 a
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
  letM $ 1 - (t1' - t2' * t3') / toFloating (n - 1)

evalMCLMC ::
  (FFunctor (g e a), Num (e a)) =>
  MCLMCModel m e f g a ->
  g e a (Primal e a) ->
  RAD m e a (f e a (Dual e a)) (Joint (f e a) (g e a) (Dual e a), Dual e a (Var e a))
evalMCLMC (MCLMC hmc) obs = proc x -> do
  (s, hmc') <- hmc -< ((), MCLMCState (Joint x (dconst obs)) Nothing)
  case hmc' ^. hmcLP of
    Just l -> returnA -< (s, l)
    Nothing -> returnA -< (s, auto 0)

applyBounce :: (JanusTyped e a, CmdRange m e, CmdStorable m e a, CmdRef m e, Floating (e a)) => e a -> e (Ptr a) -> Int -> m (e a) -> m ()
applyBounce nu mom n randn' = do
  nm <- newRef 0
  rangeM 0 (fromIntegral n) $ \i -> do
    ui <- peekElemOff mom i
    r <- randn'
    ui' <- letM $ ui + nu * r
    pokeElemOff mom ui' i
    modifyRef nm (+ (ui' * ui'))
  nm' <- readRef nm >>= letM . sqrt
  rangeM 0 (fromIntegral n) $ \i -> do
    peekElemOff mom i >>= \ui -> pokeElemOff mom (ui / nm') i

fillNormal :: (JanusTyped e a, CmdRef m e, CmdRand m e a, CmdStorable m e a, CmdRange m e, Monad m, Floating (e a)) => e (Ptr a) -> e Int64 -> m ()
fillNormal p n = rangeM 0 n $ \i ->
  randn >>= flip (pokeElemOff p) i

fillSphere ::
  ( JanusTyped e a,
    CmdRange m e,
    CmdRef m e,
    CmdWhile m e,
    ExpOrd e Int64,
    CmdRand m e a,
    Floating (e a),
    CmdFormat m e a,
    CmdStorable m e a,
    Num (e Int64)
  ) =>
  e (Ptr a) ->
  e Int64 ->
  m ()
fillSphere mom n = do
  nm <- newRef 0
  rangeM 0 n $ \_ -> do
    ui <- randn
    modifyRef nm (+ (ui * ui))
  nm' <- readRef nm >>= letM . sqrt
  rangeM 0 n $ \i -> peekElemOff mom i >>= \ui -> pokeElemOff mom (ui / nm') i

data HUnit e a f = HUnit
  deriving (Generic)

instance FFunctor (HUnit e a) where ffmap = ffmapDefault

instance FFoldable (HUnit e a) where ffoldMap = ffoldMapDefault

instance FTraversable (HUnit e a) where ftraverse = gftraverse

instance FZip (HUnit e a) where fzipWith = gfzipWith

instance FieldNames e (HUnit e a) where fieldNames = HUnit

tune ::
  forall m e a f g.
  ( FTraversable (f e a),
    FZip (f e a),
    Tangential f e a,
    FFunctor (g e a),
    FieldNames e (f e a),
    CmdRAD m e a,
    CmdRand m e a,
    ExpPtrCast e,
    ExpFloatingCast e Int64 a,
    Floating (e a),
    CmdCond m e,
    CmdPutString m e,
    CmdRef m e,
    CmdFormat m e Int64,
    CmdFormat m e a,
    JanusTyped e Bool,
    JanusTyped e (Ptr a)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  MCLMCModel m e f g a ->
  g e a (Primal e a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  e a ->
  e Int64 ->
  m ()
tune tape dtape hmc obs mom vir eps samples = do
  let n = flatSize $ Proxy @(f e a (Primal e a))
  withGradTape @_ @m @e @_ @a tape dtape (evalMCLMC hmc obs) $ \_ _ grad -> do
    fillSphere mom (fromIntegral n)
    fillNormal tape (fromIntegral n)
    _ <- grad
    rangeM 0 samples $ \i -> do
      _ <- leapfrogStep tape dtape mom n grad eps
      v <- virial tape mom dtape (fromIntegral n)
      pokeElemOff vir v i

pack :: (FFoldable (f e a), FZip (f e a), Monad m, JanusTyped e a, CmdStorable m e a) => f e a (Primal e a) -> f e a (Primal e a) -> m ()
pack ffrom fto = ftraverse_ pack' (fzipWith Pair ffrom fto)
  where
    pack' :: (Monad m, JanusTyped e b, CmdStorable m e b) => Product (Primal e b) (Primal e b) c -> m ()
    pack' (Pair (PrimalC vfrom) (PrimalV vto)) = poke vto vfrom
    pack' (Pair (PrimalV vfrom) (PrimalV vto)) = peek vfrom >>= poke vto
    pack' (Pair (PrimalT _) (PrimalT _)) = error "Brazen.pack: not implemented yet"
    pack' (Pair _ (PrimalC _)) = error "Brazen.pack: the impossible happened"

-- assumes warm start from tune being called previously so tape and dtape have reasonable values
sample ::
  forall m e a f g h.
  ( FTraversable (f e a),
    FTraversable (h e a),
    FZip (f e a),
    FZip (h e a),
    Tangential f e a,
    FFunctor (g e a),
    FieldNames e (f e a),
    FieldNames e (h e a),
    CmdRAD m e a,
    CmdRand m e a,
    ExpFloatingCast e Int64 a,
    ExpOrd e a,
    ExpPtrCast e,
    Floating (e a),
    CmdCond m e,
    CmdPutString m e,
    CmdRef m e,
    CmdFormat m e Int64,
    CmdFormat m e a,
    JanusTyped e Bool,
    JanusTyped e (Ptr a)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  MCLMCModel m e f g a ->
  (Joint (f e a) (g e a) (Primal e a) -> m (h e a (Primal e a))) ->
  g e a (Primal e a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  e a ->
  e Int64 ->
  e Int64 ->
  m ()
sample tape dtape hmc gen obs oldInternalPos oldUserPos mom eps trajLen samples = do
  let n = flatSize $ Proxy @(f e a (Primal e a))
      fn = fieldNames @e @(f e a)
      hn = fieldNames @e @(h e a)
  nu <- letM $ 1 / sqrt (toFloating $ fromIntegral n * trajLen)
  fps <- openSampleFiles fn
  hps <- openSampleFiles hn
  writeSampleHeaders fps fn
  writeSampleHeaders hps hn
  fv <- withString "virial.csv" $ \file ->
    withString "w" $ \mode -> do
      fv' <- fopen file mode
      withString "virial\n" $ \header -> hputString fv' header
      pure fv'
  ev <- withString "energy.csv" $ \file ->
    withString "w" $ \mode -> do
      fv' <- fopen file mode
      withString "energy\n" $ \header -> hputString fv' header
      pure fv'
  withGradTape @_ @m @e @_ @a tape dtape (evalMCLMC hmc obs) $ \_ _ grad -> do
    rangeM (0 :: e Int64) samples $ \_ -> do
      fillSphere mom (fromIntegral n)
      (pe0, _, x0) <- grad
      let x0' = primalize (x0 ^. parameters)
      delta <- newRef 0
      pe <- newRef pe0
      rangeM 0 (fromIntegral n) $ \i -> do
        xi <- peekElemOff tape i
        pokeElemOff oldInternalPos xi i
      pack x0' (unpack oldUserPos)
      rangeM 0 trajLen $ \step -> do
        applyBounce nu mom n randn
        pe' <- readRef pe
        (dke'',pe'', _, x) <- leapfrogStep tape dtape mom n grad eps
        modifyRef delta $ \delta' -> delta' - dke'' + (pe'' - pe')
        writeRef pe pe''
        applyBounce nu mom n randn
        whenM_ (step `eq` trajLen - 1) $ do
          u <- randf @_ @e @a >>= letM . log
          delta'' <- readRef delta
          let x' = primalize $ x ^. parameters
          whenM_ (u `gt` delta'') $ do
            pack (unpack oldUserPos) x'
            rangeM 0 (fromIntegral n) $ \i ->
              peekElemOff oldInternalPos i >>= flip (pokeElemOff tape) i
          writeSamples fps x'
          let primal (Joint f g) = Joint (primalize f) (primalize g)
          gen (primal x) >>= writeSamples hps
          v <- virial tape mom dtape (fromIntegral n)
          hformat fv v "\n"
          readRef delta >>= \e -> hformat ev e "\n"
  closeSampleFiles fps
  closeSampleFiles hps
  _ <- fclose ev
  _ <- fclose fv
  pure ()

data TwoSamplePrior e a f = TwoSamplePrior {_mu1, _mu2, _sigma21, _sigma22 :: f (Var e a)}
  deriving (Generic)

$(makeLenses ''TwoSamplePrior)

instance FFunctor (TwoSamplePrior e a) where ffmap = ffmapDefault

instance FFoldable (TwoSamplePrior e a) where ffoldMap = ffoldMapDefault

instance FTraversable (TwoSamplePrior e a) where ftraverse = gftraverse

instance FZip (TwoSamplePrior e a) where fzipWith = gfzipWith

instance Flat (TwoSamplePrior e a (Primal e a))

instance (Num (e Int64), ExpSized e a, ExpPtr e a) => Tangential TwoSamplePrior e a

instance FieldNames e (TwoSamplePrior e a)

data TwoSampleLikelihood n m e a f = TwoSampleLikelihood {_obsGroup1 :: f (Vector n e a), _obsGroup2 :: f (Vector m e a)}
  deriving (Generic)

$(makeLenses ''TwoSampleLikelihood)

instance FFunctor (TwoSampleLikelihood n m e a) where ffmap = ffmapDefault

instance FFoldable (TwoSampleLikelihood n m e a) where ffoldMap = ffoldMapDefault

instance FTraversable (TwoSampleLikelihood n m e a) where ftraverse = gftraverse

data TwoSampleGen e a f = TwoSampleGen {_sigma1 :: f (Var e a), _sigma2 :: f (Var e a), _twoSampleMeanDiff :: f (Var e a), _twoSampleEffectSize :: f (Var e a)}
  deriving (Generic)

$(makeLenses ''TwoSampleGen)

instance FFunctor (TwoSampleGen e a) where ffmap = ffmapDefault

instance FFoldable (TwoSampleGen e a) where ffoldMap = ffoldMapDefault

instance FTraversable (TwoSampleGen e a) where ftraverse = gftraverse

instance FZip (TwoSampleGen e a) where fzipWith = gfzipWith

instance Flat (TwoSampleGen e a (Primal e a))

instance FieldNames e (TwoSampleGen e a)

twoSampleModel ::
  (CmdRAD m e a, ExpInject e a, Eq a, Floating (ADExp e a), Floating a, KnownNat n1, KnownNat n2) =>
  MCLMCModel m e TwoSamplePrior (TwoSampleLikelihood n1 n2) a
twoSampleModel = proc _ -> do
  s1 <- halfCauchy (parameters . sigma21) -< auto 10
  s2 <- halfCauchy (parameters . sigma22) -< auto 10
  m1 <- normal' (parameters . mu1) -< (auto 20, auto 10)
  m2 <- normal' (parameters . mu2) -< (auto 20, auto 10)
  x1 <- iidNormal (observations . obsGroup1) -< (m1, s1)
  x2 <- iidNormal (observations . obsGroup2) -< (m2, s2)
  returnA -< Joint (TwoSamplePrior m1 m2 s1 s2) (TwoSampleLikelihood x1 x2)

twoSampleGen ::
  (CmdRAD m e a, Floating (e a)) =>
  Joint (TwoSamplePrior e a) (TwoSampleLikelihood n1 n2 e a) (Primal e a) -> m (TwoSampleGen e a (Primal e a))
twoSampleGen x = do
  let PrimalV m1 = x ^. parameters . mu1
      PrimalV m2 = x ^. parameters . mu2
      PrimalV s21 = x ^. parameters . sigma21
      PrimalV s22 = x ^. parameters . sigma22
  m1' <- peek m1
  m2' <- peek m2
  s21' <- peek s21
  s1 <- letM $ sqrt s21'
  s22' <- peek s22
  s2 <- letM $ sqrt s22'
  meanDiff <- letM $ m1' - m2'
  effSize <- letM $ meanDiff / sqrt (0.5 * (s21' + s22'))
  pure $ TwoSampleGen { _sigma1 = PrimalC s1, _sigma2 = PrimalC s2, _twoSampleMeanDiff = PrimalC meanDiff, _twoSampleEffectSize = PrimalC effSize }

specializeModel :: Proxy a -> MCLMCModel m e f g a -> MCLMCModel m e f g a
specializeModel _ x = x

data MCLMCOptions a = MCLMCOptions { _mclmcEps :: a, _mclmcTuneSamples :: Int64, _mclmcSamples :: Int64 }
  deriving (Eq, Ord, Show)

$(makeLenses ''MCLMCOptions)

runTwoSampleModel ::
  forall a.
  ( VM.Storable a,
    Eq a,
    Floating a,
    Real a,
    JanusLitC a,
    JanusCTyped a,
    FFIArg a,
    ExpFloatingCast JanusC CInt a,
    ExpFloatingCast JanusC Int64 a,
    ExpOrd JanusC a,
    Floating (JanusC a),
    CmdFormat JanusCM JanusC a
  ) =>
  MCLMCOptions a ->
  VS.Vector a ->
  VS.Vector a ->
  IO ()
runTwoSampleModel opts x y = case someNatVal (toInteger $ VS.length x) of
  Just (SomeNat px) -> case someNatVal (toInteger $ VS.length y) of
    Just (SomeNat py) -> VS.unsafeWith x $ \x' -> VS.unsafeWith y $ \y' ->
      let mkPTensor p ptr = PrimalT $ Tensor (TensorBoundCons p TensorBoundNil) ptr
          n = flatSize $ Proxy @(TwoSamplePrior Identity a (Primal Identity a))
          floatsize = fromIntegral (runIdentity $ sizeOf (Proxy @a))
          twoSampleModel' = specializeModel (Proxy @a) twoSampleModel
          tapesize = execState (runAD $ getHMC twoSampleModel') n
       in allocaBytes (tapesize * floatsize) $ \tape -> allocaBytes (tapesize * floatsize) $ \dtape ->
        allocaBytes (n * floatsize) $ \newPos -> allocaBytes (n * floatsize) $ \newMom -> allocaBytes (n * floatsize) $ \mom -> do
            virvec <- VM.generate (fromIntegral $ opts ^. mclmcTuneSamples) (const 1)
            VM.unsafeWith virvec $ \vir ->
              withJanusC (\tape' dtape' x'' y'' -> tune @JanusCM @JanusC tape' dtape' twoSampleModel' $ TwoSampleLikelihood (mkPTensor px x'') (mkPTensor py y'')) $ \k ->
                k tape dtape x' y' mom vir (opts ^. mclmcEps) (opts ^. mclmcTuneSamples)
            virvec' <- VS.unsafeFreeze virvec
            case tuneNoiseLengthScale (VS.map realToFrac virvec') of
              Nothing -> error "Insufficient trajectory length for virial to decorrelate!"
              Just len -> withJanusC (\tape' dtape' x'' y'' -> sample @JanusCM @JanusC tape' dtape' twoSampleModel' twoSampleGen (TwoSampleLikelihood (mkPTensor px x'') (mkPTensor py y''))) $ \k ->
                print len >> k tape dtape x' y' newPos newMom mom (opts ^. mclmcEps) (fromIntegral len) (opts ^. mclmcSamples)
    Nothing -> error "VS.length returned a negative value!"
  Nothing -> error "VS.length returned a negative value!"

men, women :: VS.Vector Double
men = VS.fromList [13.3, 6.0, 20.0, 8.0, 14.0, 19.0, 18.0, 25.0, 16.0, 24.0, 15.0, 1.0, 15.0]
women = VS.fromList [22.0, 16.0, 21.7, 21.0, 30.0, 26.0, 12.0, 23.2, 28.0, 23.0]
-- men = VS.fromList [-1,1]
-- women = VS.fromList [-1,1]

data PCGState m e = PCGState
  { pcgState :: Ref m e Word64,
    pcgInc :: e Word64
  }
  deriving (Generic)

pcgNext ::
  ( CmdRef m e,
    JanusTyped e Word32,
    JanusTyped e Word64,
    Num (e Word64),
    Num (e Int),
    ExpBits e Word64,
    ExpBits e Word32,
    ExpIntegralCast e Word64 Word32,
    ExpIntegralCast e Word64 Int
  ) =>
  PCGState m e ->
  m (e Word32)
pcgNext (PCGState state inc) = do
  oldstate <- readRef state
  writeRef state (oldstate * 6364136223846793005 + (inc `ior` 1))
  xorshifted <- letM . toIntegral $ ((oldstate `rshift` 18) `ieor` oldstate) `rshift` 27
  rot <- letM $ oldstate `rshift` 59
  letM $ (xorshifted `rshift` toIntegral rot) `ior` (xorshifted `lshift` toIntegral (negate rot `iand` 31))
