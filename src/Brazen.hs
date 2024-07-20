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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StarIsType #-}

module Brazen where

import Brazen.AD
import Brazen.Distributions
import Brazen.Tune
import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad
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
import Janus.Command.Array
import Janus.Command.Cond
import Janus.Command.Format
import Janus.Command.IO
import Janus.Command.Ref
import Janus.Command.Range
import Janus.Command.While
import Janus.Expression.Bits
import Janus.Expression.Bool
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Ord
-- import Janus.FFI.Ret
import Prelude hiding (id, (.))
import Janus.Backend.C
import Janus.Typed

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

data Foo e a f = Foo {_fooA, _fooB :: f (Var e a)}
  deriving (Generic)

$(makeLenses ''Foo)

deriving instance (Show (f (Var e a))) => Show (Foo e a f)

instance FFunctor (Foo e a) where ffmap = ffmapDefault

instance FFoldable (Foo e a) where ffoldMap = ffoldMapDefault

instance FTraversable (Foo e a) where ftraverse = gftraverse

instance FZip (Foo e a) where fzipWith = gfzipWith

instance FRepeat (Foo e a) where frepeat = gfrepeat

instance Flat (Foo e a (Primal e a))

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

instance (ExpSized e a, ExpPtr e a) => Tangential Foo e a where
  unpack tape = Foo (PrimalV tape) (PrimalV $ tape `ptrAdd` sizeOf (ptrProxy tape))

data MCLMCState' e a = HMCState'
  { _hmcPos, _hmcMom :: e (Ptr a),
    _hmcDim :: Int
  }

class (Fractional (e a), Monad m) => CmdRand m e a where
  randf :: m (e a)

foreign import ccall "rand" c_rand :: IO CInt

instance Fractional a => CmdRand IO Identity a where
  randf = do
    r <- c_rand
    pure $ fromIntegral r / fromIntegral (maxBound :: CInt)

instance (ExpFloatingCast JanusC CInt a, Fractional (JanusC a)) => CmdRand JanusCM JanusC a where
  randf = do
    let crand :: JanusCM (JanusC CInt)
        crand = janusCFFICall (Just "stdint.h") "rand"
    r <- crand
    pure $ toFloating r / (fromIntegral (maxBound :: CInt) :: JanusC a)

randn :: forall m e a. (JanusTyped e a, CmdRef m e, CmdRand m e a, Floating (e a)) => m (e a)
randn = do
  u1 <- randf
  u2 <- randf
  letM @_ @e $ sqrt (-2 * log u1) * cos (2 * pi * u2)

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

data FieldName e a where
  FieldVar :: String -> FieldName e (Var e a)
  FieldTensor :: String -> TensorBound sh e -> FieldName e (Tensor sh e a)

deriving instance Show (FieldName e a)

fieldName :: FieldName e a -> String
fieldName (FieldVar s) = s
fieldName (FieldTensor s _) = s

class GFieldNames f where
  gfieldNames :: String -> Proxy (f a) -> f a

instance (GFieldNames f, GFieldNames g) => GFieldNames (f :*: g) where
  gfieldNames prefix _ = gfieldNames prefix Proxy :*: gfieldNames prefix Proxy

instance (FFunctor f, FieldNames e f) => GFieldNames (K1 i (f (FieldName e))) where
  gfieldNames prefix _ =
    K1 $
      ffmap
        ( \case
            FieldVar s -> FieldVar (prepend s)
            FieldTensor s t -> FieldTensor (prepend s) t
        )
        (fieldNames :: f (FieldName e))
    where
      prepend s = prefix <> "_" <> s

instance GFieldNames (K1 i (FieldName e (Var e a))) where
  gfieldNames prefix _ = K1 $ FieldVar prefix

instance (ReifyTensorBound sh) => GFieldNames (K1 i (FieldName e (Tensor sh e a))) where
  gfieldNames prefix _ = K1 $ FieldTensor prefix (tensorBound (Proxy @sh))

instance (GFieldNames f) => GFieldNames (D1 i f) where
  gfieldNames prefix _ = M1 $ gfieldNames prefix Proxy

instance (GFieldNames f) => GFieldNames (C1 i f) where
  gfieldNames prefix _ = M1 $ gfieldNames prefix Proxy

instance (KnownSymbol n, GFieldNames f) => GFieldNames (S1 ('MetaSel ('Just n) u s d) f) where
  gfieldNames prefix p = M1 $ gfieldNames (prefix <> fname) (p' p)
    where
      p' :: Proxy (S1 i f a) -> Proxy (f a)
      p' _ = Proxy
      fname = case symbolVal (Proxy @n) of
        '_' : xs -> xs
        xs -> xs

class FieldNames e f where
  fieldNames :: f (FieldName e)
  default fieldNames :: (Generic (f (FieldName e)), GFieldNames (Rep (f (FieldName e)))) => f (FieldName e)
  fieldNames = GHC.Generics.to $ gfieldNames "" (Proxy @(Rep (f (FieldName e)) ()))

instance FieldNames e (Foo e a)

data Barf e a f = Barf {_barf1, _barf2 :: Foo e a f}
  deriving (Generic)

deriving instance (Show (f (Var e a))) => Show (Barf e a f)

instance FieldNames e (Barf e a)

evalMCLMC ::
  (FFunctor (g e a), Num (e a)) =>
  MCLMCModel m e f g a ->
  g e a (Primal e a) ->
  RAD m e a (f e a (Dual e a)) (Joint (f e a) (g e a) (Dual e a), Dual e a (Var e a))
evalMCLMC (MCLMC hmc) obs = proc x -> do
  (s, hmc') <- hmc -< ((), MCLMCState (Joint x (dconst obs)) Nothing)
  case hmc' ^. hmcLP of
    Just l -> returnA -< (s,l)
    Nothing -> returnA -< (s,auto 0)

applyBounce :: (JanusTyped e a, CmdRange m e, CmdStorable m e a, CmdRef m e, Floating (e a)) => e a -> e (Ptr a) -> Int -> m (e a) -> m ()
applyBounce nu mom n randn' = do
  nm <- newRef 0
  rangeM 0 (fromIntegral n) $ \i -> do
    ui <- peekElemOff mom i
    nm' <- readRef nm
    r <- randn'
    ui' <- letM $ ui + nu * r
    pokeElemOff mom ui' i
    writeRef nm (nm' + ui' * ui)
  nm' <- readRef nm >>= letM . sqrt
  rangeM 0 (fromIntegral n) $ \i -> do
    peekElemOff mom i >>= \ui -> pokeElemOff mom (ui / nm') i

fillNormal :: (JanusTyped e a, CmdRef m e, CmdRand m e a, CmdStorable m e a, CmdRange m e, Monad m, Floating (e a)) => e (Ptr a) -> e Int64 -> m ()
fillNormal p n = rangeM 0 n $ \i ->
  randn >>= flip (pokeElemOff p) i

fillSphere
  :: (JanusTyped e a, CmdRange m e, CmdRef m e, CmdWhile m e,
      ExpOrd e Int64, CmdRand m e a, Floating (e a),
      CmdFormat m e a, CmdStorable m e a, Num (e Int64)) =>
     e (Ptr a) -> e Int64 -> m ()
fillSphere mom n = do
  nm <- newRef 0
  rangeM 0 n $ \_ -> do
    ui <- randn
    modifyRef nm (+ (ui*ui))
  nm' <- readRef nm >>= letM . sqrt
  rangeM 0 n $ \i -> peekElemOff mom i >>= \ui -> pokeElemOff mom (ui/nm') i

data Prior e a f = Prior
  deriving (Generic)

instance FFunctor (Prior e a) where ffmap = ffmapDefault

instance FFoldable (Prior e a) where ffoldMap = ffoldMapDefault

instance FTraversable (Prior e a) where ftraverse = gftraverse

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
  MCLMCModel m e f g a ->
  g e a (Primal e a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  m ()
tune hmc obs mom vir = do
  let eps = 1e-2
      n = flatSize $ Proxy @(f e a (Primal e a))
  withGrad @_ @m @e @_ @a (evalMCLMC hmc obs) $ \tape _ grad -> do
    let Tape tape' dtape' = tape
    fillSphere mom (fromIntegral n)
    fillNormal tape' (fromIntegral n)
    _ <- grad
    rangeM 0 2048 $ \i -> do
      (_, _, _) <- minimalNormStep tape' dtape' mom n grad eps
      v <- virial tape' mom dtape' (fromIntegral n)
      pokeElemOff vir v i

sample ::
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
  MCLMCModel m e f g a ->
  g e a (Primal e a) ->
  e (Ptr a) ->
  e a ->
  m ()
sample hmc obs mom nu = do
  let eps = 1e-2
      n = flatSize $ Proxy @(f e a (Primal e a))
      fn = fieldNames @e @(f e a)
  fps <- openSampleFiles fn
  writeSampleHeaders fps fn
  fv <- withString "virial.csv" $ \file ->
    withString "w" $ \mode -> do
      fv' <- fopen file mode
      withString "virial\n" $ \header -> hputString fv' header
      pure fv'
  ivirial <- newRef 0
  trajLen <- newRef (0 :: e Int64)
  fillSphere mom (fromIntegral n)
  withGrad @_ @m @e @_ @a (evalMCLMC hmc obs) $ \tape _ grad -> do
    let Tape tape' dtape' = tape
    fillSphere tape' (fromIntegral n)
    fillNormal tape' (fromIntegral n)
    _ <- grad
    rangeM (0 :: e Int64) 1000000 $ \_ -> do
      (_, _, x) <- minimalNormStep tape' dtape' mom n grad eps
      applyBounce nu mom n randn
      writeSamples fps (primalize $ x ^. parameters)
      v <- virial tape' mom dtape' (fromIntegral n)
      modifyRef ivirial (+ v)
      iv <- readRef ivirial
      trajLen' <- readRef trajLen
      writeRef trajLen (trajLen' + 1)
      hformat fv iv "\n"
      writeRef ivirial 0
      writeRef trajLen 0
  closeSampleFiles fps
  _ <- fclose fv
  pure ()

simpleModel :: (CmdRAD m e a, ExpInject e a, Floating (e a), Eq a, Floating a) => MCLMCModel m e Foo Prior a
simpleModel = proc _ -> do
  a <- normal (parameters . fooA) -< (auto 0, auto 1)
  b <- normal (parameters . fooB) -< (auto 0, auto 1)
  returnA -< Joint (Foo a b) Prior

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

twoSampleModel ::
  (CmdRAD m e a, ExpInject e a, Eq a, Floating (e a), Floating a, KnownNat n1, KnownNat n2) =>
  MCLMCModel m e TwoSamplePrior (TwoSampleLikelihood n1 n2) a
twoSampleModel = proc _ -> do
  s1 <- halfCauchy (parameters . sigma21) -< auto 10
  s2 <- halfCauchy (parameters . sigma22) -< auto 10
  m1 <- normal' (parameters . mu1) -< (auto 20, auto 10)
  m2 <- normal' (parameters . mu2) -< (auto 20, auto 10)
  x1 <- iidNormal (observations . obsGroup1) -< (m1, s1)
  x2 <- iidNormal (observations . obsGroup2) -< (m2, s2)
  returnA -< Joint (TwoSamplePrior m1 m2 s1 s2) (TwoSampleLikelihood x1 x2)

runTwoSampleModel :: VS.Vector Float -> VS.Vector Float -> IO ()
runTwoSampleModel x y = case someNatVal (toInteger $ VS.length x) of
  Just (SomeNat px) -> case someNatVal (toInteger $ VS.length y) of
    Just (SomeNat py) -> VS.unsafeWith x $ \x' -> VS.unsafeWith y $ \y' ->
      let mkPTensor p ptr = PrimalT $ Tensor (TensorBoundCons p TensorBoundNil) ptr
          n = flatSize $ Proxy @(TwoSamplePrior Identity Double (Primal Identity Double))
          floatsz = fromIntegral (runIdentity $ sizeOf (Proxy @Float))
        in allocaBytes (n*floatsz) $ \mom -> do
          virvec <- VM.generate 2048 (const 0)
          VM.unsafeWith virvec $ \vir ->
            withJanusC (\x'' y'' -> tune @JanusCM @JanusC twoSampleModel $ TwoSampleLikelihood (mkPTensor px x'') (mkPTensor py y'')) $ \k ->
              k x' y' mom vir
          virvec' <- VS.unsafeFreeze virvec
          tuneNoiseLengthScale virvec' >>= \case
            Nothing -> error "Insufficient trajectory length for virial to decorrelate!"
            Just len -> withJanusC (\x'' y'' -> sample @JanusCM @JanusC twoSampleModel $ TwoSampleLikelihood (mkPTensor px x'') (mkPTensor py y'')) $ \k ->
              let nu = 1 / sqrt (fromIntegral $ n * len)
              in k x' y' mom nu
    Nothing -> error "VS.length returned a negative value!"
  Nothing -> error "VS.length returned a negative value!"

men, women :: VS.Vector Float
men = VS.fromList [13.3,6.0,20.0,8.0,14.0,19.0,18.0,25.0,16.0,24.0,15.0,1.0,15.0]
women = VS.fromList [22.0,16.0,21.7,21.0,30.0,26.0,12.0,23.2,28.0,23.0]

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
