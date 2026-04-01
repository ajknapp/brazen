{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE RecordWildCards #-}
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
import Brazen.Sampler
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
import Foreign.C
import Foreign.Marshal.Alloc
import GHC.Float
import GHC.Generics
import GHC.TypeLits
import Janus.Backend.C
import Janus.Command.Array
import Janus.Command.Format
import Janus.Command.Ref
import Janus.Expression.Bits
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Ord
import Janus.FFI.Arg
import Janus.Typed
import Linear.V2
import Prelude hiding (id, (.))

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

twoSamplePrior ::
  (CmdRAD m e a, ExpInject e a, Eq a, Floating (ADExp e a), Floating a) =>
  Getting (Dual e a (Var e a)) s (TwoSamplePrior e a (Dual e a)) ->
  MCLMC m e s a () (TwoSamplePrior e a (Dual e a))
twoSamplePrior l = proc () -> do
  _mu1 <- normal' (l . mu1) -< (auto 20, auto 10)
  _mu2 <- normal' (l . mu2) -< (auto 20, auto 10)
  _sigma21 <- halfCauchy (l . sigma21) -< auto 10
  _sigma22 <- halfCauchy (l . sigma22) -< auto 10
  returnA -< TwoSamplePrior {..}

type GettingTensor e a s c = forall ns. (c -> Const (Dual e a (Tensor ns e a)) c) -> s -> Const (Dual e a (Tensor ns e a)) s

twoSampleLikelihood ::
  (CmdRAD m e a, ExpInject e a, Eq a, Floating (ADExp e a), Floating a, KnownNat n1, KnownNat n2) =>
  GettingTensor e a s (TwoSampleLikelihood n1 n2 e a (Dual e a)) ->
  MCLMC m e s a (TwoSamplePrior e a (Dual e a)) (TwoSampleLikelihood n1 n2 e a (Dual e a))
twoSampleLikelihood l = proc prior -> do
  _obsGroup1 <- iidNormal (l . obsGroup1) -< (prior ^. mu1, prior ^. sigma21)
  _obsGroup2 <- iidNormal (l . obsGroup2) -< (prior ^. mu2, prior ^. sigma22)
  returnA -< TwoSampleLikelihood {..}

model :: MCLMCPrior m e f g a -> MCLMCLikelihood m e f g a -> MCLMCModel m e f g a
model prior likelihood = prior >>> id &&& likelihood >>> arr (uncurry Joint)
{-# INLINE model #-}

twoSampleModel ::
  (CmdRAD m e a, ExpInject e a, Eq a, Floating (ADExp e a), Floating a, KnownNat n1, KnownNat n2) =>
  MCLMCModel m e TwoSamplePrior (TwoSampleLikelihood n1 n2) a
twoSampleModel = model (twoSamplePrior parameters) (twoSampleLikelihood observations)

twoSampleGen ::
  (CmdRAD m e a, Floating (e a)) =>
  Joint (TwoSamplePrior e a) (TwoSampleLikelihood n1 n2 e a) (Primal e a) ->
  m (TwoSampleGen e a (Primal e a))
twoSampleGen x = do
  m1 <- peekOrPure $ x ^. parameters . mu1
  m2 <- peekOrPure $ x ^. parameters . mu2
  s21 <- peekOrPure $ x ^. parameters . sigma21
  s22 <- peekOrPure $ x ^. parameters . sigma22
  s1 <- letM $ sqrt s21
  s2 <- letM $ sqrt s22
  meanDiff <- letM $ m1 - m2
  effSize <- letM $ meanDiff / sqrt (0.5 * (s21 + s22))
  pure $ TwoSampleGen {_sigma1 = PrimalC s1, _sigma2 = PrimalC s2, _twoSampleMeanDiff = PrimalC meanDiff, _twoSampleEffectSize = PrimalC effSize}

specializeModel :: Proxy a -> MCLMCModel m e f g a -> MCLMCModel m e f g a
specializeModel _ x = x

peekOrPure :: (Applicative m, CmdStorable m e a) => Primal e a (Var e a) -> m (e a)
peekOrPure (PrimalC a) = pure a
peekOrPure (PrimalV v) = peek v

data MCLMCOptions a = MCLMCOptions {_mclmcEps :: a, _mclmcTuneSamples :: Int64, _mclmcSamples :: Int64, _mclmcThin :: Int64}
  deriving (Eq, Ord, Show)

$(makeLenses ''MCLMCOptions)

runTwoSampleModel ::
  forall a.
  ( VM.Storable a,
    Show a,
    Eq a,
    Floating a,
    Real a,
    JanusLitC a,
    JanusCTyped a,
    FFIArg a,
    ExpBoolCast JanusC a,
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
            allocaBytes (n * floatsize) $ \oldInternalPos -> allocaBytes (n * floatsize) $ \oldUserPos -> allocaBytes (n * floatsize) $ \mom -> do
              virvec <- VM.generate (fromIntegral $ opts ^. mclmcTuneSamples) (const 1)
              VM.unsafeWith virvec $ \vir ->
                withJanusC (\tape' dtape' x'' y'' -> tune @JanusCM @JanusC tape' dtape' twoSampleModel' $ TwoSampleLikelihood (mkPTensor px x'') (mkPTensor py y'')) $ \k ->
                  k tape dtape x' y' mom vir (opts ^. mclmcEps) (opts ^. mclmcTuneSamples)
              virvec' <- VS.freeze virvec
              case tuneNoiseLengthScale (VS.map realToFrac virvec') of
                Nothing -> error "Insufficient trajectory length for virial to decorrelate!"
                Just len -> withJanusC (\x'' y'' -> sample @JanusCM @JanusC twoSampleModel' twoSampleGen (TwoSampleLikelihood (mkPTensor px x'') (mkPTensor py y''))) $ \k ->
                  c_srand 0xdeadbeef >> print len >> k x' y' tape dtape oldInternalPos oldUserPos mom (opts ^. mclmcEps) (fromIntegral len) (opts ^. mclmcSamples) (opts ^. mclmcThin)
    Nothing -> error "VS.length returned a negative value!"
  Nothing -> error "VS.length returned a negative value!"

men, women :: VS.Vector Float
men = VS.fromList [13.3, 6.0, 20.0, 8.0, 14.0, 19.0, 18.0, 25.0, 16.0, 24.0, 15.0, 1.0, 15.0]
women = VS.fromList [22.0, 16.0, 21.7, 21.0, 30.0, 26.0, 12.0, 23.2, 28.0, 23.0]

data StochasticVolPrior e a f = StochasticVolPrior
  { _svMu :: f (Var e a),
    _svSigma2 :: f (Var e a)
  }
  deriving (Generic)

$(makeLenses ''StochasticVolPrior)

instance FFunctor (StochasticVolPrior e a) where ffmap = ffmapDefault

instance FFoldable (StochasticVolPrior e a) where ffoldMap = ffoldMapDefault

instance FTraversable (StochasticVolPrior e a) where ftraverse = gftraverse

instance FZip (StochasticVolPrior e a) where fzipWith = gfzipWith

instance Flat (StochasticVolPrior e a (Primal e a))

instance FieldNames e (StochasticVolPrior e a)

instance (Num (e Int64), ExpSized e a, ExpPtr e a) => Tangential StochasticVolPrior e a

data StochasticVolLikelihood n e a f = StochasticVolLikelihood
  { _svLogReturns :: f (Vector n e a)
  }
  deriving (Generic)

$(makeLenses ''StochasticVolLikelihood)

instance FFunctor (StochasticVolLikelihood n e a) where ffmap = ffmapDefault

instance FFoldable (StochasticVolLikelihood n e a) where ffoldMap = ffoldMapDefault

instance FTraversable (StochasticVolLikelihood n e a) where ftraverse = gftraverse

instance FZip (StochasticVolLikelihood n e a) where fzipWith = gfzipWith

instance (KnownNat n) => Flat (StochasticVolLikelihood n e a (Primal e a))

instance (KnownNat n) => FieldNames e (StochasticVolLikelihood n e a)

data StochasticVolGen e a f = StochasticVolGen
  { _svKelly :: f (Var e a)
  }
  deriving (Generic)

$(makeLenses ''StochasticVolGen)

instance FFunctor (StochasticVolGen e a) where ffmap = ffmapDefault

instance FFoldable (StochasticVolGen e a) where ffoldMap = ffoldMapDefault

instance FTraversable (StochasticVolGen e a) where ftraverse = gftraverse

instance FZip (StochasticVolGen e a) where fzipWith = gfzipWith

instance Flat (StochasticVolGen e a (Primal e a))

instance FieldNames e (StochasticVolGen e a)

stochasticVolModel ::
  (CmdRAD m e a, ExpInject e a, Eq a, Floating (ADExp e a), Floating a, Fractional (e a), KnownNat n) =>
  MCLMCModel m e StochasticVolPrior (StochasticVolLikelihood n) a
stochasticVolModel = proc () -> do
  _svMu <- normal' (parameters . svMu) -< (auto 0, auto 1e-4)
  _svSigma2 <- halfCauchy (parameters . svSigma2) -< auto 1e-4
  _svLogReturns <- iid geoBrownianD (observations . svLogReturns) -< V2 (broadcast _svMu) (broadcast _svSigma2)
  returnA -< Joint (StochasticVolPrior {..}) (StochasticVolLikelihood {..})
  where
    geoBrownianD (Pair (V2 mu s2) z) = normalD (Pair (V2 (mu - 0.5 * s2) s2) z)

stochasticVolGen ::
  (CmdRAD m e a, Floating (e a)) =>
  Joint (StochasticVolPrior e a) (StochasticVolLikelihood n e a) (Primal e a) ->
  m (StochasticVolGen e a (Primal e a))
stochasticVolGen x = do
  mu <- peekOrPure $ x ^. parameters . svMu
  s2 <- peekOrPure $ x ^. parameters . svSigma2
  _svKelly <- fmap PrimalC . letM $ mu / s2
  pure $ StochasticVolGen {..}

loadSP500 :: FilePath -> IO (VS.Vector Double)
loadSP500 file = VS.fromList . map (log1p . (* 1e-2)) . map read . lines <$> readFile file

runSP500 ::
  forall a.
  ( VM.Storable a,
    Show a,
    Eq a,
    Floating a,
    Real a,
    JanusLitC a,
    JanusCTyped a,
    FFIArg a,
    ExpBoolCast JanusC a,
    ExpFloatingCast JanusC CInt a,
    ExpFloatingCast JanusC Int64 a,
    ExpOrd JanusC a,
    Floating (JanusC a),
    CmdFormat JanusCM JanusC a
  ) =>
  MCLMCOptions a -> VS.Vector a -> IO ()
runSP500 opts x = case someNatVal (toInteger $ VS.length x) of
  Just (SomeNat px) -> VS.unsafeWith x $ \x' ->
    let mkPTensor p ptr = PrimalT $ Tensor (TensorBoundCons p TensorBoundNil) ptr
        n = flatSize $ Proxy @(StochasticVolPrior Identity a (Primal Identity a))
        floatsize = fromIntegral (runIdentity $ sizeOf (Proxy @a))
        stochasticVolModel' = specializeModel (Proxy @a) stochasticVolModel
        tapesize = execState (runAD $ getHMC stochasticVolModel') n
     in allocaBytes (tapesize * floatsize) $ \tape -> allocaBytes (tapesize * floatsize) $ \dtape ->
          allocaBytes (n * floatsize) $ \oldInternalPos -> allocaBytes (n * floatsize) $ \oldUserPos -> allocaBytes (n * floatsize) $ \mom -> do
            virvec <- VM.generate (fromIntegral $ opts ^. mclmcTuneSamples) (const 1)
            VM.unsafeWith virvec $ \vir ->
              withJanusC (\tape' dtape' x'' -> tune @JanusCM @JanusC tape' dtape' stochasticVolModel' $ StochasticVolLikelihood (mkPTensor px x'')) $ \k ->
                k tape dtape x' mom vir (opts ^. mclmcEps) (opts ^. mclmcTuneSamples)
            virvec' <- VS.freeze virvec
            case tuneNoiseLengthScale (VS.map realToFrac virvec') of
              Nothing -> error "Insufficient trajectory length for virial to decorrelate!"
              Just len -> withJanusC (\x'' -> sample @JanusCM @JanusC stochasticVolModel' stochasticVolGen (StochasticVolLikelihood (mkPTensor px x''))) $ \k ->
                c_srand 0xdeadbeef >> print len >> k x' tape dtape oldInternalPos oldUserPos mom (opts ^. mclmcEps) (fromIntegral len) (opts ^. mclmcSamples) (opts ^. mclmcThin)
  Nothing -> error "VS.length returned a negative value!"

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
pcgNext (PCGState st inc) = do
  oldstate <- readRef st
  writeRef st (oldstate * 6364136223846793005 + (inc `ior` 1))
  xorshifted <- letM . toIntegral $ ((oldstate `rshift` 18) `ieor` oldstate) `rshift` 27
  rot <- letM $ oldstate `rshift` 59
  letM $ (xorshifted `rshift` toIntegral rot) `ior` (xorshifted `lshift` toIntegral (negate rot `iand` 31))
