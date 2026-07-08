{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Brazen
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
import Data.Semigroup
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import Data.HKD
import Data.Int
import Data.Proxy
import Foreign.C
import Foreign.Marshal.Alloc
import GHC.Float
import GHC.Generics
import GHC.TypeLits
import Janus.Backend.C
import Janus.Backend.C.Build
import Janus.Command.Array
import Janus.Command.Format
import Janus.Command.Ref
import Janus.Expression.Cast
import Janus.Expression.Inject
import Janus.Expression.Ord
import Janus.FFI.Arg
import Linear.V2
import Prelude hiding ((.),id)
import System.Environment

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
  Just (SomeNat px) -> VS.unsafeWith x $ \x' -> do
    tmpdir <- getEnv "TMPDIR"
    let mkPTensor p ptr = PrimalT $ Tensor (TensorBoundCons p TensorBoundNil) ptr
        n = flatSize $ Proxy @(StochasticVolPrior Identity a (Primal Identity a))
        floatsize = fromIntegral (runIdentity $ sizeOf (Proxy @a))
        stochasticVolModel' = specializeModel (Proxy @a) stochasticVolModel
        tapestate = execState (runAD $ getHMC stochasticVolModel') (RADState (Sum n) 0)
        tapeSize = tapestate ^. radStateTapeSize . _Wrapped
        scratchSize = tapestate ^. radStateScratchSize . _Wrapped
        config = defaultCBuildConfig & cBuildConfigCacheDir .~ tmpdir
     in allocaBytes (tapeSize * floatsize) $ \tape -> allocaBytes (tapeSize * floatsize) $ \dtape -> allocaBytes (scratchSize * floatsize) $ \stape ->
          allocaBytes (n * floatsize) $ \oldInternalPos -> allocaBytes (n * floatsize) $ \oldUserPos -> allocaBytes (n * floatsize) $ \mom -> do
            virvec <- VM.generate (fromIntegral $ opts ^. mclmcTuneSamples) (const 1)
            VM.unsafeWith virvec $ \vir ->
              withJanusC config (\tape' dtape' stape' x'' -> tune @JanusCM @JanusC tape' dtape' stape' stochasticVolModel' $ StochasticVolLikelihood (mkPTensor px x'')) $ \k ->
                k tape dtape stape x' mom vir (opts ^. mclmcEps) (opts ^. mclmcTuneSamples)
            virvec' <- VS.freeze virvec
            case tuneNoiseLengthScale (VS.map realToFrac virvec') of
              Nothing -> error "Insufficient trajectory length for virial to decorrelate!"
              Just len -> withJanusC config (\x'' -> sample @JanusCM @JanusC stochasticVolModel' stochasticVolGen (StochasticVolLikelihood (mkPTensor px x''))) $ \k ->
                c_srand 0xdeadbeef >> print len >> k x' tape dtape stape oldInternalPos oldUserPos mom (opts ^. mclmcEps) (fromIntegral len) (opts ^. mclmcSamples) (opts ^. mclmcThin)
  Nothing -> error "VS.length returned a negative value!"

main :: IO ()
main = do
  [filepath] <- getArgs
  loadSP500 filepath >>= runSP500 (MCLMCOptions 1e-2 8192 10000 5)
