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
{-# LANGUAGE StarIsType #-} {-# LANGUAGE TemplateHaskell #-}
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
import Data.Monoid (Sum (..))
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
import Janus.Backend.C.Build
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
import System.Environment
import Prelude hiding (id, (.))

data MCLMCOptions a = MCLMCOptions {_mclmcEps :: a, _mclmcTuneSamples :: Int64, _mclmcSamples :: Int64, _mclmcThin :: Int64}
  deriving (Eq, Ord, Show)

$(makeLenses ''MCLMCOptions)

specializeModel :: Proxy a -> MCLMCModel m e f g a -> MCLMCModel m e f g a
specializeModel _ x = x

peekOrPure :: (Applicative m, CmdStorable m e a) => Primal e a (Var e a) -> m (e a)
peekOrPure (PrimalC a) = pure a
peekOrPure (PrimalV v) = peek v

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
