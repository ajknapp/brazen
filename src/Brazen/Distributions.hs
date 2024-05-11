{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Brazen.Distributions where

import Brazen.AD
import Brazen.Numeric
import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Category
import Control.Lens
import GHC.Generics
import GHC.TypeLits
import Janus.Command.Array
import Linear
import qualified Numeric.AD as AD
import Prelude hiding (id, (.))

data Joint f g a = Joint {_parameters :: f a, _observations :: g a}
  deriving (Generic)

$(makeLenses ''Joint)

data MCLMCState s e a = MCLMCState {_hmcState :: s, _hmcLP :: Maybe (Dual (Var e a))}

$(makeLenses ''MCLMCState)

newtype MCLMC m e s c a b = MCLMC {getHMC :: RAD m e c (a, MCLMCState s e c) (b, MCLMCState s e c)}
  deriving (Category, Arrow, ArrowChoice) via StateArrow (MCLMCState s e c) (RAD m e c)

type MCLMCModel m e f g a = MCLMC m e (Joint (f e a) (g e a) Dual) a () (Joint (f e a) (g e a) Dual)

updateLP :: CmdRAD m e a => RAD m e a (MCLMCState s e a, b, Dual (Var e a)) (b, MCLMCState s e a)
updateLP = proc (hmc, theta, lp) -> do
  case hmc ^. hmcLP of
    Nothing -> do returnA -< (theta, hmc & hmcLP ?~ lp)
    Just lp' -> do
      lp'' <- addA -< (lp, lp')
      returnA -< (theta, hmc & hmcLP ?~ lp'')

normalD :: (Floating (e a)) => V3 (e a) -> e a
normalD (V3 mu sigma2 x) = (x - mu) * (x - mu) / (2 * sigma2) + log (2 * pi * sigma2)

normal :: (CmdRAD m e a, Floating (e a)) => Getting (Dual (Var e a)) s (Dual (Var e a)) -> MCLMC m e s a (Dual (Var e a), Dual (Var e a)) (Dual (Var e a))
normal l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opN (\x -> let (y, dy) = AD.grad' normalD x in (y, (dy ^*))) -< V3 mu sigma2 theta
  updateLP -< (hmc, theta, lp)

iidNormal ::
  (CmdRAD m e a, KnownNat n, Floating (e a)) =>
  Getting (Dual (Vector n e a)) s (Dual (Vector n e a)) ->
  MCLMC m e s a (Dual (Var e a), Dual (Var e a)) (Dual (Vector n e a))
iidNormal l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNMapSum normalD (AD.grad normalD) -< V3 (broadcast mu) (broadcast sigma2) theta
  updateLP -< (hmc, theta, lp)
