{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Brazen.Distributions where

import Brazen.AD
import Brazen.Numeric
import Brazen.Partial
import Brazen.Shared
import Control.Arrow
import Control.Arrow.Transformer.State
import Control.Category
import Control.Lens
import Data.Reflection
import GHC.Float
import GHC.Generics
import GHC.TypeLits
import Janus.Command.Array
import Janus.Expression.Inject
import Linear
import qualified Numeric.AD as AD
import qualified Numeric.AD.Internal.Reverse as AD
import Prelude hiding (id, (.))

data Joint f g a = Joint {_parameters :: f a, _observations :: g a}
  deriving (Generic)

$(makeLenses ''Joint)

data MCLMCState s e a = MCLMCState {_hmcState :: s, _hmcLP :: Maybe (Dual e a (Var e a))}

$(makeLenses ''MCLMCState)

newtype MCLMC m e s c a b = MCLMC {getHMC :: RAD m e c (a, MCLMCState s e c) (b, MCLMCState s e c)}
  deriving (Category, Arrow, ArrowChoice) via StateArrow (MCLMCState s e c) (RAD m e c)

type MCLMCModel m e f g a = MCLMC m e (Joint (f e a) (g e a) (Dual e a)) a () (Joint (f e a) (g e a) (Dual e a))

opNAD ::
  (Traversable f, Applicative f, CmdRAD m e a, ExpInject e a, Num a, Num (ADExp e a), Eq a) =>
  (forall s. (Reifies s AD.Tape) => f (AD.Reverse s (Partial e a)) -> AD.Reverse s (Partial e a)) ->
  RAD m e a (f (Dual e a (Var e a))) (Dual e a (Var e a))
opNAD f = opN (\x -> let (y, dy) = AD.grad' f (fmap Dynamic x) in (runPartial y, \dz -> fmap (runPartial . (* Dynamic dz)) dy))

updateLP ::
  (CmdRAD m e a) =>
  RAD m e a (MCLMCState s e a, b, Dual e a (Var e a)) (b, MCLMCState s e a)
updateLP = proc (hmc, theta, lp) -> do
  case hmc ^. hmcLP of
    Nothing -> do returnA -< (theta, hmc & hmcLP ?~ lp)
    Just lp' -> do
      lp'' <- addA -< (lp, lp')
      returnA -< (theta, hmc & hmcLP ?~ lp'')

normalD :: (Floating (e a)) => V3 (e a) -> e a
normalD (V3 mu sigma2 x) = let y = x - mu in y * y / (2 * sigma2) + 0.5 * log (2 * pi * sigma2)

normal ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
normal l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD normalD -< V3 mu sigma2 theta
  updateLP -< (hmc, theta, lp)

normal' ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
normal' l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD (\(V2 z s2) -> 0.5 * z * z + 0.5 * log (2 * pi / s2)) -< V2 theta sigma2
  z <- opNAD (\(V3 m s t) -> m + t * sqrt s) -< V3 mu sigma2 theta
  updateLP -< (hmc, z, lp)

iidNormal ::
  forall m e a n s.
  (CmdRAD m e a, KnownNat n, ExpInject e a, Eq a, Floating a, Floating (ADExp e a)) =>
  Getting (Dual e a (Vector n e a)) s (Dual e a (Vector n e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Vector n e a))
iidNormal l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNMapSum (runPartial . normalD . fmap (Dynamic @e)) (fmap runPartial . AD.grad normalD . fmap (Dynamic @e)) -< V3 (broadcast mu) (broadcast sigma2) theta
  updateLP -< (hmc, theta, lp)

lognormal ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
lognormal l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD normalD -< V3 mu sigma2 theta
  phi <- expA -< theta
  updateLP -< (hmc, phi, lp)

lognormal' ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
lognormal' l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD normalD -< V3 (auto 0) (auto 1) theta
  phi <- opNAD (\(V3 mu sigma2 t) -> exp $ mu + sqrt sigma2 * t) -< V3 mu sigma2 theta
  updateLP -< (hmc, phi, lp)

halfCauchyD :: (Floating a) => V2 a -> a
halfCauchyD (V2 scale y) = log1pexp (2 * y) + log (0.5 * pi * scale) - y

halfCauchy ::
  (CmdRAD m e a, Floating (ADExp e a), Eq a, Floating a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a)) (Dual e a (Var e a))
halfCauchy l = MCLMC $ proc (scale, hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD halfCauchyD -< V2 scale theta
  (_, hmc') <- updateLP -< (hmc, theta, lp)
  phi <- expA -< theta
  returnA -< (phi, hmc')

uniformT :: (Floating a) => V3 a -> a
uniformT (V3 a' b' v) = let c = b' - a' in negate . log $ c / (pi * (1 + v * v))

uniform ::
  (CmdRAD m e a, Floating (ADExp e a), Eq a, Floating a, ExpInject e a) =>
  e a ->
  e a ->
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a () (Dual e a (Var e a))
uniform a b l = MCLMC $ proc (_, hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD cauchyD -< V3 (auto 0) (auto 1) theta
  v <- opNAD uniformT -< V3 (auto a) (auto b) theta
  updateLP -< (hmc, v, lp)

cauchyD :: Floating a => V3 a -> a
cauchyD (V3 mu sigma2 x) = let y = x - mu in log1p (y*y/sigma2)

cauchy ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
cauchy l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD cauchyD -< V3 mu sigma2 theta
  updateLP -< (hmc, theta, lp)

cauchy' ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
cauchy' l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD cauchyD -< V3 (auto 0) (auto 1) theta
  phi <- opNAD (\(V3 m s t) -> m + sqrt s * t) -< V3 mu sigma2 theta
  updateLP -< (hmc, phi, lp)

logcauchy ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
logcauchy l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD cauchyD -< V3 mu sigma2 theta
  phi <- expA -< theta
  updateLP -< (hmc, phi, lp)

logcauchy' ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
logcauchy' l = MCLMC $ proc ((mu, sigma2), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD cauchyD -< V3 (auto 0) (auto 1) theta
  phi <- opNAD (\(V3 m s t) -> exp $ m + sqrt s * t) -< V3 mu sigma2 theta
  updateLP -< (hmc, phi, lp)

exponentialD :: Floating a => V2 a -> a
exponentialD (V2 lam x) = lam*exp x - x - log lam

exponential ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a)) (Dual e a (Var e a))
exponential l = MCLMC $ proc (lam, hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD exponentialD -< V2 lam theta
  phi <- expA -< theta
  updateLP -< (hmc, phi, lp)

exponential' ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a)) (Dual e a (Var e a))
exponential' l = MCLMC $ proc (lam, hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD exponentialD -< V2 (auto 1) theta
  phi <- opNAD (\(V2 lam' t) -> lam'*t) -< V2 lam theta
  updateLP -< (hmc, phi, lp)

laplaceD :: Floating a => V3 a -> a
laplaceD (V3 mu sigma x) = let y = x - mu in abs y / sigma + log sigma + log 2

laplace ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
laplace l = MCLMC $ proc ((mu, sigma), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD laplaceD -< V3 mu sigma theta
  updateLP -< (hmc, theta, lp)

laplace' ::
  (CmdRAD m e a, Floating (ADExp e a), Floating a, Eq a, ExpInject e a) =>
  Getting (Dual e a (Var e a)) s (Dual e a (Var e a)) ->
  MCLMC m e s a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
laplace' l = MCLMC $ proc ((mu, sigma), hmc) -> do
  let theta = hmc ^. hmcState . l
  lp <- opNAD laplaceD -< V3 (auto 0) (auto 1) theta
  phi <- opNAD (\(V3 m s t) -> m + s * t) -< V3 mu sigma theta
  updateLP -< (hmc, phi, lp)
