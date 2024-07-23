{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Brazen.Partial where

import Brazen.Numeric
import Brazen.Shared
import GHC.Float
import Janus.Expression.Inject

data Partial e a = Static a | Dynamic (ADExp e a)

partialUnop
  :: (ExpInject e a) =>
     (a -> a)
     -> (ADExp e a -> ADExp e a)
     -> Partial e a
     -> Partial e a
partialUnop fs _ (Static a) = Static (fs a)
partialUnop _ fd (Dynamic a) = Dynamic (fd a)

partialBinop
  :: forall e a. (ExpInject e a, ExpShare e) =>
     (a -> a -> a)
     -> (ADExp e a -> ADExp e a -> ADExp e a)
     -> Partial e a
     -> Partial e a
     -> Partial e a
partialBinop fs _ (Static a) (Static b) = Static (fs a b)
partialBinop _ fd (Static a) (Dynamic b) = Dynamic (fd (ADExp . toShared $ inject @e a) b)
partialBinop _ fd (Dynamic a) (Static b) = Dynamic (fd a (ADExp . toShared $ inject @e b))
partialBinop _ fd (Dynamic a) (Dynamic b) = Dynamic (fd a b)

instance (ExpInject e a, ExpShare e, Eq a, Num a, Num (ADExp e a)) => Num (Partial e a) where
  fromInteger = Static . fromInteger
  Static a + Static b = Static (a + b)
  Static 0 + Dynamic b = Dynamic b
  Static a + Dynamic b = Dynamic (ADExp (toShared (inject @e a)) + b)
  Dynamic a + Static 0 = Dynamic a
  Dynamic a + Static b = Dynamic (a + ADExp (toShared (inject @e b)))
  Dynamic a + Dynamic b = Dynamic (a + b)
  Static a - Static b = Static (a - b)
  Static 0 - Dynamic b = Dynamic (negate b)
  Static a - Dynamic b = Dynamic (ADExp (toShared (inject @e a)) - b)
  Dynamic a - Static 0 = Dynamic a
  Dynamic a - Static b = Dynamic (a - ADExp (toShared (inject @e b)))
  Dynamic a - Dynamic b = Dynamic (a - b)
  Static a * Static b = Static (a * b)
  Static 1 * Dynamic b = Dynamic b
  Static a * Dynamic b = Dynamic (ADExp (toShared (inject @e a)) * b)
  Dynamic a * Static 1 = Dynamic a
  Dynamic a * Static b = Dynamic (a * ADExp (toShared (inject @e b)))
  Dynamic a * Dynamic b = Dynamic (a * b)
  negate = partialUnop negate negate
  abs = partialUnop abs abs
  signum = partialUnop signum signum

instance (ExpInject e a, ExpShare e, Eq a, Fractional a, Fractional (ADExp e a)) => Fractional (Partial e a) where
  (/) = partialBinop (/) (/)
  recip = partialUnop recip recip
  fromRational = Static . fromRational

instance (ExpInject e a, ExpShare e, Eq a, Floating a, Floating (ADExp e a)) => Floating (Partial e a) where
  pi = Static pi
  exp = partialUnop exp exp
  log = partialUnop log log
  sqrt = partialUnop sqrt sqrt
  Static a ** Static b = Static (a ** b)
  Dynamic _ ** Static 0 = Static 1
  Dynamic a ** Static 1 = Dynamic a
  Dynamic a ** Static b = Dynamic (a ** ADExp (toShared (inject @e b)))
  Static a ** Dynamic b = Dynamic (ADExp (toShared (inject @e a)) ** b)
  Dynamic a ** Dynamic b = Dynamic (a ** b)
  logBase = partialBinop logBase logBase
  sin = partialUnop sin sin
  cos = partialUnop cos cos
  tan = partialUnop tan tan
  asin = partialUnop asin asin
  acos = partialUnop acos acos
  atan = partialUnop atan atan
  sinh = partialUnop sinh sinh
  cosh = partialUnop cosh cosh
  tanh = partialUnop tanh tanh
  asinh = partialUnop asinh asinh
  acosh = partialUnop acosh acosh
  atanh = partialUnop atanh atanh
  log1p = partialUnop log1p log1p
  expm1 = partialUnop expm1 expm1
  log1pexp = partialUnop log1pexp log1pexp
  log1mexp = partialUnop log1mexp log1mexp

instance (ExpInject e a, ExpShare e, Eq a, Gamma a, Gamma (ADExp e a)) => Gamma (Partial e a) where
  lngamma = partialUnop lngamma lngamma
  polygamma n = partialUnop (polygamma n) (polygamma n)

runPartial :: forall e a. (ExpInject e a, ExpShare e) => Partial e a -> ADExp e a
runPartial (Static a) = ADExp $ toShared (inject @e a)
runPartial (Dynamic a) = a
