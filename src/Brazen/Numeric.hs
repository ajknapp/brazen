{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Brazen.Numeric where

import Control.Arrow
import GHC.Float
import Foreign.C
import Data.Coerce

class Arrow k => ArrowNum k a where
  addA :: (a, a) `k` a
  subA :: (a, a) `k` a
  mulA :: (a, a) `k` a
  negateA :: a `k` a
  absA :: a `k` a
  signumA :: a `k` a

instance Num a => ArrowNum (->) a where
  addA = uncurry (+)
  subA = uncurry (-)
  mulA = uncurry (*)
  negateA = negate
  absA = abs
  signumA = signum

class Arrow k => ArrowFractional k a where
  divA :: (a, a) `k` a
  recipA :: a `k` a

instance Fractional a => ArrowFractional (->) a where
  divA = uncurry (/)
  recipA = recip

class Arrow k => ArrowFloating k a where
  expA :: a `k` a
  logA :: a `k` a
  sqrtA :: a `k` a
  powA :: (a, a) `k` a
  logBaseA :: (a, a) `k` a
  sinA :: a `k` a
  cosA :: a `k` a
  tanA :: a `k` a
  asinA :: a `k` a
  acosA :: a `k` a
  atanA :: a `k` a
  sinhA :: a `k` a
  coshA :: a `k` a
  tanhA :: a `k` a
  asinhA :: a `k` a
  acoshA :: a `k` a
  atanhA :: a `k` a
  log1pA :: a `k` a
  expm1A :: a `k` a
  log1pexpA :: a `k` a
  log1mexpA :: a `k` a

instance Floating a => ArrowFloating (->) a where
  expA = exp
  logA = log
  sqrtA = sqrt
  powA = uncurry (**)
  logBaseA = uncurry logBase
  sinA = sin
  cosA = cos
  tanA = tan
  asinA = asin
  acosA = acos
  atanA = atan
  sinhA = sinh
  coshA = cosh
  tanhA = tanh
  asinhA = asinh
  acoshA = acosh
  atanhA = atanh
  log1pA = log1p
  expm1A = expm1
  log1pexpA = log1pexp
  log1mexpA = log1mexp

class Floating a => Gamma a where
  lngamma :: a -> a
  polygamma :: Int -> a -> a

digamma :: Gamma a => a -> a
digamma = polygamma 0
{-# INLINE digamma #-}

foreign import ccall "math.h lgammaf" lgammaf_c :: CFloat -> CFloat
foreign import ccall "polygammaf" polygammaf_c :: CInt -> CFloat -> CFloat

instance Gamma Float where
  lngamma = coerce lgammaf_c
  polygamma n = coerce (polygammaf_c (fromIntegral n))

foreign import ccall "math.h lgamma" lgamma_c :: CDouble -> CDouble
foreign import ccall "polygamma" polygamma_c :: CInt -> CDouble -> CDouble

instance Gamma Double where
  lngamma = coerce lgamma_c
  polygamma n = coerce (polygamma_c (fromIntegral n))
