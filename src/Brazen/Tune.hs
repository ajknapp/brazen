module Brazen.Tune where

import Data.Complex
import Data.Vector.FFT
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

tuneNoiseLengthScale :: VS.Vector Double -> Maybe Int
tuneNoiseLengthScale v = go 0
  where
    ac = autocorr (VU.convert v)
    n = VS.length v
    go i
      | i >= n = Nothing
      | ac VU.! i < 0 = Just i
      | otherwise = go (i + 1)

autocorr :: VU.Vector Double -> VU.Vector Double
autocorr v = VU.map (\vi -> realPart vi / fromIntegral n) $ VU.take n v''
  where
    n = VU.length v
    vnorm = sqrt . VU.sum $ VU.map (\vi -> vi * vi) v
    v' = VU.map (/ vnorm) v VU.++ VU.replicate n 0
    u = fft (VU.map (:+ 0) v')
    u' = VU.map (\ui -> conjugate ui * ui) u
    v'' = ifft u'
