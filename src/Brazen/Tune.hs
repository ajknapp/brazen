module Brazen.Tune where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C
import Foreign.Ptr

foreign import ccall unsafe acfF :: Ptr Float -> Ptr Float -> CInt -> IO ()

foreign import ccall unsafe acfD :: Ptr Double -> Ptr Double -> CInt -> IO ()

class ACF a where
  acf :: Ptr a -> Ptr a -> Int -> IO ()

instance ACF Float where
  acf x p n = acfF x p (fromIntegral n)

instance ACF Double where
  acf x p n = acfD x p (fromIntegral n)

tuneNoiseLengthScale :: (VM.Storable a, Num a, Ord a, ACF a) => VS.Vector a -> IO (Maybe Int)
tuneNoiseLengthScale v = do
  let v' = v VS.++ VS.replicate n 0
      n = VS.length v
  VS.unsafeWith v' $ \x -> do
    m <- VM.new (VS.length v')
    VM.unsafeWith m $ \p -> acf x p (VS.length v')
    m' <- VS.unsafeFreeze m
    let go i
          | i >= n = Nothing
          | m' VS.! i < 0 = Just i
          | otherwise = go (i + 1)
    pure (go 0)
