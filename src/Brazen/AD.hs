{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Brazen.AD where

import Brazen.Numeric
import Control.Applicative
import Control.Arrow
import Control.Arrow.Transformer.Static
import Control.Category
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Functor.Identity
import Data.HKD
import Data.Int
import Data.Proxy
import Data.Traversable
import Foreign.Ptr hiding (nullPtr)
import GHC.Float
import GHC.Generics
import GHC.TypeLits
import Janus.Command.Array
import Janus.Command.Format
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Cast
import Janus.Expression.Let
import Janus.Expression.Ord
import Linear
import Prelude hiding (id, (.))

data Dual a = Dual {primal, dual :: a}
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Tape e a = Tape {tapePrimal, tapeDual :: e (Ptr a)}

newtype RAD m e c a b = RAD
  { runAD :: State Int (Kleisli (ReaderT (Tape e c) (Codensity m)) a b)
  }
  deriving
    (Category, Arrow, ArrowChoice)
    via StaticMonadArrow (State Int) (Kleisli (ReaderT (Tape e c) (Codensity m)))

instance (CmdRAD m e a) => ArrowNum (RAD m e a) (DVar e a) where
  addA = op2 (\x y -> (x + y, \dz -> (dz, dz)))
  subA = op2 (\x y -> (x - y, \dz -> (dz, negate dz)))
  mulA = op2 (\x y -> (x * y, \dwdz -> (dwdz * y, dwdz * x)))
  negateA = op1 (\x -> (negate x, negate))
  absA = op1 (\x -> (abs x, signum))
  signumA = op1 (\x -> (signum x, const 0))

instance (CmdRAD m e a, Fractional (e a)) => ArrowFractional (RAD m e a) (DVar e a) where
  divA = op2 (\x y -> (x / y, \dwdz -> (dwdz / y, dwdz * x / (y * y))))
  recipA = op1 (\x -> (recip x, negate . (* recip (x * x))))

instance (CmdRAD m e a, Floating (e a)) => ArrowFloating (RAD m e a) (DVar e a) where
  expA = op1 (\x -> let y = let_ (exp x) id in (y, (* y)))
  logA = op1 (\x -> (log x, (/ x)))
  sqrtA = op1 (\x -> let y = let_ (sqrt x) id in (y, (/ (2 * y))))
  powA = op2 (\x y -> let z = let_ (x ** y) id in (z, \dwdz -> (dwdz * y * (x ** (y - 1)), dwdz * log x * z)))
  logBaseA = op2 (\x y -> let z = let_ (logBase x y) id in (z, \dwdz -> (-dwdz * z / (x * log x), dwdz / (y * log x))))
  sinA = op1 (\x -> (sin x, (* cos x)))
  cosA = op1 (\x -> (cos x, negate . (* sin x)))
  tanA = op1 (\x -> (tan x, \dwdz -> let y = let_ (cos x) id in dwdz / y * y))
  asinA = op1 (\x -> (asin x, (/ sqrt (1 - x * x))))
  acosA = op1 (\x -> (acos x, negate . (/ sqrt (1 - x * x))))
  atanA = op1 (\x -> (atan x, (/ (1 + x * x))))
  sinhA = op1 (\x -> (sinh x, (* cosh x)))
  coshA = op1 (\x -> (cosh x, (* sinh x)))
  tanhA = op1 (\x -> (tanh x, (/ (let y = let_ (cosh x) id in y * y))))
  asinhA = op1 (\x -> (asinh x, (/ sqrt (x * x + 1))))
  acoshA = op1 (\x -> (acosh x, (/ sqrt (x * x - 1))))
  atanhA = op1 (\x -> (atanh x, (/ (1 - x * x))))
  log1pA = op1 (\x -> (log1p x, \dz -> dz / (x + 1)))
  expm1A = op1 (\x -> let y = let_ (expm1 x) id in (y, (* y)))
  log1pexpA = op1 (\x -> (log1pexpA x, \dz -> let_ (exp x) $ \y -> dz * y / (y + 1)))
  log1mexpA = op1 (\x -> (log1mexpA x, \dz -> let_ (exp x) $ \y -> dz * y / (y - 1)))

type CmdRAD m e a =
  ( Num (e Int64),
    ExpLet e,
    ExpOrd e Int64,
    ExpPtr e a,
    CmdWhile m e,
    CmdStorable m e a,
    CmdRef m e a,
    CmdRef m e Int64,
    CmdMem m e,
    Num (e a),
    Monad m
  )

adSize :: RAD m e c a b -> Int
adSize (RAD f) = execState f 0

newtype Var e a = Var {getVar :: e (Ptr a)}

peekVar :: CmdStorable m e a => Var e a -> m (e a)
peekVar = peek . getVar

pokeVar :: CmdStorable m e a => Var e a -> e a -> m ()
pokeVar (Var x) = poke x

data DVar e a where
  DVar :: Var e a -> Var e a -> DVar e a
  DVConst :: e a -> DVar e a
  DVConst' :: Var e a -> DVar e a

op1 ::
  forall m e a.
  (CmdRAD m e a) =>
  (e a -> (e a, e a -> e a)) ->
  RAD m e a (DVar e a) (DVar e a)
op1 f =
  arr Identity
    >>> opN (\(Identity x) -> let (g, dg) = f x in (g, Identity . dg))

op2 ::
  forall m e a.
  (CmdRAD m e a) =>
  (e a -> e a -> (e a, e a -> (e a, e a))) ->
  RAD m e a (DVar e a, DVar e a) (DVar e a)
op2 f =
  arr (uncurry V2)
    >>> opN
      ( \(V2 x y) ->
          let (g, dg) = f x y
           in (g, \dh -> let (dx, dy) = dg dh in V2 dx dy)
      )

op3 ::
  forall m e a.
  (CmdRAD m e a) =>
  (e a -> e a -> e a -> (e a, e a -> (e a, e a, e a))) ->
  RAD m e a (DVar e a, DVar e a, DVar e a) (DVar e a)
op3 f =
  arr (\(x, y, z) -> V3 x y z)
    >>> opN
      ( \(V3 x y z) ->
          let (g, dg) = f x y z
           in (g, \dh -> let (dx, dy, dz) = dg dh in V3 dx dy dz)
      )

-- f must be a fixed size container with a zip-like applicative instance
opN ::
  forall m e a f.
  (CmdRAD m e a, Traversable f, Applicative f) =>
  (f (e a) -> (e a, e a -> f (e a))) ->
  RAD m e a (f (DVar e a)) (DVar e a)
opN f = RAD $ do
  idx <- get
  put (idx + 1)
  pure $ Kleisli $ \xs -> do
    xs' <- for xs $ \case
      DVar (Var xi) _ -> lift . lift $ peek xi
      DVConst a -> pure a
      DVConst' (Var a) -> lift $ lift $ peek a
    let (z', dzdx) = f xs'
    (z, dz) <- destination idx
    lift $ shift $ \k -> lift $ do
      pokeVar z z'
      w <- k (DVar z dz)
      dz' <- peekVar dz
      let dxdzs = dzdx dz'
      for_ ((,) <$> xs <*> dxdzs) $ \(xi, dxidz) -> case xi of
        DVar _ dxi -> do
          dxi' <- peekVar dxi
          pokeVar dxi $ dxi' + dxidz
        _ -> pure ()
      pure w

-- f must be a fixed size container with a zip-like applicative instance
opNMap ::
  forall m e a n f.
  (CmdRAD m e a, Traversable f, Applicative f, KnownNat n) =>
  (f (e a) -> e a) ->
  (f (e a) -> f (e a)) ->
  RAD m e a (f (DVector n e a)) (DVector n e a)
opNMap f df = RAD $ do
  let n = natVal (Proxy @n)
  idx <- get
  put (idx + fromIntegral n)
  pure $ Kleisli $ \xs -> do
    (y, dy) <- destinationVector idx
    lift $ shift $ \k -> lift $ do
      rangeM 0 (fromIntegral n) $ \i -> do
        xs' <- for xs $ \xi -> readDTensorPrimal xi (i :. Z)
        writeTensor y (i :. Z) (f xs')
      ans <- k (DTensor y dy)
      rangeM 0 (fromIntegral n) $ \i -> do
        xs' <- for xs $ \xi -> readDTensorPrimal xi (i :. Z)
        dyi' <- readTensor dy (i :. Z)
        let dxdy' = df xs'
        for_ ((,) <$> xs <*> dxdy') $ \(dxi_, dxidyi') -> case dxi_ of
          DTensor _ dxi -> do
            dxi' <- readTensor dxi (i :. Z)
            writeTensor dxi (i :. Z) (dxi' + dxidyi' * dyi')
          DTBroadcast xi -> case xi of
            DVar _ dxi -> do
              dxi' <- peekVar dxi
              pokeVar dxi (dxi' + dxidyi' * dyi')
            DVConst _ -> pure ()
            DVConst' _ -> pure ()
          DTConst _ -> pure ()
      pure ans

-- f must be a fixed size container with a zip-like applicative instance
opNMapSum ::
  forall m e a n f.
  (CmdRAD m e a, Traversable f, Applicative f, KnownNat n) =>
  (f (e a) -> e a) ->
  (f (e a) -> f (e a)) ->
  RAD m e a (f (DVector n e a)) (DVar e a)
opNMapSum f df = RAD $ do
  let n = natVal (Proxy @n)
  idx <- get
  put (idx + 1)
  pure $ Kleisli $ \xs -> do
    (y, dy) <- destination idx
    lift $ shift $ \k -> lift $ do
      poke (getVar y) 0
      rangeM 0 (fromIntegral n) $ \i -> do
        xs' <- for xs $ \xi -> readDTensorPrimal xi (i :. Z)
        y' <- peekVar y
        pokeVar y (y' + f xs')
      ans <- k (DVar y dy)
      rangeM 0 (fromIntegral n) $ \i -> do
        xs' <- for xs $ \xi -> readDTensorPrimal xi (i :. Z)
        dyi' <- peekVar dy
        let dxdy' = df xs'
        for_ ((,) <$> xs <*> dxdy') $ \(dxi_, dxidyi') -> case dxi_ of
          DTensor _ dxi -> do
            dxi' <- readTensor dxi (i :. Z)
            writeTensor dxi (i :. Z) (dxi' + dxidyi' * dyi')
          DTBroadcast xi_ -> case xi_ of
            DVar _ dxi -> do
              dxi' <- peekVar dxi
              pokeVar dxi (dxi' + dxidyi' * dyi')
            DVConst _ -> pure ()
            DVConst' _ -> pure ()
          DTConst _ -> pure ()
      pure ans

readDTensorPrimal :: (Applicative m, CmdStorable m e a, Num (e Int64)) => DTensor d e a -> TensorIndex d e -> m (e a)
readDTensorPrimal (DTensor x _) idx = readTensor x idx
readDTensorPrimal (DTBroadcast (DVar (Var x) _)) _ = peek x
readDTensorPrimal (DTBroadcast (DVConst x)) _ = pure x
readDTensorPrimal (DTBroadcast (DVConst' (Var x))) _ = peek x
readDTensorPrimal (DTConst x) idx = readTensor x idx

destination :: forall m e a. (ExpPtr e a, Num (e Int64), ExpSized e a) => Int -> ReaderT (Tape e a) (Codensity m) (Var e a, Var e a)
destination idx = do
  Tape t dt <- ask
  let inc = flip ptrAdd (fromIntegral idx * sizeOf (Proxy @a))
  pure (Var $ inc t, Var $ inc dt)

destinationVector :: forall m e a n. (ExpPtr e a, Num (e Int64), ExpSized e a, KnownNat n) => Int -> ReaderT (Tape e a) (Codensity m) (Vector n e a, Vector n e a)
destinationVector idx = do
  let bds = TensorBoundCons (Proxy @n) TensorBoundNil
  (Var t, Var dt) <- destination idx
  pure (Tensor bds t, Tensor bds dt)

auto :: e a -> DVar e a
auto = DVConst

data DTensor sh e a where
  DTBroadcast :: DVar e a -> DTensor sh e a
  DTConst :: Tensor sh e a -> DTensor sh e a
  DTensor :: Tensor sh e a -> Tensor sh e a -> DTensor sh e a

type DVector n e a = DTensor '[n] e a

type DMatrix n m e a = DTensor '[n, m] e a

autoT :: Tensor sh e a -> DTensor sh e a
autoT = DTConst

dotA :: forall m e a n. (CmdRAD m e a, KnownNat n) => RAD m e a (DVector n e a, DVector n e a) (DVar e a)
dotA = arr (uncurry V2) >>> opNMapSum (\(V2 x y) -> x * y) (\(V2 x y) -> V2 y x)

class Flat a where
  flatSize :: Proxy a -> Int
  default flatSize :: (Generic a, GFlat (Rep a)) => Proxy a -> Int
  flatSize _ = gflatSize (Proxy @(Rep a ()))

instance Flat (Var e a) where
  flatSize _ = 1

instance (Flat a) => Flat (Primal a) where
  flatSize _ = flatSize (Proxy @a)

instance Flat (Tensor '[] e a) where
  flatSize _ = 1

instance (KnownNat n, Flat (Tensor ns e a)) => Flat (Tensor (n ': ns) e a) where
  flatSize _ = fromIntegral (natVal (Proxy @n)) * flatSize (Proxy @(Tensor ns e a))

class GFlat f where
  gflatSize :: Proxy (f a) -> Int

instance (GFlat f) => GFlat (M1 i c f) where
  gflatSize p = gflatSize (strip p)
    where
      strip :: Proxy (M1 i c f a) -> Proxy (f a)
      strip _ = Proxy

instance (GFlat f, GFlat g) => GFlat (f :*: g) where
  gflatSize _ = gflatSize (Proxy @(f _)) + gflatSize (Proxy @(g _))

instance (Flat a) => (GFlat (Rec0 a)) where
  gflatSize _ = flatSize (Proxy @a)

withGrad ::
  forall f m e a.
  ( CmdRAD m e a,
    ExpPtrCast e () a,
    ExpPtrCast e a (),
    Tangential f e a,
    FZip (f e a)
  ) =>
  RAD m e a (f e a Dual) (DVar e a) ->
  (Tape e a -> m (e a, f e a Primal) -> m ()) ->
  m ()
withGrad (RAD f) k = do
  let m = flatSize (Proxy @(f e a Primal))
      (f'', n) = runState f m
  tape <- ptrCast <$> malloc ((fromIntegral n :: e Int64) * sizeOf (Proxy @a))
  dtape <- ptrCast <$> malloc ((fromIntegral n :: e Int64) * sizeOf (Proxy @a))
  let tape' = Tape tape dtape
  k tape' $ do
    rangeM 0 (fromIntegral n) $ \i -> pokeElemOff dtape 0 i
    z <- lowerCodensity $ reset $ do
      runReaderT (runKleisli f'' $ unpackTangent tape dtape) tape' >>= \case
        DVar z dz -> do
          z' <- lift $ peekVar z
          lift $ pokeVar dz 1
          pure z'
        DVConst a -> pure a
        DVConst' (Var a) -> lift $ peek a
    pure (z, unpack dtape)
  free (ptrCast tape)
  free (ptrCast dtape)
  pure ()

data Primal a where
  PrimalV :: Var e a -> Primal (Var e a)
  PrimalT :: Tensor ds e a -> Primal (Tensor ds e a)

class (Flat (f e a Primal)) => Tangential f e a where
  unpack :: e (Ptr a) -> f e a Primal

unpackTangent ::
  (FZip (f e a), Tangential f e a) =>
  e (Ptr a) ->
  e (Ptr a) ->
  f e a Dual
unpackTangent tape dtape =
  fzipWith
    ( \x y -> case (x, y) of
        (PrimalV x', PrimalV y') -> Dual x' y'
        (PrimalT x', PrimalT y') -> Dual x' y'
    )
    (unpack tape)
    (unpack dtape)

--------------------------------------------------------------------------------

data DingoDango e a f = DingoDango {f1 :: f (Var e a), f2 :: f (Vector 6 e a)}
  deriving (Generic)

instance FFunctor (DingoDango e a) where ffmap = ffmapDefault

instance FFoldable (DingoDango e a) where ffoldMap = ffoldMapDefault

instance FTraversable (DingoDango e a) where ftraverse = gftraverse

instance FZip (DingoDango e a) where fzipWith = gfzipWith

instance FRepeat (DingoDango e a) where frepeat = gfrepeat

instance Flat (DingoDango e a Primal)

instance (ExpSized e a, ExpPtr e a) => Tangential DingoDango e a where
  unpack tape = DingoDango (PrimalV $ Var tape) (PrimalT $ Tensor (TensorBoundCons Proxy TensorBoundNil) (tape `ptrAdd` sizeOf (Proxy @a)))

foo :: IO ()
foo = do
  withGrad (arr (\x -> Identity $ DTensor (primal $ f2 x) (dual $ f2 x)) >>> opNMapSum (\(Identity z) -> let_ (sin z) $ \y -> y * y) (\z -> 2 * sin z * cos z)) $ \tape grad -> do
    rangeM (0 :: Identity Int) 3 $ \_ -> do
      let Tape tape' _ = tape
      pokeElemOff tape' 0 0
      pokeElemOff tape' 1 1
      pokeElemOff tape' 2 2
      pokeElemOff tape' 3 3
      pokeElemOff tape' 4 4
      pokeElemOff tape' 5 5
      pokeElemOff tape' 8 6
      (lp, dlp) <- grad :: IO (Identity Double, DingoDango Identity Double Primal)
      format lp
      rangeM 0 6 $ readTensor ((\(PrimalT x) -> x) $ f2 dlp) . (:. Z) >=> format
