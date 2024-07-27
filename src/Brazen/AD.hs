{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Brazen.AD where

import Brazen.Numeric
import Brazen.Shared
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
import Janus.Command.Range
import Janus.Command.Ref
import Janus.Command.While
import Janus.Expression.Cast
import Janus.Expression.Let
import Janus.Expression.Ord
import Janus.Typed
import Linear
import Prelude hiding (id, (.))

data Primal e a b where
  PrimalV :: e (Ptr a) -> Primal e a (Var e a)
  PrimalT :: Tensor ds e a -> Primal e a (Tensor ds e a)

data Dual e a b where
  DVar :: Var e a -> Dual e a (Var e a)
  DTBroadcast :: Dual e a (Var e a) -> Dual e a (Tensor sh e a)
  DTConst :: Tensor sh e a -> Dual e a (Tensor sh e a)
  DTensor :: Tensor sh e a -> Tensor sh e a -> Dual e a (Tensor sh e a)

data Tape e a = Tape {tapePrimal, tapeDual :: e (Ptr a)}

newtype RAD m e c a b = RAD
  { runAD :: State Int (Kleisli (ReaderT (Tape e c) (Codensity m)) a b)
  }
  deriving
    (Functor, Applicative)
    via StaticMonadArrow (State Int) (Kleisli (ReaderT (Tape e c) (Codensity m))) a
  deriving
    (Category, Arrow, ArrowChoice)
    via StaticMonadArrow (State Int) (Kleisli (ReaderT (Tape e c) (Codensity m)))

instance (CmdRAD m e a) => ArrowNum (RAD m e a) (Dual e a (Var e a)) where
  addA = op2 (\x y -> (x + y, \dz -> (dz, dz)))
  subA = op2 (\x y -> (x - y, \dz -> (dz, negate dz)))
  mulA = op2 (\x y -> (x * y, \dwdz -> (dwdz * y, dwdz * x)))
  negateA = op1 (\x -> (negate x, negate))
  absA = op1 (\x -> (abs x, signum))
  signumA = op1 (\x -> (signum x, const 0))

instance (CmdRAD m e a, Fractional (ADExp e a)) => ArrowFractional (RAD m e a) (Dual e a (Var e a)) where
  divA = op2 (\x y -> (x / y, \dwdz -> (dwdz / y, dwdz * x / (y * y))))
  recipA = op1 (\x -> (recip x, negate . (* recip (x * x))))

instance (CmdRAD m e a, Floating (ADExp e a)) => ArrowFloating (RAD m e a) (Dual e a (Var e a)) where
  expA = op1 (\x -> let y = exp x in (y, (* y)))
  logA = op1 (\x -> (log x, (/ x)))
  sqrtA = op1 (\x -> let y = sqrt x in (y, (/ (2 * y))))
  powA = op2 (\x y -> let z = x ** y in (z, \dwdz -> (dwdz * y * (x ** (y - 1)), dwdz * log x * z)))
  logBaseA = op2 (\x y -> let z = logBase x y in (z, \dwdz -> (-dwdz * z / (x * log x), dwdz / (y * log x))))
  sinA = op1 (\x -> (sin x, (* cos x)))
  cosA = op1 (\x -> (cos x, negate . (* sin x)))
  tanA = op1 (\x -> (tan x, \dwdz -> let y = cos x in dwdz / y * y))
  asinA = op1 (\x -> (asin x, (/ sqrt (1 - x * x))))
  acosA = op1 (\x -> (acos x, negate . (/ sqrt (1 - x * x))))
  atanA = op1 (\x -> (atan x, (/ (1 + x * x))))
  sinhA = op1 (\x -> (sinh x, (* cosh x)))
  coshA = op1 (\x -> (cosh x, (* sinh x)))
  tanhA = op1 (\x -> (tanh x, (/ (let y = cosh x in y * y))))
  asinhA = op1 (\x -> (asinh x, (/ sqrt (x * x + 1))))
  acoshA = op1 (\x -> (acosh x, (/ sqrt (x * x - 1))))
  atanhA = op1 (\x -> (atanh x, (/ (1 - x * x))))
  log1pA = op1 (\x -> (log1p x, \dz -> dz / (x + 1)))
  expm1A = op1 (\x -> let y = expm1 x in (y, (* y)))
  log1pexpA = op1 (\x -> (log1pexpA x, \dz -> let y = exp x in dz * y / (y + 1)))
  log1mexpA = op1 (\x -> (log1mexpA x, \dz -> let y = exp x in dz * y / (y - 1)))

type CmdRAD m e a =
  ( JanusTyped e a,
    JanusTyped e Int64,
    Num (e Int64),
    ExpLet e,
    ExpOrd e Int64,
    ExpPtr e a,
    ExpShare e,
    CmdWhile m e,
    CmdStorable m e a,
    CmdRange m e,
    CmdRef m e,
    CmdMem m e,
    Num (e a),
    Num (ADExp e a),
    Monad m
  )

adSize :: RAD m e c a b -> Int
adSize (RAD f) = execState f 0

data Var e a where
  VConst :: e a -> Var e a
  VConst' :: e (Ptr a) -> Var e a
  VDyn :: e (Ptr a) -> e (Ptr a) -> Var e a

op1 ::
  forall m e a.
  (CmdRAD m e a) =>
  (ADExp e a -> (ADExp e a, ADExp e a -> ADExp e a)) ->
  RAD m e a (Dual e a (Var e a)) (Dual e a (Var e a))
op1 f =
  arr Identity
    >>> opN (\(Identity x) -> let (g, dg) = f x in (g, Identity . dg))

op2 ::
  forall m e a.
  (CmdRAD m e a) =>
  (ADExp e a -> ADExp e a -> (ADExp e a, ADExp e a -> (ADExp e a, ADExp e a))) ->
  RAD m e a (Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
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
  (ADExp e a -> ADExp e a -> ADExp e a -> (ADExp e a, ADExp e a -> (ADExp e a, ADExp e a, ADExp e a))) ->
  RAD m e a (Dual e a (Var e a), Dual e a (Var e a), Dual e a (Var e a)) (Dual e a (Var e a))
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
  (CmdRAD m e a, Traversable f, Applicative f, ExpShare e) =>
  (f (ADExp e a) -> (ADExp e a, ADExp e a -> f (ADExp e a))) ->
  RAD m e a (f (Dual e a (Var e a))) (Dual e a (Var e a))
opN f = RAD $ do
  idx <- get
  put (idx + 1)
  pure $ Kleisli $ \xs -> do
    xs' <- for xs $ \case
      DVar (VConst a) -> pure . ADExp $ toShared a
      DVar (VConst' a) -> lift . lift $ ADExp . toShared <$> peek a
      DVar (VDyn xi _) -> lift . lift $ ADExp . toShared <$> peek xi
    let (z', dzdx) = f xs'
    (z, dz) <- destination idx
    lift $ shift $ \k -> lift $ do
      poke z (share $ getADExp z')
      w <- k (DVar (VDyn z dz))
      dz' <- peek dz
      let dxdzs = dzdx (ADExp $ toShared dz')
      dxdzs' <- shareM (fmap getADExp dxdzs)
      for_ ((,) <$> xs <*> dxdzs') $ \(xi, dxidz) -> case xi of
        DVar (VDyn _ dxi) -> do
          dxi' <- peek dxi
          poke dxi $ dxi' + dxidz
        _ -> pure ()
      pure w

-- f must be a fixed size container with a zip-like applicative instance
opNMap ::
  forall m e a n f.
  (CmdRAD m e a, Traversable f, Applicative f, KnownNat n) =>
  (f (e a) -> e a) ->
  (f (e a) -> f (e a)) ->
  RAD m e a (f (Dual e a (Vector n e a))) (Dual e a (Vector n e a))
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
            DVar (VConst _) -> pure ()
            DVar (VConst' _) -> pure ()
            DVar (VDyn _ dxi) -> do
              dxi' <- peek dxi
              poke dxi (dxi' + dxidyi' * dyi')
          DTConst _ -> pure ()
      pure ans

-- f must be a fixed size container with a zip-like applicative instance
opNMapSum ::
  forall m e a n f.
  (CmdRAD m e a, Traversable f, Applicative f, KnownNat n) =>
  (f (ADExp e a) -> ADExp e a) ->
  (f (ADExp e a) -> f (ADExp e a)) ->
  RAD m e a (f (Dual e a (Vector n e a))) (Dual e a (Var e a))
opNMapSum f df = RAD $ do
  let n = natVal (Proxy @n)
  idx <- get
  put (idx + 1)
  pure $ Kleisli $ \xs -> do
    (y, dy) <- destination idx
    lift $ shift $ \k -> lift $ do
      poke y 0
      rangeM 0 (fromIntegral n) $ \i -> do
        xs' <- for xs $ \xi -> ADExp . toShared <$> readDTensorPrimal xi (i :. Z)
        y' <- peek y
        poke y (y' + share (getADExp $ f xs'))
      ans <- k (DVar (VDyn y dy))
      rangeM 0 (fromIntegral n) $ \i -> do
        xs' <- for xs $ \xi -> ADExp . toShared <$> readDTensorPrimal xi (i :. Z)
        dyi' <- peek dy
        dxdy' <- shareM . fmap getADExp $ df xs'
        for_ ((,) <$> xs <*> dxdy') $ \(dxi_, dxidyi') -> case dxi_ of
          DTensor _ dxi -> do
            dxi' <- readTensor dxi (i :. Z)
            writeTensor dxi (i :. Z) (dxi' + dxidyi' * dyi')
          DTBroadcast xi_ -> case xi_ of
            DVar (VConst _) -> pure ()
            DVar (VConst' _) -> pure ()
            DVar (VDyn _ dxi) -> do
              dxi' <- peek dxi
              poke dxi (dxi' + dxidyi' * dyi')
          DTConst _ -> pure ()
      pure ans

readDTensorPrimal :: (Applicative m, CmdStorable m e a, Num (e Int64)) => Dual e a (Tensor d e a) -> TensorIndex d e -> m (e a)
readDTensorPrimal (DTensor x _) idx = readTensor x idx
readDTensorPrimal (DTBroadcast (DVar (VConst x))) _ = pure x
readDTensorPrimal (DTBroadcast (DVar (VConst' x))) _ = peek x
readDTensorPrimal (DTBroadcast (DVar (VDyn x _))) _ = peek x
readDTensorPrimal (DTConst x) idx = readTensor x idx

destination :: forall m e a. (ExpPtr e a, Num (e Int64), ExpSized e a) => Int -> ReaderT (Tape e a) (Codensity m) (e (Ptr a), e (Ptr a))
destination idx = do
  Tape t dt <- ask
  let inc = flip ptrIndex (fromIntegral idx)
  pure (inc t, inc dt)

destinationVector :: forall m e a n. (ExpPtr e a, Num (e Int64), ExpSized e a, KnownNat n) => Int -> ReaderT (Tape e a) (Codensity m) (Vector n e a, Vector n e a)
destinationVector idx = do
  let bds = TensorBoundCons (Proxy @n) TensorBoundNil
  (t, dt) <- destination idx
  pure (Tensor bds t, Tensor bds dt)

auto :: e a -> Dual e a (Var e a)
auto = DVar . VConst

type DTensor sh e a = Dual e a (Tensor sh e a)

type DVector n e a = DTensor '[n] e a

type DMatrix n m e a = DTensor '[n, m] e a

autoT :: Tensor sh e a -> DTensor sh e a
autoT = DTConst

broadcast :: Dual e a (Var e a) -> Dual e a (Tensor sh e a)
broadcast = DTBroadcast

dotA :: forall m e a n. (CmdRAD m e a, KnownNat n) => RAD m e a (DVector n e a, DVector n e a) (Dual e a (Var e a))
dotA = arr (uncurry V2) >>> opNMapSum (\(V2 x y) -> x * y) (\(V2 x y) -> V2 y x)

class Flat a where
  flatSize :: Proxy a -> Int
  default flatSize :: (Generic a, GFlat (Rep a)) => Proxy a -> Int
  flatSize _ = gflatSize (Proxy @(Rep a ()))

instance Flat (Var e a) where
  flatSize _ = 1

instance (Flat b) => Flat (Primal e a b) where
  flatSize _ = flatSize (Proxy @b)

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
  forall f m e s a.
  ( CmdRAD m e a,
    ExpPtrCast e,
    Tangential f e a,
    FZip (f e a),
    JanusTyped e (Ptr a)
  ) =>
  RAD m e a (f e a (Dual e a)) (s, Dual e a (Var e a)) ->
  (Tape e a -> f e a (Primal e a) -> m (e a, f e a (Primal e a), s) -> m ()) ->
  m ()
withGrad (RAD f) k = do
  let m = flatSize (Proxy @(f e a (Primal e a)))
      (f'', n) = runState f m
  tape <- malloc ((fromIntegral n :: e Int64) * sizeOf (Proxy @a)) >>= letM . fromVoidPtr
  dtape <- malloc ((fromIntegral n :: e Int64) * sizeOf (Proxy @a)) >>= letM . fromVoidPtr
  let tape' = Tape tape dtape
  k tape' (unpack tape) $ do
    rangeM 0 (fromIntegral n) $ \i -> pokeElemOff dtape 0 i
    (s, z) <- lowerCodensity $ reset $ do
      runReaderT (runKleisli f'' $ unpackTangent tape dtape) tape' >>= \case
        (s, DVar (VConst e)) -> pure (s, e)
        (s, DVar (VConst' e)) -> lift $ (s,) <$> peek e
        (s, DVar (VDyn z dz)) -> do
          z' <- lift $ peek z
          lift $ poke dz 1
          pure (s, z')
    pure (z, unpack dtape, s)
  free (toVoidPtr tape)
  free (toVoidPtr dtape)

class (Flat (f e a (Primal e a))) => Tangential f e a where
  unpack :: e (Ptr a) -> f e a (Primal e a)
  default unpack :: (Generic (f e a (Primal e a)), GTangential (Rep (f e a (Primal e a))) e a) => e (Ptr a) -> f e a (Primal e a)
  unpack p = GHC.Generics.to $ gunpack p

class GTangential f e a where
  gunpack :: e (Ptr a) -> f b

instance (GTangential f e a, GTangential g e a, GFlat f, Num (e Int64), ExpSized e a, ExpPtr e a) => GTangential (f :*: g) e a where
  gunpack p = gunpack p :*: gunpack (p `ptrIndex` fromIntegral (gflatSize (Proxy @(f a))))

instance GTangential (K1 i (Primal e a (Var e a))) e a where
  gunpack p = K1 $ PrimalV p

instance (ReifyTensorBound sh) => GTangential (K1 i (Primal e a (Tensor sh e a))) e a where
  gunpack p = K1 $ PrimalT $ Tensor (tensorBound (Proxy @sh)) p

instance (GTangential f e a) => GTangential (M1 c i f) e a where
  gunpack p = M1 (gunpack p)

unpackTangent ::
  (FZip (f e a), Tangential f e a) =>
  e (Ptr a) ->
  e (Ptr a) ->
  f e a (Dual e a)
unpackTangent tape dtape =
  fzipWith
    ( \x y -> case (x, y) of
        (PrimalV x', PrimalV y') -> DVar (VDyn x' y')
        (PrimalT x', PrimalT y') -> DTensor x' y'
    )
    (unpack tape)
    (unpack dtape)

dconst :: (FFunctor (f e a)) => f e a (Primal e a) -> f e a (Dual e a)
dconst =
  ffmap
    ( \case
        PrimalV e -> DVar (VConst' e)
        PrimalT t -> DTConst t
    )

primalize :: (FFunctor (f e a)) => f e a (Dual e a) -> f e a (Primal e a)
primalize =
  ffmap
    ( \case
        DVar (VDyn v _) -> PrimalV v
        DTensor t _ -> PrimalT t
        _ -> error "primalize: the impossible happened"
    )

--------------------------------------------------------------------------------

data DingoDango e a f = DingoDango {f1 :: f (Var e a), f2 :: f (Vector 6 e a)}
  deriving (Generic)

instance FFunctor (DingoDango e a) where ffmap = ffmapDefault

instance FFoldable (DingoDango e a) where ffoldMap = ffoldMapDefault

instance FTraversable (DingoDango e a) where ftraverse = gftraverse

instance FZip (DingoDango e a) where fzipWith = gfzipWith

instance FRepeat (DingoDango e a) where frepeat = gfrepeat

instance Flat (DingoDango e a (Primal e a))

instance (Num (e Int64), ExpSized e a, ExpPtr e a) => Tangential DingoDango e a
