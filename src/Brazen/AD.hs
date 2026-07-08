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
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Lens
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.HKD
import Data.Int
import Data.Monoid (Sum (..))
import Data.Proxy
import Data.Semigroup (Max (..))
import Data.Traversable
import Foreign.Ptr hiding (nullPtr)
import GHC.Float
import GHC.Generics hiding (to)
import qualified GHC.Generics as G
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

data Var e a where
  VConst :: e a -> Var e a
  VConst' :: e (Ptr a) -> Var e a
  VDyn :: e (Ptr a) -> e (Ptr a) -> Var e a

data Primal e a b where
  PrimalC :: e a -> Primal e a (Var e a)
  PrimalV :: e (Ptr a) -> Primal e a (Var e a)
  PrimalT :: Tensor ds e a -> Primal e a (Tensor ds e a)

data Dual e a b where
  DVar :: Var e a -> Dual e a (Var e a)
  DTBroadcast :: Dual e a (Var e a) -> Dual e a (Tensor sh e a)
  DTConst :: Tensor sh e a -> Dual e a (Tensor sh e a)
  DTensor :: Tensor sh e a -> Tensor sh e a -> Dual e a (Tensor sh e a)

data Tape e a = Tape {_tapePrimal, _tapeDual, _tapeScratch :: e (Ptr a)}

makeLenses ''Tape

data RADState = RADState {_radStateTapeSize :: Sum Int, _radStateScratchSize :: Max Int}
  deriving (Eq, Show, Ord, Generic)

makeLenses ''RADState

newtype RAD m e c a b = RAD
  { runAD :: State RADState (Kleisli (ReaderT (Tape e c) (Codensity m)) a b)
  }
  deriving
    (Functor, Applicative)
    via StaticMonadArrow (State RADState) (Kleisli (ReaderT (Tape e c) (Codensity m))) a
  deriving
    (Category, Arrow, ArrowChoice)
    via StaticMonadArrow (State RADState) (Kleisli (ReaderT (Tape e c) (Codensity m)))

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
    Monad m,
    JanusTyped e (Ptr a),
    CmdRef m e
  )

adTapeSize :: RAD m e c a b -> Int
adTapeSize (RAD f) = execState f (RADState 0 0) ^. radStateTapeSize . to getSum

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
  idx <- use (radStateTapeSize . to getSum)
  radStateTapeSize <>= 1
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
  idx <- use (radStateTapeSize . to getSum)
  radStateTapeSize <>= fromIntegral n
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
  idx <- use (radStateTapeSize . to getSum)
  radStateTapeSize <>= 1
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

-- inverseQuadraticForm ::
--   forall m e a n f.
--   ( CmdRAD m e a
--   , ExpShare e
--   , MonadReader (Tape e a) m
--   , Traversable f
--   , Applicative f
--   , KnownNat n
--   , JanusTyped e Int64
--   , Floating (e a)) =>
--   (f (ADExp e a) -> f (ADExp e a) -> ADExp e a) ->
--   (f (ADExp e a) -> f (ADExp e a) -> f (ADExp e a)) ->
--   RAD m e a (f (Dual e a (Vector n e a)), Dual e a (Vector n e a)) (Dual e a (Var e a))
-- inverseQuadraticForm f df = RAD $ do
--   let n = natVal (Proxy @n)
--   radStateScratchSize <>= Max (fromIntegral $ n*n)
--   vecIdx <- use radStateTapeSize
--   radStateTapeSize <>= Sum (fromIntegral n)
--   ansIdx <- use radStateTapeSize
--   radStateTapeSize <>= Sum 1
--   pure $ Kleisli $ \(xs,y) -> do
--     scratch <- use tapeScratch
--     (z, _) <- destination (getSum vecIdx)
--     (b, db) <- destination (getSum ansIdx)
--     let pn = Proxy @n
--         a = Tensor (TensorBoundCons pn (TensorBoundCons pn TensorBoundNil)) scratch
--     lift $ shift $ \k -> lift $ do
--       rangeM 0 (fromIntegral n) $ \i -> do
--         xs' <- for xs $ \xsi -> ADExp . toShared <$> readDTensorPrimal xsi (i :. Z)
--         rangeM 0 (i+1) $ \j -> do
--           ys' <- for xs $ \xsj -> ADExp . toShared <$> readDTensorPrimal xsj (j :. Z)
--           Identity aij <- shareM . Identity . getADExp $ f xs' ys'
--           writeTensor a (i :. j :. Z) aij
--       let extractVector :: Dual e a (Vector n e a) -> Vector n e a
--           extractVector (DTensor x _) = x
--           extractVector (DTConst x) = x
--           extractVector (DTBroadcast _) = error "extractVector: not implemented yet"
--       choleskySolve a (extractVector y) (Tensor (TensorBoundCons pn TensorBoundNil) z)
--       poke b 0
--       rangeM 0 (fromIntegral n) $ \i -> do
--         yi <- readDTensorPrimal y (i :. Z)
--         zi <- readTensor (Tensor (TensorBoundCons pn TensorBoundNil) z) (i :. Z)
--         b' <- peek b
--         poke b (b' + yi * zi)
--       ans <- k (DVar (VDyn b db))
--       dbVal <- peek db
--       rangeM 0 (fromIntegral n) $ \i -> do
--         zi <- readTensor (Tensor (TensorBoundCons pn TensorBoundNil) z) (i :. Z)
--         case y of
--           DTensor _ dy -> do
--             dyi <- readTensor dy (i :. Z)
--             writeTensor dy (i :. Z) (dyi + 2 * zi * dbVal)
--           DTBroadcast (DVar (VDyn _ dy)) -> do
--             dyVal <- peek dy
--             poke dy (dyVal + 2 * zi * dbVal)
--           _ -> pure ()
--         xi <- for xs $ \xsi -> ADExp . toShared <$> readDTensorPrimal xsi (i :. Z)
--         rangeM 0 (fromIntegral n) $ \j -> do
--           zj <- readTensor (Tensor (TensorBoundCons pn TensorBoundNil) z) (j :. Z)
--           xj <- for xs $ \xsj -> ADExp . toShared <$> readDTensorPrimal xsj (j :. Z)
--           g <- shareM . fmap getADExp $ df xi xj
--           for_ ((,) <$> xs <*> g) $ \(xsi_, gik) -> case xsi_ of
--             DTensor _ dxsi -> do
--               dxsiVal <- readTensor dxsi (i :. Z)
--               writeTensor dxsi (i :. Z) (dxsiVal - 2 * zi * zj * dbVal * gik)
--             DTBroadcast (DVar (VDyn _ dxsi)) -> do
--               dxsiVal <- peek dxsi
--               poke dxsi (dxsiVal - 2 * zi * zj * dbVal * gik)
--             _ -> pure ()
--       pure ans

-- -- -- solve Ax = y by computing the Cholesky distribution in-place and then doing
-- -- lower-triangular solves with x being stored in dest
-- choleskySolve :: forall m e a n. (CmdRAD m e a, KnownNat n, Floating (e a)) => Matrix n n e a -> Vector n e a -> Vector n e a -> m ()
-- choleskySolve a y dest = do undefined
-- --   let n = fromIntegral (natVal (Proxy @n)) :: e Int64
-- --   -- Cholesky decomposition A = L L^T in-place (lower triangle of a)
-- --   rangeM 0 n $ \i -> do
-- --     rangeM 0 i $ \j -> do
-- --       s <- newRef 0
-- --       rangeM 0 j $ \k -> do
-- --         lik <- readTensor a (i :. k :. Z)
-- --         ljk <- readTensor a (j :. k :. Z)
-- --         modifyRef s (+ (lik * ljk))
-- --       aij <- readTensor a (i :. j :. Z)
-- --       ljj <- readTensor a (j :. j :. Z)
-- --       sval <- readRef s
-- --       lij <- letM $ (aij - sval) / ljj
-- --       writeTensor a (i :. j :. Z) lij
-- --     s <- newRef 0
-- --     rangeM 0 i $ \k -> do
-- --       lik <- readTensor a (i :. k :. Z)
-- --       modifyRef s (+ (lik * lik))
-- --     aii <- readTensor a (i :. i :. Z)
-- --     sval <- readRef s
-- --     lii <- letM $ sqrt (aii - sval)
-- --     writeTensor a (i :. i :. Z) lii
-- --   -- Forward substitution L w = y, store w in dest
-- --   rangeM 0 n $ \i -> do
-- --     s <- newRef 0
-- --     rangeM 0 i $ \j -> do
-- --       lij <- readTensor a (i :. j :. Z)
-- --       wj <- readTensor dest (j :. Z)
-- --       modifyRef s (+ (lij * wj))
-- --     yi <- readTensor y (i :. Z)
-- --     lii <- readTensor a (i :. i :. Z)
-- --     sval <- readRef s
-- --     wi <- letM $ (yi - sval) / lii
-- --     writeTensor dest (i :. Z) wi
-- --   -- Backward substitution L^T x = w, store x in dest
-- --   rangeM 0 n $ \k -> do
-- --     let i = n - 1 - k
-- --     s <- newRef 0
-- --     rangeM (i + 1) n $ \j -> do
-- --       lji <- readTensor a (j :. i :. Z)
-- --       xj <- readTensor dest (j :. Z)
-- --       modifyRef s (+ (lji * xj))
-- --     wi <- readTensor dest (i :. Z)
-- --     lii <- readTensor a (i :. i :. Z)
-- --     sval <- readRef s
-- --     xi <- letM $ (wi - sval) / lii
-- --     writeTensor dest (i :. Z) xi

readDTensorPrimal :: (Applicative m, CmdStorable m e a, Num (e Int64)) => Dual e a (Tensor d e a) -> TensorIndex d e -> m (e a)
readDTensorPrimal (DTensor x _) idx = readTensor x idx
readDTensorPrimal (DTBroadcast (DVar (VConst x))) _ = pure x
readDTensorPrimal (DTBroadcast (DVar (VConst' x))) _ = peek x
readDTensorPrimal (DTBroadcast (DVar (VDyn x _))) _ = peek x
readDTensorPrimal (DTConst x) idx = readTensor x idx

destination :: forall m e a. (CmdRAD m e a) => Int -> ReaderT (Tape e a) (Codensity m) (e (Ptr a), e (Ptr a))
destination idx = do
  Tape t dt _scratch <- ask
  let inc = flip ptrIndex (fromIntegral idx)
      incM = lift . lift . letM . inc
  (,) <$> incM t <*> incM dt

destinationVector :: forall m e a n. (CmdRAD m e a, KnownNat n) => Int -> ReaderT (Tape e a) (Codensity m) (Vector n e a, Vector n e a)
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

withGradTape ::
  forall f m e s a.
  ( CmdRAD m e a,
    ExpPtrCast e,
    Tangential f e a,
    FZip (f e a),
    JanusTyped e (Ptr a)
  ) =>
  e (Ptr a) ->
  e (Ptr a) ->
  e (Ptr a) ->
  RAD m e a (f e a (Dual e a)) (s, Dual e a (Var e a)) ->
  (Tape e a -> f e a (Primal e a) -> m (e a, f e a (Primal e a), s) -> m ()) ->
  m ()
withGradTape tape dtape stape (RAD f) k = do
  let m = flatSize (Proxy @(f e a (Primal e a)))
      (f'', n) = runState f (RADState (Sum m) 0)
      tape' = Tape tape dtape stape
  k tape' (unpack tape) $ do
    rangeM 0 (fromIntegral $ n ^. radStateTapeSize . _Wrapped) $ \i -> pokeElemOff dtape 0 i
    (s, z) <- lowerCodensity $ reset $ do
      runReaderT (runKleisli f'' $ unpackTangent tape dtape) tape' >>= \case
        (s, DVar (VConst e)) -> pure (s, e)
        (s, DVar (VConst' e)) -> lift $ (s,) <$> peek e
        (s, DVar (VDyn z dz)) -> do
          z' <- lift $ peek z
          lift $ poke dz 1
          pure (s, z')
    pure (z, unpack dtape, s)

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
withGrad f k = do
  let m = flatSize (Proxy @(f e a (Primal e a)))
      s = execState (runAD f) (RADState (Sum m) 0)
      n = s ^. radStateTapeSize . _Wrapped
      ns = s ^. radStateScratchSize . _Wrapped
  tape <- malloc ((fromIntegral n :: e Int64) * sizeOf (Proxy @a)) >>= letM . fromVoidPtr
  dtape <- malloc ((fromIntegral n :: e Int64) * sizeOf (Proxy @a)) >>= letM . fromVoidPtr
  stape <- malloc ((fromIntegral ns :: e Int64) * sizeOf (Proxy @a)) >>= letM . fromVoidPtr
  withGradTape tape dtape stape f k
  free (toVoidPtr tape)
  free (toVoidPtr dtape)
  free (toVoidPtr stape)

class (Flat (f e a (Primal e a))) => Tangential f e a where
  unpack :: e (Ptr a) -> f e a (Primal e a)
  default unpack :: (Generic (f e a (Primal e a)), GTangential (Rep (f e a (Primal e a))) e a) => e (Ptr a) -> f e a (Primal e a)
  unpack p = G.to $ gunpack p

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
        _ -> error "unpackTangent: the impossible happened"
    )
    (unpack tape)
    (unpack dtape)

dconst :: (FFunctor (f e a)) => f e a (Primal e a) -> f e a (Dual e a)
dconst =
  ffmap
    ( \case
        PrimalC e -> DVar (VConst e)
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
