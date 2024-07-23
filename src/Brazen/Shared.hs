{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Brazen.Shared where

import Control.Monad.Cont
import Control.Monad.State
import Data.Fix
import Data.Functor.Identity
import qualified Data.IntMap as IM
import Data.Kind
import Data.Reify
import GHC.Float
import Janus.Backend.C
import Janus.Command.Ref
import Janus.Expression.Let
import Janus.Typed
import Numeric.AD.Mode.Reverse
import System.IO.Unsafe
import Data.Functor.Compose

data MathF a e = LitF a | UnopF (a -> a) e | BinopF (a -> a -> a) e e

newtype MathE a = MathE {getMathE :: Fix (MathF a)}

instance MuRef (MathE a) where
  type DeRef (MathE a) = MathF a
  mapDeRef _ (MathE (Fix (LitF a))) = pure (LitF a)
  mapDeRef f (MathE (Fix (UnopF g a))) = UnopF g <$> f (MathE a)
  mapDeRef f (MathE (Fix (BinopF g a b))) = BinopF g <$> f (MathE a) <*> f (MathE b)

matheUnop
  :: (a -> a) -> MathE a -> MathE a
matheUnop f a = MathE $ Fix $ UnopF f (getMathE a)

matheBinop
  :: (a -> a -> a) -> MathE a -> MathE a -> MathE a
matheBinop f a b = MathE $ Fix $ BinopF f (getMathE a) (getMathE b)

instance Num a => Num (MathE a) where
  fromInteger = MathE . Fix . LitF . fromInteger
  (+) = matheBinop (+)
  (-) = matheBinop (-)
  (*) = matheBinop (*)
  negate = matheUnop negate
  signum = matheUnop signum
  abs = matheUnop abs

instance Fractional a => Fractional (MathE a) where
  (/) = matheBinop (/)
  recip = matheUnop recip
  fromRational = MathE . Fix . LitF . fromRational

instance Floating a => Floating (MathE a) where
  pi = MathE (Fix (LitF pi))
  exp = matheUnop exp
  log = matheUnop log
  sqrt = matheUnop sqrt
  (**) = matheBinop (**)
  logBase = matheBinop logBase
  sin = matheUnop sin
  cos = matheUnop cos
  tan = matheUnop tan
  asin = matheUnop asin
  acos = matheUnop acos
  atan = matheUnop atan
  sinh = matheUnop sinh
  cosh = matheUnop cosh
  tanh = matheUnop tanh
  asinh = matheUnop asinh
  acosh = matheUnop acosh
  atanh = matheUnop atanh
  log1p = matheUnop log1p
  expm1 = matheUnop expm1
  log1pexp = matheUnop log1pexp
  log1mexp = matheUnop log1mexp

foo :: Floating a => a -> a -> a
foo x1 x2 = z*z + x*x
  where
    x = x2*sin x1
    y = x*cos x
    z = y*tan y

baz :: Floating a => a -> a -> [a]
baz a b = [y, z]
  where
    x = a*a + b
    y = foo x a
    z = foo x b

hehe  :: (MuRef a, Traversable t, Applicative f) =>
     (forall b. (MuRef b, DeRef a ~ DeRef b) => b -> f u)
     -> t a -> f (Compose t (DeRef a) u)
hehe f = fmap Compose . traverse (mapDeRef f)

letK :: (JanusTyped e a, ExpLet e) => e a -> StateT (IM.IntMap (e a)) (Cont (e a)) (e a)
letK a = lift $ ContT $ \k -> pure $ let_ a $ \a' -> runIdentity (k a')

newtype ADExpr e a = ADExpr {getADExpr :: e a}
  deriving newtype (Num, Fractional, Floating)

type instance JanusTyped (ADExpr e) = JanusTyped e

instance ExpLet e => ExpLet (ADExpr e) where
  let_ (ADExpr e) f = ADExpr $ let_ e (getADExpr . f . ADExpr)

class ExpShare e where
  type Shared e :: Type -> Type
  toShared :: e a -> Shared e a
  share :: JanusTyped e a => Shared e a -> e a
  shareM :: (JanusTyped e a, CmdRef m e, Traversable t) => t (Shared e a) -> m (t (e a))

instance ExpShare Identity where
  type Shared Identity = Identity
  toShared = id
  {-# INLINE toShared #-}
  share = id
  {-# INLINE share #-}
  shareM = pure
  {-# INLINE shareM #-}

shareMathE
  :: (JanusTyped e a, ExpLet e) =>
     MathE (e a) -> e a
shareMathE expr = runCont (runStateT (shareMathE' (IM.fromList g) top) IM.empty) fst
  where
    Graph g top = unsafePerformIO $ reifyGraph expr
    shareMathE' mg i = do
      me <- get
      case IM.lookup i me of
        Just e -> pure e
        Nothing -> case IM.lookup i mg of
          Just (LitF e) -> pure e
          Just (UnopF f a) -> do
            a' <- shareMathE' mg a
            b' <- letK (f a')
            modify $ IM.insert i b'
            pure b'
          Just (BinopF f a b) -> do
            a' <- shareMathE' mg a
            b' <- shareMathE' mg b
            c' <- letK (f a' b')
            modify $ IM.insert i c'
            pure c'
          Nothing -> error "share: the impossible happened"

shareMathEM :: (JanusTyped e a, CmdRef m e, Traversable t) => t (MathE (e a)) -> m (t (e a))
shareMathEM exprs = evalStateT (traverse shareGraph graphs) IM.empty
  where
    graphs = unsafePerformIO $ reifyGraphs exprs
    shareGraph (Graph g e) = shareMathEM' (IM.fromList g) e
    shareMathEM' mg i = do
      me <- get
      case IM.lookup i me of
        Just e -> pure e
        Nothing -> case IM.lookup i mg of
          Just (LitF e) -> pure e
          Just (UnopF f a) -> do
            a' <- shareMathEM' mg a
            b' <- lift $ letM (f a')
            modify $ IM.insert i b'
            pure b'
          Just (BinopF f a b) -> do
            a' <- shareMathEM' mg a
            b' <- shareMathEM' mg b
            c' <- lift $ letM (f a' b')
            modify $ IM.insert i c'
            pure c'
          Nothing -> error "share: the impossible happened"

newtype MathExp e a = MathExp {getMathExp :: e a}
  deriving newtype (Num, Fractional, Floating)

type instance JanusTyped (MathExp e) = JanusTyped e

instance ExpLet e => ExpLet (MathExp e) where
  let_ x f = MathExp $ let_ (getMathExp x) (getMathExp . f . MathExp)

newtype MathExpM m a = MathExpM {getMathExpM :: m a}
  deriving newtype (Functor, Applicative, Monad)

newtype MathExpRef m e a = MathExpRef {getMathExpRef :: Ref m e a }

instance CmdRef m e => CmdRef (MathExpM m) (MathExp e) where
  type Ref (MathExpM m) (MathExp e) a = MathExpRef m e a
  newRef (MathExp a) = MathExpM $ MathExpRef <$> newRef a
  newRef' = MathExpM $ MathExpRef <$> newRef'
  readRef (MathExpRef r) = MathExpM $ MathExp <$> readRef r
  writeRef (MathExpRef r) (MathExp e) = MathExpM $ writeRef r e
  letM (MathExp e) = MathExpM $ MathExp <$> letM e

instance ExpShare JanusC where
  type Shared JanusC = Compose MathE JanusC
  toShared e = Compose $ MathE $ Fix $ LitF e
  share (Compose e) = shareMathE e
  shareM ts = shareMathEM (fmap getCompose ts)

newtype ADExp e a = ADExp {getADExp :: Shared e a}

deriving instance Num (Shared e a) => Num (ADExp e a)
deriving instance Fractional (Shared e a) => Fractional (ADExp e a)
deriving instance Floating (Shared e a) => Floating (ADExp e a)
