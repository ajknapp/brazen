{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Brazen.Pairrow where

import Control.Arrow
-- import Control.Arrow.Transformer.Reader as AR
-- import Control.Arrow.Transformer.State as AS
import Control.Category
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont
import Control.Monad.Reader as MR
import Control.Monad.State
import Prelude hiding ((.),id)

data Pairrow f g a b = Pairrow (f a b) (g a b)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Applicative (f a), Applicative (g a)) => Applicative (Pairrow f g a) where
  pure f = Pairrow (pure f) (pure f)
  Pairrow f g <*> Pairrow x y = Pairrow (f <*> x) (g <*> y)

instance (Category f, Category g) => Category (Pairrow f g) where
  id = Pairrow id id
  Pairrow f g . Pairrow f' g' = Pairrow (f . f') (g . g')

instance (Arrow f, Arrow g) => Arrow (Pairrow f g) where
  arr f = Pairrow (arr f) (arr f)
  first (Pairrow f g) = Pairrow (first f) (first g)

runPairrowFst :: Pairrow f g a b -> f a b
runPairrowFst (Pairrow f _) = f

runPairrowSnd :: Pairrow f g a b -> g a b
runPairrowSnd (Pairrow _ g) = g

-- idx :: Monad m => Dingas m () Int
-- idx = Dingas $ \_ (Foo f m) -> Foo (\i -> f i >> pure i) m

-- sync :: Monad m => Dingas m () ()
-- sync = Dingas $ \n (Foo f m) -> Foo (const (pure ())) (m >> forM_ [0..n] f)

-- foo :: Show a => Dingas IO a ()
-- foo = Dingas $ \_ (Foo f m) -> Foo (f >=> print) m

-- blah :: Dingas IO () ()
-- blah = proc _ -> do
--   i <- idx -< ()
--   foo -< i
--   sync -< ()
--   foo -< i
