{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Brazen.FieldNames where

import Brazen.AD
import Data.HKD
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Janus.Command.Array

data FieldName e a where
  FieldVar :: String -> FieldName e (Var e a)
  FieldTensor :: String -> TensorBound sh e -> FieldName e (Tensor sh e a)

deriving instance Show (FieldName e a)

fieldName :: FieldName e a -> String
fieldName (FieldVar s) = s
fieldName (FieldTensor s _) = s

class GFieldNames f where
  gfieldNames :: String -> Proxy (f a) -> f a

instance (GFieldNames f, GFieldNames g) => GFieldNames (f :*: g) where
  gfieldNames prefix _ = gfieldNames prefix Proxy :*: gfieldNames prefix Proxy

instance (FFunctor f, FieldNames e f) => GFieldNames (K1 i (f (FieldName e))) where
  gfieldNames prefix _ =
    K1 $
      ffmap
        ( \case
            FieldVar s -> FieldVar (prepend s)
            FieldTensor s t -> FieldTensor (prepend s) t
        )
        (fieldNames :: f (FieldName e))
    where
      prepend s = prefix <> "_" <> s

instance GFieldNames (K1 i (FieldName e (Var e a))) where
  gfieldNames prefix _ = K1 $ FieldVar prefix

instance (ReifyTensorBound sh) => GFieldNames (K1 i (FieldName e (Tensor sh e a))) where
  gfieldNames prefix _ = K1 $ FieldTensor prefix (tensorBound (Proxy @sh))

instance (GFieldNames f) => GFieldNames (D1 i f) where
  gfieldNames prefix _ = M1 $ gfieldNames prefix Proxy

instance (GFieldNames f) => GFieldNames (C1 i f) where
  gfieldNames prefix _ = M1 $ gfieldNames prefix Proxy

instance (KnownSymbol n, GFieldNames f) => GFieldNames (S1 ('MetaSel ('Just n) u s d) f) where
  gfieldNames prefix p = M1 $ gfieldNames (prefix <> fname) (p' p)
    where
      p' :: Proxy (S1 i f a) -> Proxy (f a)
      p' _ = Proxy
      fname = case symbolVal (Proxy @n) of
        '_' : xs -> xs
        xs -> xs

class FieldNames e f where
  fieldNames :: f (FieldName e)
  default fieldNames :: (Generic (f (FieldName e)), GFieldNames (Rep (f (FieldName e)))) => f (FieldName e)
  fieldNames = GHC.Generics.to $ gfieldNames "" (Proxy @(Rep (f (FieldName e)) ()))
