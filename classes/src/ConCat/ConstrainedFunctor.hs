{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ConCat.ConstrainedFunctor where

import ConCat.Misc
import GHC.Types

type ConstrainedFunctor :: (Type -> Type) -> Constraint
class ConstrainedFunctor f where
  type Ok f :: Type -> Constraint
  type Ok f = Yes1
  cmap :: (Ok f a, Ok f b) => (a -> b) -> f a -> f b
  default cmap :: Functor f => (a -> b) -> f a -> f b
  cmap = fmap
