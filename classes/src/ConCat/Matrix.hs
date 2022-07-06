{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ConCat.Matrix where

import qualified Data.Vector.Sized as Vector
import GHC.Generics ((:.:) (..))
import Data.Distributive (Distributive (..))
import Data.List.Extra (snoc)
import Data.Kind (Constraint, Type)
import qualified ConCat.Zip as Zip
import ConCat.Additive (Additive, sumA)
import GHC.TypeLits

class MatrixMap m where
  type Dim1 m :: Type -> Type
  type Dim2 m :: Type -> Type
  linearP :: (Additive s, Num s) => Dim1 m s -> m s -> Dim2 m s
  linearX :: (Additive s, Num s) => m s -> Dim1 m s -> Dim2 m s
  outerV :: Num s => Dim1 m s -> Dim2 m s -> m s

class Transpose m where
  type Transposed m :: Type -> Type
  transpose :: m s -> Transposed m s

class Bump b where
  type BumpRep b :: Type -> Type
  type BumpConstraint b :: Type -> Constraint
  bump :: BumpConstraint b s => b s -> BumpRep b s
  unbump :: BumpConstraint b s => BumpRep b s -> b s

instance (Foldable f, Zip.Zip f, Functor g) => MatrixMap (g :.: f) where
  type Dim1 (g :.: f) = f
  type Dim2 (g :.: f) = g
  linearP a (Comp1 ba) = (<.> a) <$> ba
  linearX (Comp1 ba) a = (<.> a) <$> ba
  outerV b a = Comp1 ((*^ b) <$> a)

(<.>) :: (Foldable a, Zip.Zip a, Additive s, Num s) => a s -> a s -> s
xs <.> ys = sumA (Zip.zipWith (*) xs ys)

(*^) :: (Functor a, Num s) => s -> a s -> a s
s *^ v = (s *) <$> v

instance (Distributive f, Functor g) => Transpose (g :.: f) where
  type Transposed (g :.: f) = f :.: g
  transpose = Comp1 . distribute . unComp1

instance {-# OVERLAPPING #-} Transpose ([] :.: []) where
  type Transposed ([] :.: []) = ([] :.: [])
  transpose (Comp1 [])             = Comp1 []
  transpose (Comp1 ([]   : xss))   = transpose (Comp1 xss)
  transpose (Comp1 ((x:xs) : xss)) = 
    Comp1 ((x : [h | (h:_) <- xss]) : unComp1 (transpose (Comp1 (xs : [ t | (_:t) <- xss]))))

instance Bump (Vector.Vector n) where
  type BumpRep (Vector.Vector n) = Vector.Vector (n + 1)
  type BumpConstraint (Vector.Vector n) = Num
  bump = (`Vector.snoc` 1)
  unbump = Vector.init

instance Bump [] where
  type BumpRep [] = []
  type BumpConstraint [] = Num
  bump = (`snoc` 1)
  unbump = init

class MatrixMap2 f2 f1 where
  type Matrix2 f2 f1 :: Type -> Type
  linearMat :: (Additive s, Num s) => f1 s -> Matrix2 f2 f1 s -> f2 s
  linearVec :: (Additive s, Num s) => Matrix2 f2 f1 s -> f1 s -> f2 s
  outerVec :: Num s => f1 s -> f2 s -> Matrix2 f2 f1 s

instance KnownNat n => MatrixMap2 (Vector.Vector m) (Vector.Vector n) where
  type Matrix2 (Vector.Vector m) (Vector.Vector n) = Vector.Vector m :.: Vector.Vector n
  linearMat a (Comp1 ba) = (<.> a) <$> ba
  linearVec (Comp1 ba) a = (<.> a) <$> ba
  outerVec b a = Comp1 ((*^ b) <$> a)