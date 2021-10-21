{-# LANGUAGE TypeOperators #-}

module ConCat.MatrixMap (linear) where

import ConCat.Additive (Additive)
import qualified ConCat.Category as C
import qualified ConCat.Matrix as Matrix

linear :: (Matrix.MatrixMap m, Additive s, Num s) => m s -> Matrix.Dim1 m s -> Matrix.Dim2 m s
linear = Matrix.linear
{-# INLINE [0] linear #-}

-- linearC :: (C.MatrixMapCat k m, C.Ok k s, Additive s, Num s) => m s -> Matrix.Dim1 m s `k` Matrix.Dim2 m s
-- linearC = C.linearC
-- {-# INLINE [0] linearC #-}

-- bump :: (Num s,Matrix.Bump b) => b s -> Matrix.BumpRep b s
-- bump = Matrix.bump
-- {-# INLINE [0] bump #-}

-- bumpC :: (C.BumpCat k b, C.Ok k s, Num s) => b s `k` Matrix.BumpRep b s
-- bumpC = C.bumpC
-- {-# INLINE [0] bumpC #-}

-- tanhC :: (C.FloatingCat k b, C.Ok k b) => b `k` b
-- tanhC = C.tanhC
-- {-# INLINE [0] tanhC #-}

-- fmapC :: (C.FunctorCat k h, C.Ok2 k a b) => a `k` b -> h a `k` h b
-- fmapC = C.fmapC
-- {-# INLINE [0] fmapC #-}

-- maxC :: (C.MinMaxCat k a, C.Ok k a) => (a, a) `k` a
-- maxC = C.maxC
-- {-# INLINE [0] maxC #-}
