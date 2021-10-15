{-# LANGUAGE TypeOperators #-}

module ConCat.MatrixMap (linearC, linear, bump, bumpC, tanhC, fmapC, maxC) where

import qualified ConCat.Category as C

linear :: C.MatrixMap m => m s -> C.Dim1 m s -> C.Dim2 m s
linear = C.linear
{-# INLINE [0] linear #-}

linearC :: (C.MatrixMapCat k m, C.Ok k s) => m s -> C.Dim1 m s `k` C.Dim2 m s
linearC = C.linearC
{-# INLINE [0] linearC #-}

bump :: (Num s, C.Bump b) => b s -> C.BumpRep b s
bump = C.bump
{-# INLINE [0] bump #-}

bumpC :: (C.BumpCat k b, C.Ok k s) => b s `k` C.BumpRep b s
bumpC = C.bumpC
{-# INLINE [0] bumpC #-}

tanhC :: (C.FloatingCat k b, C.Ok k b) => b `k` b
tanhC = C.tanhC
{-# INLINE [0] tanhC #-}

fmapC :: (C.FunctorCat k h, C.Ok2 k a b) => a `k` b -> h a `k` h b
fmapC = C.fmapC
{-# INLINE [0] fmapC #-}

maxC :: (C.MinMaxCat k a, C.Ok k a) => (a, a) `k` a
maxC = C.maxC
{-# INLINE [0] maxC #-}