{-# LANGUAGE TypeOperators #-}

module ConCat.MatrixMap (linearC, linear, bump, bumpC) where

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
