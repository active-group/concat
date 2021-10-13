{-# LANGUAGE TypeOperators #-}

module ConCat.MatrixMap (linearC, linear) where

import qualified ConCat.Category as C

linear :: C.MatrixMap m => C.Dim1 m s -> m s -> C.Dim2 m s
linear = C.linear
{-# INLINE [0] linear #-}

linearC :: (C.MatrixMapCat k m, C.Ok k s) => C.Dim1 m s -> m s `k` C.Dim2 m s
linearC = C.linearC
{-# INLINE [0] linearC #-}
