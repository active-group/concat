{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- TEMP
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Automatic differentiation
module ConCat.ADFun where

-- import Debug.Trace (trace)

-- hiding (linear)

-- hiding (linear)
import ConCat.AdditiveFun
import ConCat.AltCat
-- The following imports allows the instances to type-check. Why?
import qualified ConCat.Category as C
import ConCat.Free.LinearRow
import ConCat.Free.VectorSpace (HasV (..), IsScalar, inV)
import ConCat.GAD
import ConCat.Misc (R, Yes1, oops, result, sqr, unzip, (:*), type (&+&))
import ConCat.Rep (repr)
import Control.Newtype.Generics (Newtype (..))
import Data.Constraint hiding ((&&&), (***), (:=>))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))
import Data.Key (Zip (..))
import Data.Pointed (Pointed)
import GHC.Generics (Par1)
import Prelude hiding (const, curry, id, uncurry, unzip, zip, (.))
import qualified Prelude as P

-- Differentiable functions
type D = GD (-+>)

#if 0
instance ClosedCat D where
  apply = D (\ (f,a) -> (f a, \ (df,da) -> df a ^+^ deriv f a da))
  curry (D h) = D (\ a -> (curry f a, \ da -> \ b -> f' (a,b) (da,zero)))
   where
     (f,f') = unfork h
  {-# INLINE apply #-}
  {-# INLINE curry #-}

-- TODO: generalize to ClosedCat k for an arbitrary CCC k. I guess I can simply
-- apply ccc to the lambda expressions.
#elif 0

instance ClosedCat D where
  apply = applyD ; {-# INLINE apply #-}
  curry = curryD ; {-# INLINE curry #-}

applyD :: forall a b. Ok2 D a b => D ((a -> b) :* a) b
-- applyD = D (\ (f,a) -> (f a, \ (df,da) -> df a ^+^ f da))
applyD = -- trace "calling applyD" $
 D (\ (f,a) -> let (b,f') = andDerF f a in (b, \ (df,da) -> df a ^+^ f' da))
-- applyD = oops "applyD called"   -- does it?

curryD :: forall a b c. Ok3 D a b c => D (a :* b) c -> D a (b -> c)
curryD (D (unfork -> (f,f'))) =
  D (\ a -> (curry f a, \ da -> \ b -> f' (a,b) (da,zero)))

{-# INLINE applyD #-}
{-# INLINE curryD #-}
#else
-- No ClosedCat D instance
#endif

{--------------------------------------------------------------------
    Differentiation interface
--------------------------------------------------------------------}

andDerF :: forall a b. (a -> b) -> (a -> b :* (a -> b))
andDerF f = unMkD (toCcc @D f)
{-# INLINE andDerF #-}

andDerF' :: forall a b. (a -> b) -> (a -> b :* (a -> b))
andDerF' f = unMkD (toCcc' @D f)
{-# INLINE andDerF' #-}

-- Type specialization of deriv
derF :: forall a b. (a -> b) -> (a -> (a -> b))
derF = (result P.. result) snd andDerF
-- derF f = \ a -> snd (andDerF f a)
{-# INLINE derF #-}

-- AD with derivative-as-function, then converted to linear map
andDerFL :: forall s a b. HasLin s a b => (a -> b) -> (a -> b :* L s a b)
andDerFL f = second ConCat.Free.LinearRow.linear . andDerF f
{-# INLINE andDerFL #-}

-- AD with derivative-as-function, then converted to linear map
derFL :: forall s a b. HasLin s a b => (a -> b) -> (a -> L s a b)
derFL f = ConCat.Free.LinearRow.linear . derF f
{-# INLINE derFL #-}

dualV :: forall s a. (HasLin s a s, IsScalar s) => (a -> s) -> a
dualV h = unV (unpack (unpack (ConCat.Free.LinearRow.linear @s h)))
{-# INLINE dualV #-}

--                                h    :: a -> s
--                      linear @s h    :: L s a s
--              unpack (linear @s h)   :: V s s (V s a s)
--                                     :: Par1 (V s a s)
--      unpack (unpack (linear @s h))  :: V s a s
-- unV (unpack (unpack (linear @s h))) :: a

andGradFL :: (HasLin s a s, IsScalar s) => (a -> s) -> (a -> s :* a)
andGradFL f = second dualV . andDerF f
{-# INLINE andGradFL #-}

gradF :: (HasLin s a s, IsScalar s) => (a -> s) -> (a -> a)
gradF f = dualV . derF f
{-# INLINE gradF #-}

-- NOTE: gradF is fairly expensive due to linear (via dualV). For efficiency,
-- use GD (Dual AdditiveFun) instead.

#if 1

{--------------------------------------------------------------------
    Conversion to linear map. Replace HasL in LinearRow and LinearCol
--------------------------------------------------------------------}

linear1 :: (Representable f, Eq (Rep f), Num s)
        => (f s -> s) -> f s
-- linear1 = (<$> diag 0 1)
linear1 = (`fmapC` diag 0 1)
{-# INLINE linear1 #-}

linearN :: (Representable f, Eq (Rep f), Distributive g, Num s)
        => (f s -> g s) -> (f :-* g) s
linearN h = linear1 <$> distribute h
{-# INLINE linearN #-}

-- h :: f s -> g s
-- distribute h :: g (f s -> s)
-- linear1 <$> distribute h :: g (f s)

{--------------------------------------------------------------------
    Alternative definitions using Representable
--------------------------------------------------------------------}

type RepresentableV s a = (HasV s a, Representable (V s a))
type RepresentableVE s a = (RepresentableV s a, Eq (Rep (V s a)))
type HasLinR s a b = (RepresentableVE s a, RepresentableV s b, Num s)

linearNR :: ( HasV s a, RepresentableVE s a
            , HasV s b, Distributive (V s b), Num s )
         => (a -> b) -> L s a b
linearNR h = pack (linear1 <$> distribute (inV h))
{-# INLINE linearNR #-}

-- AD with derivative-as-function, then converted to linear map
andDerFLR :: forall s a b. HasLinR s a b => (a -> b) -> (a -> b :* L s a b)
andDerFLR f = second linearNR . andDerF f
{-# INLINE andDerFLR #-}

-- AD with derivative-as-function, then converted to linear map
derFLR :: forall s a b. HasLinR s a b => (a -> b) -> (a -> L s a b)
derFLR f = linearNR . derF f
{-# INLINE derFLR #-}

dualVR :: forall s a. (HasV s a, RepresentableVE s a, IsScalar s, Num s)
      => (a -> s) -> a
dualVR h = unV (linear1 (unpack . inV @s h))
{-# INLINE dualVR #-}

--                            h   :: a -> s
--                        inV h   :: V s a s -> V s s s
--                                :: V s a s -> Par1 s
--              (unpack . inV h)  :: V s a s -> s
--      linear1 (unpack . inV h)  :: V s a s
-- unV (linear1 (unpack . inV h)) :: a

andGradFLR :: forall s a. (IsScalar s, RepresentableVE s a, Num s)
           => (a -> s) -> (a -> s :* a)
andGradFLR f = second dualVR . andDerF f
{-# INLINE andGradFLR #-}

gradFR :: forall s a. (IsScalar s, RepresentableVE s a, Num s)
       => (a -> s) -> (a -> a)
gradFR f = dualVR . derF f
{-# INLINE gradFR #-}

#endif
