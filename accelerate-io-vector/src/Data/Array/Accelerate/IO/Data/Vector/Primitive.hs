{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Primitive
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Efficient conversion between 'Data.Vector.Primitive' vectors and Accelerate
-- 'Array's.
--

module Data.Array.Accelerate.IO.Data.Vector.Primitive (

  Vectors,
  toVectors,
  fromVectors,

) where

import Data.Vector.Primitive

import Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal

import Data.Array.Accelerate.Array.Data                             ( ArrayData, GArrayDataR, ScalarArrayDataR )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array                            hiding ( Vector )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R


-- | A family of types which represent a collection of Primitive Vectors. The
-- structure of the collection depends on the element type @e@ of the
-- corresponding Accelerate array.
--
type Vectors e = GArrayDataR Vector e


-- | /O(n)/ (typically). Convert a collection of primitive vectors into an
-- Accelerate array.
--
-- If the underlying vectors are pinned then this can be done without.
--
-- See also: <https://ghc.haskell.org/trac/ghc/ticket/5556>
--
-- @since 0.1.0.0@
--
{-# INLINE fromVectors #-}
fromVectors :: forall sh e. (HasCallStack, Shape sh, Elt e) => sh -> Vectors (EltR e) -> Array sh e
fromVectors sh vecs = Array (R.Array (fromElt sh) (aux (eltR @e) vecs))
  where
    aux :: TypeR a -> Vectors a -> ArrayData a
    aux TupRunit           ()       = ()
    aux (TupRpair aR1 aR2) (a1, a2) = (aux aR1 a1, aux aR2 a2)
    aux (TupRsingle aR)    a        = scalar aR a

    scalar :: ScalarType a -> Vectors a -> ArrayData a
    scalar (SingleScalarType t) a = single t a 1
    scalar (VectorScalarType t) a = vector t a

    vector :: VectorType a -> Vectors a -> ArrayData a
    vector (VectorType w t) a
      | SingleArrayDict <- singleArrayDict t
      = single t a w

    single :: SingleType a -> Vectors a -> Int -> ArrayData a
    single (NumSingleType t) = num t

    num :: NumType a -> Vectors a -> Int -> ArrayData a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> Vectors a -> Int -> ArrayData a
    integral TypeInt    = wrap
    integral TypeInt8   = wrap
    integral TypeInt16  = wrap
    integral TypeInt32  = wrap
    integral TypeInt64  = wrap
    integral TypeWord   = wrap
    integral TypeWord8  = wrap
    integral TypeWord16 = wrap
    integral TypeWord32 = wrap
    integral TypeWord64 = wrap

    floating :: FloatingType a -> Vectors a -> Int -> ArrayData a
    floating TypeHalf   = wrap
    floating TypeFloat  = wrap
    floating TypeDouble = wrap

    wrap :: Prim a => Vector a -> Int -> UniqueArray a
    wrap v@(Vector _ l _) w
      = boundsCheck "shape mismatch" (w * size sh == l)
      $ uniqueArrayOfVector v


-- | /O(1)/ (typically). Convert an Accelerate array into a collection of
-- primitive vectors.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- @since 0.1.0.0@
--
{-# INLINE toVectors #-}
toVectors :: forall sh e. (HasCallStack, Shape sh, Elt e) => Array sh e -> Vectors (EltR e)
toVectors (Array (R.Array sh adata)) = aux (eltR @e) adata
  where
    aux :: TypeR a -> ArrayData a -> Vectors a
    aux TupRunit           ()       = ()
    aux (TupRpair aR1 aR2) (a1, a2) = (aux aR1 a1, aux aR2 a2)
    aux (TupRsingle aR)    a        = scalar aR a

    scalar :: ScalarType a -> ArrayData a -> Vectors a
    scalar (SingleScalarType t) a = single t a 1
    scalar (VectorScalarType t) a = vector t a

    vector :: VectorType a -> ArrayData a -> Vectors a
    vector (VectorType w t) a
      | SingleArrayDict <- singleArrayDict t
      = single t a w

    single :: SingleType a -> ArrayData a -> Int -> Vectors a
    single (NumSingleType t) = num t

    num :: NumType a -> ArrayData a -> Int -> Vectors a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> ArrayData a -> Int -> Vectors a
    integral TypeInt    = wrap
    integral TypeInt8   = wrap
    integral TypeInt16  = wrap
    integral TypeInt32  = wrap
    integral TypeInt64  = wrap
    integral TypeWord   = wrap
    integral TypeWord8  = wrap
    integral TypeWord16 = wrap
    integral TypeWord32 = wrap
    integral TypeWord64 = wrap

    floating :: FloatingType a -> ArrayData a -> Int -> Vectors a
    floating TypeHalf   = wrap
    floating TypeFloat  = wrap
    floating TypeDouble = wrap

    n :: Int
    n = R.size (shapeR @sh) sh

    wrap :: Prim a => UniqueArray a -> Int -> Vector a
    wrap ua w = vectorOfUniqueArray (w * n) ua


data SingleArrayDict a where
  SingleArrayDict :: ( GArrayDataR Vector a ~ Vector (ScalarArrayDataR a)
                     , GArrayDataR UniqueArray a ~ UniqueArray (ScalarArrayDataR a)
                     , ScalarArrayDataR a ~ a )
                  => SingleArrayDict a

singleArrayDict :: SingleType a -> SingleArrayDict a
singleArrayDict = single
  where
    single :: SingleType a -> SingleArrayDict a
    single (NumSingleType t) = num t

    num :: NumType a -> SingleArrayDict a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> SingleArrayDict a
    integral TypeInt    = SingleArrayDict
    integral TypeInt8   = SingleArrayDict
    integral TypeInt16  = SingleArrayDict
    integral TypeInt32  = SingleArrayDict
    integral TypeInt64  = SingleArrayDict
    integral TypeWord   = SingleArrayDict
    integral TypeWord8  = SingleArrayDict
    integral TypeWord16 = SingleArrayDict
    integral TypeWord32 = SingleArrayDict
    integral TypeWord64 = SingleArrayDict

    floating :: FloatingType a -> SingleArrayDict a
    floating TypeHalf   = SingleArrayDict
    floating TypeFloat  = SingleArrayDict
    floating TypeDouble = SingleArrayDict

