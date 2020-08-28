{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Storable
-- Copyright   : [2012..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Efficient non-copying conversion between 'Data.Vector.Storable' vectors and
-- Accelerate 'Array's.
--

module Data.Array.Accelerate.IO.Data.Vector.Storable (

  Vectors,
  toVectors,
  fromVectors,

) where

import Data.Array.Accelerate.Array.Data                             ( ArrayData, GArrayDataR, ScalarArrayDataR )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array                            hiding ( Vector )
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R

import Data.Vector.Storable
import System.IO.Unsafe


-- | A family of types that represents a collection of storable 'Vector's. The
-- structure of the collection depends on the element type @e@.
--
-- For example:
--
--   * if @e :: Int@,             then @Vectors (EltR e) :: Vector Int@
--
--   * if @e :: (Double, Float)@, then @Vectors (EltR e) :: (((), Vector Double), Vector Float)@
--
type Vectors e = GArrayDataR Vector e


-- | /O(1)/. Treat a set of storable vectors as Accelerate arrays. The type of
-- elements @e@ in the output Accelerate array determines the structure  of the
-- collection that will be required as the second argument. See 'Vectors'.
--
-- Data will be consumed from the vector in row-major order. You must make sure
-- that each of the input vectors contains the right number of elements
--
{-# INLINE fromVectors #-}
fromVectors :: forall sh e. (HasCallStack, Shape sh, Elt e) => sh -> Vectors (EltR e) -> Array sh e
fromVectors sh vecs = Array (R.Array (fromElt sh) (aux (eltR @e) vecs))
  where
    wrap :: Storable a => Vector a -> Int -> UniqueArray a
    wrap v w
      = boundsCheck "shape mismatch" (vsize `quot` w == size sh)
      $ unsafePerformIO $ newUniqueArray fp
      where
        (fp, vsize) = unsafeToForeignPtr0 v

    aux :: TypeR a -> Vectors a -> ArrayData a
    aux TupRunit           ()      = ()
    aux (TupRpair aR1 aR2) (a1,a2) = (aux aR1 a1, aux aR2 a2)
    aux (TupRsingle aR)    a       = scalar aR a

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


-- | /O(1)/. Turn the Accelerate array into a collection of storable 'Vector's.
-- The element type of the array @e@ will determine the structure of the output
-- collection. See 'Vectors'.
--
-- Data will be output in row-major order.
--
{-# INLINE toVectors #-}
toVectors :: forall sh e. (Shape sh, Elt e) => Array sh e -> Vectors (EltR e)
toVectors (Array (R.Array sh adata)) = aux (eltR @e) adata
  where
    wrap :: Storable a => UniqueArray a -> Int -> Vector a
    wrap ua w = unsafeFromForeignPtr0 (unsafeGetValue (uniqueArrayData ua)) (n * w)
    n         = R.size (shapeR @sh) sh

    aux :: TypeR a -> ArrayData a -> Vectors a
    aux TupRunit           ()      = ()
    aux (TupRpair aR1 aR2) (a1,a2) = (aux aR1 a1, aux aR2 a2)
    aux (TupRsingle aR)    a       = scalar aR a

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

