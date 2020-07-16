{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.ByteString
-- Copyright   : [2010..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Conversion between strict 'ByteString's and Accelerate 'Array's.
--

module Data.Array.Accelerate.IO.Data.ByteString
  where

import Data.Array.Accelerate.Array.Data                             -- ( ArrayData, GArrayDataR, ScalarArrayDataR )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R

import Data.Primitive.Vec

import Data.ByteString                                              as B
import Data.ByteString.Internal                                     as B
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

#if !MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr
#endif


-- | A family of types that represents a collection of 'ByteString's. The
-- structure of the collection depends on the element type @e@.
--
type family ByteStrings e where
  ByteStrings ()        = ()
  ByteStrings Int       = ByteString
  ByteStrings Int8      = ByteString
  ByteStrings Int16     = ByteString
  ByteStrings Int32     = ByteString
  ByteStrings Int64     = ByteString
  ByteStrings Word      = ByteString
  ByteStrings Word8     = ByteString
  ByteStrings Word16    = ByteString
  ByteStrings Word32    = ByteString
  ByteStrings Word64    = ByteString
  ByteStrings Half      = ByteString
  ByteStrings Float     = ByteString
  ByteStrings Double    = ByteString
  ByteStrings Bool      = ByteString
  ByteStrings Char      = ByteString
  ByteStrings (Vec n a) = ByteStrings a
  ByteStrings (a,b)     = (ByteStrings a, ByteStrings b)


-- | /O(1)/. Treat a set of strict 'ByteStrings' as an Accelerate array. The
-- type of the elements @e@ in the output Accelerate array determines the
-- structure of the collection.
--
-- Data is considered to be in row-major order. You must ensure that each input
-- contains the right number of bytes (this is not checked).
--
-- The input data may not be modified through the 'ByteString's afterwards.
--
-- @since 0.1.0.0@
--
{-# INLINE fromByteStrings #-}
fromByteStrings :: forall sh e. (Shape sh, Elt e) => sh -> ByteStrings (EltR e) -> Array sh e
fromByteStrings sh bs = Array (R.Array (fromElt sh) (tuple (eltR @e) bs))
  where
    wrap :: ByteString -> UniqueArray a
    wrap (B.toForeignPtr -> (ps,s,_)) =
      unsafePerformIO $ newUniqueArray (castForeignPtr (plusForeignPtr ps s))

    tuple :: TypeR a -> ByteStrings a -> ArrayData a
    tuple TupRunit           ()       = ()
    tuple (TupRpair aR1 aR2) (a1, a2) = (tuple aR1 a1, tuple aR2 a2)
    tuple (TupRsingle t)     a        = scalar t a

    scalar :: ScalarType a -> ByteStrings a -> ArrayData a
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType t) = vector t

    vector :: VectorType a -> ByteStrings a -> ArrayData a
    vector (VectorType _ t)
      | SingleArrayDict <- singleArrayDict t
      = single t

    single :: SingleType a -> ByteStrings a -> ArrayData a
    single (NumSingleType t) = num t

    num :: NumType a -> ByteStrings a -> ArrayData a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> ByteStrings a -> ArrayData a
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

    floating :: FloatingType a -> ByteStrings a -> ArrayData a
    floating TypeHalf   = wrap
    floating TypeFloat  = wrap
    floating TypeDouble = wrap


-- | /O(1)/. Convert an Accelerate 'Array' into a collection of strict
-- 'ByteStrings'. The element type @e@ will determine the structure of the
-- output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 0.1.0.0@
--
{-# INLINE toByteStrings #-}
toByteStrings :: forall sh e. (Shape sh, Elt e) => Array sh e -> ByteStrings (EltR e)
toByteStrings (Array (R.Array sh adata)) = tuple (eltR @e) adata
  where
    wrap :: forall a. Storable a => UniqueArray a -> Int -> ByteString
    wrap (unsafeGetValue . uniqueArrayData -> fp) k =
      B.fromForeignPtr (castForeignPtr fp) 0 (R.size (shapeR @sh) sh * k * sizeOf (undefined::a))

    tuple :: TypeR a -> ArrayData a -> ByteStrings a
    tuple TupRunit           ()       = ()
    tuple (TupRpair aR1 aR2) (a1, a2) = (tuple aR1 a1, tuple aR2 a2)
    tuple (TupRsingle t)     a        = scalar t a 1

    scalar :: ScalarType a -> ArrayData a -> Int -> ByteStrings a
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType t) = vector t

    vector :: VectorType a -> ArrayData a -> Int -> ByteStrings a
    vector (VectorType w t) a _
      | SingleArrayDict <- singleArrayDict t
      = single t a w

    single :: SingleType a -> ArrayData a -> Int -> ByteStrings a
    single (NumSingleType t) = num t

    num :: NumType a -> ArrayData a -> Int -> ByteStrings a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> ArrayData a -> Int -> ByteStrings a
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

    floating :: FloatingType a -> ArrayData a -> Int -> ByteStrings a
    floating TypeHalf   = wrap
    floating TypeFloat  = wrap
    floating TypeDouble = wrap

#if !MIN_VERSION_base(4,10,0)
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr# c) (I# d#) = ForeignPtr (plusAddr# addr# d#) c
#endif

