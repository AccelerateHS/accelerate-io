{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Array.Unboxed
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Efficient conversion between immutable unboxed 'IArray's and Accelerate
-- 'Array's.
--

module Data.Array.Accelerate.IO.Data.Array.Unboxed (

  IxShapeRepr,
  fromUArray,
  toUArray,

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R

import Data.Array.Accelerate.IO.Data.Array.Internal
import Data.Array.Accelerate.IO.Data.Primitive.ByteArray

import Data.Primitive                                               ( Prim, sizeOf )
import Data.Primitive.ByteArray

import Data.Array.Base
import Data.Array.Unboxed                                           as U hiding ( Array )
import System.IO.Unsafe


-- | /O(n)/. Convert an unboxed 'UArray' into an Accelerate array.
--
-- See 'Data.Array.Accelerate.IO.Data.Array.IArray.fromIArray' for more
-- information about the array index type.
--
-- If the underlying vectors are pinned then this can be done without copying.
--
-- See also: <https://ghc.haskell.org/trac/ghc/ticket/5556>
--
-- @since 0.1.0.0@
--
{-# INLINE fromUArray #-}
fromUArray
    :: forall ix sh e. (HasCallStack, IxShapeRepr (EltR ix) ~ EltR sh, IArray UArray e, Ix ix, Shape sh, Elt ix, Elt e)
    => UArray ix e
    -> Array sh e
fromUArray (UArray lo hi n ba#) = Array (R.Array (fromElt sh) (tuple (eltR @e)))
  where
    sh = rangeToShape (toIxShapeRepr lo, toIxShapeRepr hi) :: sh

    wrap :: forall a. Prim a => UniqueArray a
    wrap = unsafePerformIO (newUniqueArray =<< foreignPtrOfByteArray 0 (n * sizeOf (undefined::a)) (ByteArray ba#))

    tuple :: TypeR a -> ArrayData a
    tuple TupRunit           = ()
    tuple (TupRpair aR1 aR2) = (tuple aR1, tuple aR2)
    tuple (TupRsingle t)     = scalar t

    scalar :: ScalarType t -> ArrayData t
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType _) = internalError "unsupported type"

    single :: SingleType t -> ArrayData t
    single (NumSingleType t) = num t

    num :: NumType t -> ArrayData t
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> ArrayData t
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

    floating :: FloatingType t -> ArrayData t
    floating TypeHalf   = wrap
    floating TypeFloat  = wrap
    floating TypeDouble = wrap


-- | /O(1)/ (typically). Convert an Accelerate 'Array' to an unboxed 'UArray'.
--
-- See 'Data.Array.Accelerate.IO.Data.Array.IArray.fromIArray' for more
-- information about the array index type.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- @since 0.1.0.0@
--
{-# INLINE toUArray #-}
toUArray
    :: forall ix sh e. (HasCallStack, IxShapeRepr (EltR ix) ~ EltR sh, IArray UArray e, Ix ix, Shape sh, Elt e, Elt ix)
    => Maybe ix         -- ^ if 'Just' this is the index lower bound, otherwise the array is indexed from zero
    -> Array sh e
    -> UArray ix e
toUArray mix0 arr@(Array (R.Array _ adata)) =
  case ba of
    ByteArray ba# -> UArray lo hi n ba#
  where
    n       = size (shape arr)
    bnds    = shapeToRange (shape arr)
    lo      = fromIxShapeRepr (offset (fst bnds))
    hi      = fromIxShapeRepr (offset (snd bnds))
    ba      = tuple (eltR @e) adata

    offset :: sh -> sh
    offset ix =
      case mix0 of
        Nothing  -> ix
        Just ix0 -> offset' ix0 ix

    offset' :: ix -> sh -> sh
    offset' ix0 = toElt . go (eltR @sh) (fromElt (toIxShapeRepr ix0 :: sh)) . fromElt
      where
        go :: TypeR sh' -> sh' -> sh' -> sh'
        go TupRunit                                                                    ()       ()    = ()
        go (TupRpair tl tr)                                                            (l0, r0) (l,r) = (go tl l0 l, go tr r0 r)
        go (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) i0       i     = i0+i
        go _ _ _ =
          internalError "error in index offset"

    wrap :: forall a. Prim a => UniqueArray a -> ByteArray
    wrap ua = unsafePerformIO $ byteArrayOfForeignPtr (n * sizeOf (undefined::a)) (unsafeGetValue (uniqueArrayData ua))

    tuple :: TypeR a -> ArrayData a -> ByteArray
    tuple (TupRsingle t) = scalar t
    tuple _              = internalError "unsupported type"

    scalar :: ScalarType t -> ArrayData t -> ByteArray
    scalar (SingleScalarType t) = single t
    scalar (VectorScalarType _) = internalError "unsupported type"

    single :: SingleType t -> ArrayData t -> ByteArray
    single (NumSingleType t) = num t

    num :: NumType t -> ArrayData t -> ByteArray
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> ArrayData t -> ByteArray
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

    floating :: FloatingType t -> ArrayData t -> ByteArray
    floating TypeHalf   = wrap
    floating TypeFloat  = wrap
    floating TypeDouble = wrap

