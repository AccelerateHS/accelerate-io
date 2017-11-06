{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Array.Unboxed
-- Copyright   : [2016..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Array.Accelerate.IO.Data.Array.Internal
import Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal

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
-- @since 1.1.0.0@
--
{-# INLINE fromUArray #-}
fromUArray
    :: forall ix sh e. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, IArray UArray e, Ix ix, Shape sh, Elt ix, Elt e)
    => UArray ix e
    -> Array sh e
fromUArray (UArray lo hi n ba#) = Array (fromElt sh) (aux (arrayElt :: ArrayEltR (EltRepr e)))
  where
    sh = rangeToShape (toIxShapeRepr lo, toIxShapeRepr hi) :: sh

    wrap :: forall a. Prim a => (UniqueArray a -> ArrayData a) -> ArrayData a
    wrap k = k $ unsafePerformIO (newUniqueArray =<< foreignPtrOfByteArray 0 (n * sizeOf (undefined::a)) (ByteArray ba#))

    aux :: ArrayEltR a -> ArrayData a
    aux ArrayEltRint    = wrap AD_Int
    aux ArrayEltRint8   = wrap AD_Int8
    aux ArrayEltRint16  = wrap AD_Int16
    aux ArrayEltRint32  = wrap AD_Int32
    aux ArrayEltRint64  = wrap AD_Int64
    aux ArrayEltRword   = wrap AD_Word
    aux ArrayEltRword8  = wrap AD_Word8
    aux ArrayEltRword16 = wrap AD_Word16
    aux ArrayEltRword32 = wrap AD_Word32
    aux ArrayEltRword64 = wrap AD_Word64
    aux ArrayEltRfloat  = wrap AD_Float
    aux ArrayEltRdouble = wrap AD_Double
    aux ArrayEltRchar   = wrap AD_Char
    aux ArrayEltRbool   = $internalError "fromUArray" "TODO: Bool"  -- need to unpack bit array
    aux _               = $internalError "fromUArray" "unsupported type"


-- | /O(1)/ (typically). Convert an Accelerate 'Array' to an unboxed 'UArray'.
--
-- See 'Data.Array.Accelerate.IO.Data.Array.IArray.fromIArray' for more
-- information about the array index type.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- @since 1.1.0.0@
--
{-# INLINE toUArray #-}
toUArray
    :: forall ix sh e. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, IArray UArray e, Ix ix, Shape sh, Elt ix)
    => ix           -- ^ index lower bound
    -> Array sh e
    -> UArray ix e
toUArray lo arr@(Array sh adata) =
  case ba of
    ByteArray ba# -> UArray lo hi n ba#
  where
    n       = R.size sh
    (_,u)   = shapeToRange (shape arr)
    hi      = fromIxShapeRepr (offset u)
    ba      = aux arrayElt adata

    offset :: sh -> sh
    offset = toElt . go (eltType (undefined::sh)) (fromElt (toIxShapeRepr lo :: sh)) . fromElt
      where
        go :: TupleType sh' -> sh' -> sh' -> sh'
        go UnitTuple                                                 ()       ()    = ()
        go (PairTuple tl tr)                                         (l0, r0) (l,r) = (go tl l0 l, go tr r0 r)
        go (SingleTuple (NumScalarType (IntegralNumType TypeInt{}))) i0       i     = i0+i
        go _ _ _
          = $internalError "toUArray" "error in index offset"

    wrap :: forall a. Prim a => UniqueArray a -> ByteArray
    wrap ua = unsafePerformIO $ byteArrayOfForeignPtr (n * sizeOf (undefined::a)) (unsafeGetValue (uniqueArrayData ua))

    aux :: ArrayEltR a -> ArrayData a -> ByteArray
    aux ArrayEltRint    (AD_Int v)    = wrap v
    aux ArrayEltRint8   (AD_Int8 v)   = wrap v
    aux ArrayEltRint16  (AD_Int16 v)  = wrap v
    aux ArrayEltRint32  (AD_Int32 v)  = wrap v
    aux ArrayEltRint64  (AD_Int64 v)  = wrap v
    aux ArrayEltRword   (AD_Word v)   = wrap v
    aux ArrayEltRword8  (AD_Word8 v)  = wrap v
    aux ArrayEltRword16 (AD_Word16 v) = wrap v
    aux ArrayEltRword32 (AD_Word32 v) = wrap v
    aux ArrayEltRword64 (AD_Word64 v) = wrap v
    aux ArrayEltRfloat  (AD_Float v)  = wrap v
    aux ArrayEltRdouble (AD_Double v) = wrap v
    aux ArrayEltRchar   (AD_Char v)   = wrap v
    aux ArrayEltRbool   (AD_Bool _)   = $internalError "toUArray" "TODO: Bool"  -- need to pack bit array
    aux _ _ = $internalError "toUArray" "unsupported type"

