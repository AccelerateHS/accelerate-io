{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Foreign.Ptr
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Foreign.Ptr
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique

import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe


-- | A family of types which represent a collection of 'Ptr's. The
-- structure of the collection depends on the element type @e@.
--
type Ptrs e = ArrayPtrs e


-- | /O(1)/. Treat the set of 'Ptrs' as an Accelerate array. The type of
-- elements @e@ in the output Accelerate array determines the structure of the
-- collection.
--
-- Data is considered to be in row-major order. You must ensure that each of the
-- input pointers contains the right number of elements.
--
-- The data may not be modified through the 'Ptrs' afterwards.
--
-- You are responsible for ensuring that the data remains alive for the duration
-- of the Accelerate computation, and for freeing it afterwards.
--
-- You should make sure that the data is suitably aligned.
--
-- @since 1.1.0.0@
--
{-# INLINE fromPtrs #-}
fromPtrs :: (Shape sh, Elt e) => sh -> Ptrs (EltRepr e) -> Array sh e
fromPtrs sh ps = Array (fromElt sh) (aux arrayElt ps)
  where
    wrap :: (UniqueArray e -> r) -> Ptr e -> r
    wrap k p = k (unsafePerformIO $ newUniqueArray =<< newForeignPtr_ p)

    aux :: ArrayEltR e -> Ptrs e -> ArrayData e
    aux ArrayEltRunit           = const AD_Unit
    aux ArrayEltRint            = wrap AD_Int
    aux ArrayEltRint8           = wrap AD_Int8
    aux ArrayEltRint16          = wrap AD_Int16
    aux ArrayEltRint32          = wrap AD_Int32
    aux ArrayEltRint64          = wrap AD_Int64
    aux ArrayEltRword           = wrap AD_Word
    aux ArrayEltRword8          = wrap AD_Word8
    aux ArrayEltRword16         = wrap AD_Word16
    aux ArrayEltRword32         = wrap AD_Word32
    aux ArrayEltRword64         = wrap AD_Word64
    aux ArrayEltRcshort         = wrap AD_CShort
    aux ArrayEltRcushort        = wrap AD_CUShort
    aux ArrayEltRcint           = wrap AD_CInt
    aux ArrayEltRcuint          = wrap AD_CUInt
    aux ArrayEltRclong          = wrap AD_CLong
    aux ArrayEltRculong         = wrap AD_CULong
    aux ArrayEltRcllong         = wrap AD_CLLong
    aux ArrayEltRcullong        = wrap AD_CULLong
    aux ArrayEltRhalf           = wrap AD_Half
    aux ArrayEltRfloat          = wrap AD_Float
    aux ArrayEltRdouble         = wrap AD_Double
    aux ArrayEltRcfloat         = wrap AD_CFloat
    aux ArrayEltRcdouble        = wrap AD_CDouble
    aux ArrayEltRbool           = wrap AD_Bool
    aux ArrayEltRchar           = wrap AD_Char
    aux ArrayEltRcchar          = wrap AD_CChar
    aux ArrayEltRcschar         = wrap AD_CSChar
    aux ArrayEltRcuchar         = wrap AD_CUChar
    aux (ArrayEltRvec2 ae)      = AD_V2 . aux ae
    aux (ArrayEltRvec3 ae)      = AD_V3 . aux ae
    aux (ArrayEltRvec4 ae)      = AD_V4 . aux ae
    aux (ArrayEltRvec8 ae)      = AD_V8 . aux ae
    aux (ArrayEltRvec16 ae)     = AD_V16 . aux ae
    aux (ArrayEltRpair ae1 ae2) = \(v1,v2) -> AD_Pair (aux ae1 v1) (aux ae2 v2)


-- | /O(1)/. Yield the underlying 'Ptrs' backing the given Accelerate array. The
-- element type @e@ will determine the structure of the output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 1.1.0.0@
--
{-# INLINE toPtrs #-}
toPtrs :: (Shape sh, Elt e) => Array sh e -> Ptrs (EltRepr e)
toPtrs (Array _ adata) = ptrsOfArrayData adata

