{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Foreign.ForeignPtr
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Foreign.ForeignPtr
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Type

import Foreign.ForeignPtr
import System.IO.Unsafe


-- | A family of types which represent a collection of 'ForeignPtr's. The
-- structure of the collection depends on the element type @e@.
--
type family ForeignPtrs e

type instance ForeignPtrs ()      = ()
type instance ForeignPtrs Int     = ForeignPtr Int
type instance ForeignPtrs Int8    = ForeignPtr Int8
type instance ForeignPtrs Int16   = ForeignPtr Int16
type instance ForeignPtrs Int32   = ForeignPtr Int32
type instance ForeignPtrs Int64   = ForeignPtr Int64
type instance ForeignPtrs Word    = ForeignPtr Word
type instance ForeignPtrs Word8   = ForeignPtr Word8
type instance ForeignPtrs Word16  = ForeignPtr Word16
type instance ForeignPtrs Word32  = ForeignPtr Word32
type instance ForeignPtrs Word64  = ForeignPtr Word64
type instance ForeignPtrs CShort  = ForeignPtr Int16
type instance ForeignPtrs CUShort = ForeignPtr Word16
type instance ForeignPtrs CInt    = ForeignPtr Int32
type instance ForeignPtrs CUInt   = ForeignPtr Word32
type instance ForeignPtrs CLong   = ForeignPtr HTYPE_LONG
type instance ForeignPtrs CULong  = ForeignPtr HTYPE_UNSIGNED_LONG
type instance ForeignPtrs CLLong  = ForeignPtr Int64
type instance ForeignPtrs CULLong = ForeignPtr Word64
type instance ForeignPtrs Half    = ForeignPtr Half
type instance ForeignPtrs Float   = ForeignPtr Float
type instance ForeignPtrs Double  = ForeignPtr Double
type instance ForeignPtrs CFloat  = ForeignPtr Float
type instance ForeignPtrs CDouble = ForeignPtr Double
type instance ForeignPtrs Bool    = ForeignPtr Word8
type instance ForeignPtrs Char    = ForeignPtr Char
type instance ForeignPtrs CChar   = ForeignPtr HTYPE_CCHAR
type instance ForeignPtrs CSChar  = ForeignPtr Int8
type instance ForeignPtrs CUChar  = ForeignPtr Word8
type instance ForeignPtrs (V2 a)  = ForeignPtrs a
type instance ForeignPtrs (V3 a)  = ForeignPtrs a
type instance ForeignPtrs (V4 a)  = ForeignPtrs a
type instance ForeignPtrs (V8 a)  = ForeignPtrs a
type instance ForeignPtrs (V16 a) = ForeignPtrs a
type instance ForeignPtrs (a,b)   = (ForeignPtrs a, ForeignPtrs b)


-- | /O(1)/. Treat the set of 'ForeignPtrs' as an Accelerate array. The type of
-- elements @e@ in the output Accelerate array determines the structure of the
-- collection.
--
-- Data is considered to be in row-major order. You must ensure that each of the
-- input pointers contains the right number of elements.
--
-- The data may not be modified through the 'ForeignPtr's afterwards.
--
-- You should make sure that the data is suitably aligned.
--
-- @since 1.1.0.0@
--
{-# INLINE fromForeignPtrs #-}
fromForeignPtrs :: (Shape sh, Elt e) => sh -> ForeignPtrs (EltRepr e) -> Array sh e
fromForeignPtrs sh fps = Array (fromElt sh) (aux arrayElt fps)
  where
    wrap :: (UniqueArray e -> r) -> ForeignPtr e -> r
    wrap k fp = k (unsafePerformIO $ newUniqueArray fp)

    aux :: ArrayEltR e -> ForeignPtrs e -> ArrayData e
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


-- | /O(1)/. Yield the 'ForeignPtr's underlying the given Accelerate 'Array'.
-- The element type @e@ will determine the structure of the output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 1.1.0.0@
--
{-# INLINE toForeignPtrs #-}
toForeignPtrs :: (Shape sh, Elt e) => Array sh e -> ForeignPtrs (EltRepr e)
toForeignPtrs (Array _ adata) = aux arrayElt adata
  where
    wrap :: UniqueArray a -> ForeignPtr a
    wrap ua = unsafeGetValue (uniqueArrayData ua)

    aux :: ArrayEltR e -> ArrayData e -> ForeignPtrs e
    aux ArrayEltRunit           AD_Unit         = ()
    aux ArrayEltRint            (AD_Int s)      = wrap s
    aux ArrayEltRint8           (AD_Int8 s)     = wrap s
    aux ArrayEltRint16          (AD_Int16 s)    = wrap s
    aux ArrayEltRint32          (AD_Int32 s)    = wrap s
    aux ArrayEltRint64          (AD_Int64 s)    = wrap s
    aux ArrayEltRword           (AD_Word s)     = wrap s
    aux ArrayEltRword8          (AD_Word8 s)    = wrap s
    aux ArrayEltRword16         (AD_Word16 s)   = wrap s
    aux ArrayEltRword32         (AD_Word32 s)   = wrap s
    aux ArrayEltRword64         (AD_Word64 s)   = wrap s
    aux ArrayEltRcshort         (AD_CShort s)   = wrap s
    aux ArrayEltRcushort        (AD_CUShort s)  = wrap s
    aux ArrayEltRcint           (AD_CInt s)     = wrap s
    aux ArrayEltRcuint          (AD_CUInt s)    = wrap s
    aux ArrayEltRclong          (AD_CLong s)    = wrap s
    aux ArrayEltRculong         (AD_CULong s)   = wrap s
    aux ArrayEltRcllong         (AD_CLLong s)   = wrap s
    aux ArrayEltRcullong        (AD_CULLong s)  = wrap s
    aux ArrayEltRhalf           (AD_Half s)     = wrap s
    aux ArrayEltRfloat          (AD_Float s)    = wrap s
    aux ArrayEltRdouble         (AD_Double s)   = wrap s
    aux ArrayEltRcfloat         (AD_CFloat s)   = wrap s
    aux ArrayEltRcdouble        (AD_CDouble s)  = wrap s
    aux ArrayEltRbool           (AD_Bool s)     = wrap s
    aux ArrayEltRchar           (AD_Char s)     = wrap s
    aux ArrayEltRcchar          (AD_CChar s)    = wrap s
    aux ArrayEltRcschar         (AD_CSChar s)   = wrap s
    aux ArrayEltRcuchar         (AD_CUChar s)   = wrap s
    aux (ArrayEltRvec2 ae)      (AD_V2 s)       = aux ae s
    aux (ArrayEltRvec3 ae)      (AD_V3 s)       = aux ae s
    aux (ArrayEltRvec4 ae)      (AD_V4 s)       = aux ae s
    aux (ArrayEltRvec8 ae)      (AD_V8 s)       = aux ae s
    aux (ArrayEltRvec16 ae)     (AD_V16 s)      = aux ae s
    aux (ArrayEltRpair ae1 ae2) (AD_Pair s1 s2) = (aux ae1 s1, aux ae2 s2)

