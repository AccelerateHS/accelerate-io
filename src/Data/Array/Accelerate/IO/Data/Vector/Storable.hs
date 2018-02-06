{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Storable
-- Copyright   : [2012] Adam C. Foltzer
--               [2012..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            hiding ( Vector )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Representation         as R

-- standard libraries
import Data.Vector.Storable
import System.IO.Unsafe


-- | A family of types that represents a collection of storable 'Vector's. The
-- structure of the collection depends on the element type @e@.
--
-- For example:
--
--   * if @e :: Int@,             then @Vectors (EltRepr e) :: Vector Int@
--
--   * if @e :: (Double, Float)@, then @Vectors (EltRepr e) :: (((), Vector Double), Vector Float)@
--
type family Vectors e

type instance Vectors ()      = ()
type instance Vectors Int     = Vector Int
type instance Vectors Int8    = Vector Int8
type instance Vectors Int16   = Vector Int16
type instance Vectors Int32   = Vector Int32
type instance Vectors Int64   = Vector Int64
type instance Vectors Word    = Vector Word
type instance Vectors Word8   = Vector Word8
type instance Vectors Word16  = Vector Word16
type instance Vectors Word32  = Vector Word32
type instance Vectors Word64  = Vector Word64
type instance Vectors CShort  = Vector Int16
type instance Vectors CUShort = Vector Word16
type instance Vectors CInt    = Vector Int32
type instance Vectors CUInt   = Vector Word32
type instance Vectors CLong   = Vector HTYPE_LONG
type instance Vectors CULong  = Vector HTYPE_UNSIGNED_LONG
type instance Vectors CLLong  = Vector Int64
type instance Vectors CULLong = Vector Word64
type instance Vectors Half    = Vector Half
type instance Vectors Float   = Vector Float
type instance Vectors CFloat  = Vector Float
type instance Vectors Double  = Vector Double
type instance Vectors CDouble = Vector Double
type instance Vectors Bool    = Vector Word8
type instance Vectors Char    = Vector Char
type instance Vectors CChar   = Vector HTYPE_CCHAR
type instance Vectors CSChar  = Vector Int8
type instance Vectors CUChar  = Vector Word8
type instance Vectors (V2 a)  = Vectors a
type instance Vectors (V3 a)  = Vectors a
type instance Vectors (V4 a)  = Vectors a
type instance Vectors (V8 a)  = Vectors a
type instance Vectors (V16 a) = Vectors a
type instance Vectors (a,b)   = (Vectors a, Vectors b)


-- | /O(1)/. Treat a set of storable vectors as Accelerate arrays. The type of
-- elements @e@ in the output Accelerate array determines the structure  of the
-- collection that will be required as the second argument. See 'Vectors'.
--
-- Data will be consumed from the vector in row-major order. You must make sure
-- that each of the input vectors contains the right number of elements
--
{-# INLINE fromVectors #-}
fromVectors :: (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array (fromElt sh) (aux arrayElt vecs 1)
  where
    {-# INLINE wrap #-}
    wrap :: Storable e => (UniqueArray e -> a) -> Vector e -> Int -> a
    wrap k v s
      = $boundsCheck "fromVectors" "shape mismatch" (vsize `quot` s == size sh)
      $ k (unsafePerformIO $ newUniqueArray fp)
      where
        (fp,vsize) = unsafeToForeignPtr0 v

    {-# INLINE aux #-}
    aux :: ArrayEltR e -> Vectors e -> Int -> ArrayData e
    aux ArrayEltRunit           _       _ = AD_Unit
    aux ArrayEltRint            v       s = wrap AD_Int v s
    aux ArrayEltRint8           v       s = wrap AD_Int8 v s
    aux ArrayEltRint16          v       s = wrap AD_Int16 v s
    aux ArrayEltRint32          v       s = wrap AD_Int32 v s
    aux ArrayEltRint64          v       s = wrap AD_Int64 v s
    aux ArrayEltRword           v       s = wrap AD_Word v s
    aux ArrayEltRword8          v       s = wrap AD_Word8 v s
    aux ArrayEltRword16         v       s = wrap AD_Word16 v s
    aux ArrayEltRword32         v       s = wrap AD_Word32 v s
    aux ArrayEltRword64         v       s = wrap AD_Word64 v s
    aux ArrayEltRcshort         v       s = wrap AD_CShort v s
    aux ArrayEltRcushort        v       s = wrap AD_CUShort v s
    aux ArrayEltRcint           v       s = wrap AD_CInt v s
    aux ArrayEltRcuint          v       s = wrap AD_CUInt v s
    aux ArrayEltRclong          v       s = wrap AD_CLong v s
    aux ArrayEltRculong         v       s = wrap AD_CULong v s
    aux ArrayEltRcllong         v       s = wrap AD_CLLong v s
    aux ArrayEltRcullong        v       s = wrap AD_CULLong v s
    aux ArrayEltRhalf           v       s = wrap AD_Half v s
    aux ArrayEltRfloat          v       s = wrap AD_Float v s
    aux ArrayEltRdouble         v       s = wrap AD_Double v s
    aux ArrayEltRcfloat         v       s = wrap AD_CFloat v s
    aux ArrayEltRcdouble        v       s = wrap AD_CDouble v s
    aux ArrayEltRbool           v       s = wrap AD_Bool v s
    aux ArrayEltRchar           v       s = wrap AD_Char v s
    aux ArrayEltRcchar          v       s = wrap AD_CChar v s
    aux ArrayEltRcschar         v       s = wrap AD_CSChar v s
    aux ArrayEltRcuchar         v       s = wrap AD_CUChar v s
    aux (ArrayEltRvec2 ae)      v       s = AD_V2 (aux ae v (s*2))
    aux (ArrayEltRvec3 ae)      v       s = AD_V3 (aux ae v (s*3))
    aux (ArrayEltRvec4 ae)      v       s = AD_V4 (aux ae v (s*4))
    aux (ArrayEltRvec8 ae)      v       s = AD_V8 (aux ae v (s*8))
    aux (ArrayEltRvec16 ae)     v       s = AD_V16 (aux ae v (s*16))
    aux (ArrayEltRpair ae1 ae2) (v1,v2) s = AD_Pair (aux ae1 v1 s) (aux ae2 v2 s)


-- | /O(1)/. Turn the Accelerate array into a collection of storable 'Vector's.
-- The element type of the array @e@ will determine the structure of the output
-- collection. See 'Vectors'.
--
-- Data will be output in row-major order.
--
{-# INLINE toVectors #-}
toVectors :: (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array sh adata) = aux arrayElt adata 1
  where
    {-# INLINE wrap #-}
    wrap :: Storable a => UniqueArray a -> Int -> Vector a
    wrap ua k = unsafeFromForeignPtr0 (unsafeGetValue (uniqueArrayData ua)) (R.size sh * k)

    {-# INLINE aux #-}
    aux :: ArrayEltR e -> ArrayData e -> Int -> Vectors e
    aux ArrayEltRunit           AD_Unit         _ = ()
    aux ArrayEltRint            (AD_Int s)      k = wrap s k
    aux ArrayEltRint8           (AD_Int8 s)     k = wrap s k
    aux ArrayEltRint16          (AD_Int16 s)    k = wrap s k
    aux ArrayEltRint32          (AD_Int32 s)    k = wrap s k
    aux ArrayEltRint64          (AD_Int64 s)    k = wrap s k
    aux ArrayEltRword           (AD_Word s)     k = wrap s k
    aux ArrayEltRword8          (AD_Word8 s)    k = wrap s k
    aux ArrayEltRword16         (AD_Word16 s)   k = wrap s k
    aux ArrayEltRword32         (AD_Word32 s)   k = wrap s k
    aux ArrayEltRword64         (AD_Word64 s)   k = wrap s k
    aux ArrayEltRcshort         (AD_CShort s)   k = wrap s k
    aux ArrayEltRcushort        (AD_CUShort s)  k = wrap s k
    aux ArrayEltRcint           (AD_CInt s)     k = wrap s k
    aux ArrayEltRcuint          (AD_CUInt s)    k = wrap s k
    aux ArrayEltRclong          (AD_CLong s)    k = wrap s k
    aux ArrayEltRculong         (AD_CULong s)   k = wrap s k
    aux ArrayEltRcllong         (AD_CLLong s)   k = wrap s k
    aux ArrayEltRcullong        (AD_CULLong s)  k = wrap s k
    aux ArrayEltRhalf           (AD_Half s)     k = wrap s k
    aux ArrayEltRfloat          (AD_Float s)    k = wrap s k
    aux ArrayEltRdouble         (AD_Double s)   k = wrap s k
    aux ArrayEltRcfloat         (AD_CFloat s)   k = wrap s k
    aux ArrayEltRcdouble        (AD_CDouble s)  k = wrap s k
    aux ArrayEltRbool           (AD_Bool s)     k = wrap s k
    aux ArrayEltRchar           (AD_Char s)     k = wrap s k
    aux ArrayEltRcchar          (AD_CChar s)    k = wrap s k
    aux ArrayEltRcschar         (AD_CSChar s)   k = wrap s k
    aux ArrayEltRcuchar         (AD_CUChar s)   k = wrap s k
    aux (ArrayEltRvec2 ae)      (AD_V2 s)       k = aux ae s (k*2)
    aux (ArrayEltRvec3 ae)      (AD_V3 s)       k = aux ae s (k*3)
    aux (ArrayEltRvec4 ae)      (AD_V4 s)       k = aux ae s (k*4)
    aux (ArrayEltRvec8 ae)      (AD_V8 s)       k = aux ae s (k*8)
    aux (ArrayEltRvec16 ae)     (AD_V16 s)      k = aux ae s (k*16)
    aux (ArrayEltRpair ae1 ae2) (AD_Pair s1 s2) k = (aux ae1 s1 k, aux ae2 s2 k)

