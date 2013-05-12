{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Vector
-- Copyright   : [2012] Adam C. Foltzer, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Helpers for fast conversion of 'Data.Vector.Storable' vectors into
-- Accelerate arrays.
--

module Data.Array.Accelerate.IO.Vector (

  -- ** Data.Vector.Storable
  Vectors, toVectors, fromVectors,

) where

-- standard libraries
import Data.Int
import Data.Word
import Data.Vector.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import System.IO.Unsafe
import GHC.Base                                 ( Int(..), Int# )
import Data.Array.Base                          ( wORD_SCALE, fLOAT_SCALE, dOUBLE_SCALE )

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar        hiding ( Vector )
import Data.Array.Accelerate.IO.BlockCopy


-- | A family of types that represents a collection of storable 'Vector's. The
-- structure of the collection depends on the element type @e@.
--
-- For example:
--
--   * if @e :: Int@,             then @Vectors (EltRepr e) :: ((), Vector Int)@
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
type instance Vectors Float   = Vector Float
type instance Vectors CFloat  = Vector Float
type instance Vectors Double  = Vector Double
type instance Vectors CDouble = Vector Double
type instance Vectors Bool    = Vector Word8
type instance Vectors Char    = Vector Char
type instance Vectors CChar   = Vector Int8
type instance Vectors CSChar  = Vector Int8
type instance Vectors CUChar  = Vector Word8
type instance Vectors (a,b)   = (Vectors a, Vectors b)


-- | /O(n)/. Copy a set of storable vectors into freshly allocated Accelerate
-- arrays. The type of elements @e@ in the output Accelerate array determines
-- the structure of the collection that will be required as the second argument.
-- See 'Vectors'.
--
-- Data will be consumed from the vector in row-major order. You must make sure
-- that each of the input vectors contains the right number of elements.
--
fromVectors :: (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = unsafePerformIO $! do
  aux arrayElt adata vecs
  return array
  where
    sizeA                       = size sh
    array@(Array _ adata)       = allocateArray sh
    base accPtr bytes vec       = unsafeWith vec $ \vecPtr -> blockCopy vecPtr accPtr bytes

    aux :: ArrayEltR e -> ArrayData e -> Vectors e -> IO ()
    aux ArrayEltRunit    _  = return
    aux ArrayEltRint     ad = base (ptrsOfArrayData ad) (box wORD_SCALE sizeA)
    aux ArrayEltRint8    ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRint16   ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRint32   ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRint64   ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRword    ad = base (ptrsOfArrayData ad) (box wORD_SCALE sizeA)
    aux ArrayEltRword8   ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRword16  ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRword32  ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRword64  ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRcshort  ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRcushort ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRcint    ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRcuint   ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRclong   ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRculong  ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRcllong  ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRcullong ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRfloat   ad = base (ptrsOfArrayData ad) (box fLOAT_SCALE sizeA)
    aux ArrayEltRcfloat  ad = base (ptrsOfArrayData ad) (box fLOAT_SCALE sizeA)
    aux ArrayEltRdouble  ad = base (ptrsOfArrayData ad) (box dOUBLE_SCALE sizeA)
    aux ArrayEltRcdouble ad = base (ptrsOfArrayData ad) (box dOUBLE_SCALE sizeA)
    aux ArrayEltRbool    ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRchar    ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRcchar   ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRcschar  ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRcuchar  ad = base (ptrsOfArrayData ad) sizeA
    aux (ArrayEltRpair ae1 ae2) (AD_Pair ad1 ad2) = \(v1, v2) -> do
      aux ae1 ad1 v1
      aux ae2 ad2 v2


-- | /O(n)/. Turn the Accelerate array into a collection of storable 'Vector's.
-- The element type of the array @e@ will determine the structure of the output
-- collection. See 'Vectors'.
--
-- Data will be output in row-major order.
--
toVectors :: (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors array@(Array _ adata) = unsafePerformIO $! aux arrayElt adata
  where
    sizeA       = size (shape array)

    base :: Storable a => Ptr a -> Int -> IO (Vector a)
    base accPtr bytes = do
      fp     <- mallocForeignPtrBytes bytes
      withForeignPtr fp $ \vecPtr -> blockCopy accPtr vecPtr bytes
      return $! unsafeFromForeignPtr0 fp sizeA

    aux :: ArrayEltR e -> ArrayData e -> IO (Vectors e)
    aux ArrayEltRunit    _  = return ()
    aux ArrayEltRint     ad = base (ptrsOfArrayData ad) (box wORD_SCALE sizeA)
    aux ArrayEltRint8    ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRint16   ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRint32   ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRint64   ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRword    ad = base (ptrsOfArrayData ad) (box wORD_SCALE sizeA)
    aux ArrayEltRword8   ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRword16  ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRword32  ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRword64  ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRcshort  ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRcushort ad = base (ptrsOfArrayData ad) (sizeA * 2)
    aux ArrayEltRcint    ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRcuint   ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRclong   ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRculong  ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRcllong  ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRcullong ad = base (ptrsOfArrayData ad) (sizeA * 8)
    aux ArrayEltRfloat   ad = base (ptrsOfArrayData ad) (box fLOAT_SCALE sizeA)
    aux ArrayEltRcfloat  ad = base (ptrsOfArrayData ad) (box fLOAT_SCALE sizeA)
    aux ArrayEltRdouble  ad = base (ptrsOfArrayData ad) (box dOUBLE_SCALE sizeA)
    aux ArrayEltRcdouble ad = base (ptrsOfArrayData ad) (box dOUBLE_SCALE sizeA)
    aux ArrayEltRbool    ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRchar    ad = base (ptrsOfArrayData ad) (sizeA * 4)
    aux ArrayEltRcchar   ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRcschar  ad = base (ptrsOfArrayData ad) sizeA
    aux ArrayEltRcuchar  ad = base (ptrsOfArrayData ad) sizeA
    aux (ArrayEltRpair ae1 ae2) (AD_Pair ad1 ad2) =
      do v1 <- aux ae1 ad1
         v2 <- aux ae2 ad2
         return (v1, v2)


-- Helpers
--
box :: (Int# -> Int#) -> Int -> Int
box f (I# x) = I# (f x)

