{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Vector
-- Copyright   : [2012] Adam C. Foltzer
--               [2012..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Helpers for fast conversion between 'Data.Vector.Storable' vectors into
-- Accelerate arrays.
--

module Data.Array.Accelerate.IO.Vector (

  -- ** Data.Vector.Storable
  --
  -- | This provides an efficient non-copying conversion between storable
  -- vectors and Accelerate arrays.
  --
  Vectors, toVectors, fromVectors,

) where

-- standard libraries
import Data.Int
import Data.Word
import Foreign.C.Types
import Data.Vector.Storable
import Data.Array.Storable.Internals                            ( StorableArray(..) )
import System.IO.Unsafe

-- friends
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                        hiding ( Vector )


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
type instance Vectors CChar   = Vector HTYPE_CCHAR
type instance Vectors CSChar  = Vector Int8
type instance Vectors CUChar  = Vector Word8
type instance Vectors (a,b)   = (Vectors a, Vectors b)


-- | /O(1)/. Treat a set of storable vectors as Accelerate arrays. The type of
-- elements @e@ in the output Accelerate array determines the structure  of the
-- collection that will be required as the second argument. See 'Vectors'.
--
-- Data will be consumed from the vector in row-major order. You must make sure
-- that each of the input vectors contains the right number of elements
--
fromVectors :: (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array (fromElt sh) (aux arrayElt vecs)
  where
    wrap k v = let (p,n) = unsafeToForeignPtr0 v
               in  k (unsafePerformIO $ uniqueFromStorable $ StorableArray 0 n n p)

    aux :: ArrayEltR e -> Vectors e -> ArrayData e
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
    aux ArrayEltRfloat          = wrap AD_Float
    aux ArrayEltRdouble         = wrap AD_Double
    aux ArrayEltRcfloat         = wrap AD_CFloat
    aux ArrayEltRcdouble        = wrap AD_CDouble
    aux ArrayEltRbool           = wrap AD_Bool
    aux ArrayEltRchar           = wrap AD_Char
    aux ArrayEltRcchar          = wrap AD_CChar
    aux ArrayEltRcschar         = wrap AD_CSChar
    aux ArrayEltRcuchar         = wrap AD_CUChar
    aux (ArrayEltRpair ae1 ae2) = \(v1,v2) -> AD_Pair (aux ae1 v1) (aux ae2 v2)


-- | /O(1)/. Turn the Accelerate array into a collection of storable 'Vector's.
-- The element type of the array @e@ will determine the structure of the output
-- collection. See 'Vectors'.
--
-- Data will be output in row-major order.
--
toVectors :: (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array _ adata) = aux arrayElt adata
  where
    wrap (storableFromUnique -> StorableArray _ _ n p) = unsafeFromForeignPtr0 p n

    aux :: ArrayEltR e -> ArrayData e -> Vectors e
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
    aux ArrayEltRfloat          (AD_Float s)    = wrap s
    aux ArrayEltRdouble         (AD_Double s)   = wrap s
    aux ArrayEltRcfloat         (AD_CFloat s)   = wrap s
    aux ArrayEltRcdouble        (AD_CDouble s)  = wrap s
    aux ArrayEltRbool           (AD_Bool s)     = wrap s
    aux ArrayEltRchar           (AD_Char s)     = wrap s
    aux ArrayEltRcchar          (AD_CChar s)    = wrap s
    aux ArrayEltRcschar         (AD_CSChar s)   = wrap s
    aux ArrayEltRcuchar         (AD_CUChar s)   = wrap s
    aux (ArrayEltRpair ae1 ae2) (AD_Pair s1 s2) = (aux ae1 s1, aux ae2 s2)

