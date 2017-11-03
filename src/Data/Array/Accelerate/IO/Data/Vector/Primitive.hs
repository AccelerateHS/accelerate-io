{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Primitive
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            hiding ( Vector )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Int
import Data.Word
import System.IO.Unsafe

-- | A family of types which represent a collection of Primitive Vectors. The
-- structure of the collection depends on the element type @e@ of the
-- corresponding Accelerate array.
--
type family Vectors e :: *

type instance Vectors ()     = ()
type instance Vectors Int    = Vector Int
type instance Vectors Int8   = Vector Int8
type instance Vectors Int16  = Vector Int16
type instance Vectors Int32  = Vector Int32
type instance Vectors Int64  = Vector Int64
type instance Vectors Word   = Vector Word
type instance Vectors Word8  = Vector Word8
type instance Vectors Word16 = Vector Word16
type instance Vectors Word32 = Vector Word32
type instance Vectors Word64 = Vector Word64
type instance Vectors Float  = Vector Float
type instance Vectors Double = Vector Double
type instance Vectors Char   = Vector Char
type instance Vectors (a,b)  = (Vectors a, Vectors b)


-- | /O(n)/ (typically). Convert a collection of primitive vectors into an
-- Accelerate array.
--
-- If the underlying vectors are pinned then this can be done without.
--
-- @since 1.1.0.0@
--
fromVectors :: (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array (fromElt sh) (aux arrayElt vecs)
  where
    wrap :: Prim a => Vector a -> UniqueArray a
    wrap v@(Vector _ l _)
      = $boundsCheck "fromVectors" "shape mismatch" (size sh == l)
      $ unsafePerformIO (uniqueArrayOfVector v)

    aux :: ArrayEltR e -> Vectors e -> ArrayData e
    aux ArrayEltRunit           _       = AD_Unit
    aux ArrayEltRint            v       = AD_Int    (wrap v)
    aux ArrayEltRint8           v       = AD_Int8   (wrap v)
    aux ArrayEltRint16          v       = AD_Int16  (wrap v)
    aux ArrayEltRint32          v       = AD_Int32  (wrap v)
    aux ArrayEltRint64          v       = AD_Int64  (wrap v)
    aux ArrayEltRword           v       = AD_Word   (wrap v)
    aux ArrayEltRword8          v       = AD_Word8  (wrap v)
    aux ArrayEltRword16         v       = AD_Word16 (wrap v)
    aux ArrayEltRword32         v       = AD_Word32 (wrap v)
    aux ArrayEltRword64         v       = AD_Word64 (wrap v)
    aux ArrayEltRchar           v       = AD_Char   (wrap v)
    aux ArrayEltRfloat          v       = AD_Float  (wrap v)
    aux ArrayEltRdouble         v       = AD_Double (wrap v)
    aux (ArrayEltRpair ad1 ad2) (v1,v2) = AD_Pair   (aux ad1 v1) (aux ad2 v2)
    --
    aux _ _ = $internalError "fromVectors" "unsupported type"


-- | /O(1)/ (typically). Convert an Accelerate array into a collection of
-- primitive vectors.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- @since 1.1.0.0@
--
toVectors :: (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array sh adata) = aux arrayElt adata
  where
    n :: Int
    n = R.size sh

    wrap :: Prim a => UniqueArray a -> Vector a
    wrap ua = unsafePerformIO (vectorOfUniqueArray n ua)

    aux :: ArrayEltR e -> ArrayData e -> Vectors e
    aux ArrayEltRunit           AD_Unit         = ()
    aux ArrayEltRint            (AD_Int v)      = wrap v
    aux ArrayEltRint8           (AD_Int8 v)     = wrap v
    aux ArrayEltRint16          (AD_Int16 v)    = wrap v
    aux ArrayEltRint32          (AD_Int32 v)    = wrap v
    aux ArrayEltRint64          (AD_Int64 v)    = wrap v
    aux ArrayEltRword           (AD_Word v)     = wrap v
    aux ArrayEltRword8          (AD_Word8 v)    = wrap v
    aux ArrayEltRword16         (AD_Word16 v)   = wrap v
    aux ArrayEltRword32         (AD_Word32 v)   = wrap v
    aux ArrayEltRword64         (AD_Word64 v)   = wrap v
    aux ArrayEltRchar           (AD_Char v)     = wrap v
    aux ArrayEltRfloat          (AD_Float v)    = wrap v
    aux ArrayEltRdouble         (AD_Double v)   = wrap v
    aux (ArrayEltRpair ad1 ad2) (AD_Pair v1 v2) = (aux ad1 v1, aux ad2 v2)
    --
    aux _ _ = $internalError "toVectors" "unsupported type"

