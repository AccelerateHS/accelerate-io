{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
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

import GHC.Base
import GHC.TypeLits


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

type instance Vectors ()        = ()
type instance Vectors Int       = Vector Int
type instance Vectors Int8      = Vector Int8
type instance Vectors Int16     = Vector Int16
type instance Vectors Int32     = Vector Int32
type instance Vectors Int64     = Vector Int64
type instance Vectors Word      = Vector Word
type instance Vectors Word8     = Vector Word8
type instance Vectors Word16    = Vector Word16
type instance Vectors Word32    = Vector Word32
type instance Vectors Word64    = Vector Word64
type instance Vectors Half      = Vector Half
type instance Vectors Float     = Vector Float
type instance Vectors Double    = Vector Double
type instance Vectors Bool      = Vector Word8
type instance Vectors Char      = Vector Char
type instance Vectors (Vec n a) = Vectors a
type instance Vectors (a,b)     = (Vectors a, Vectors b)


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
    aux ArrayEltRunit           _       !_ = AD_Unit
    aux ArrayEltRint            v       !s = wrap AD_Int v s
    aux ArrayEltRint8           v       !s = wrap AD_Int8 v s
    aux ArrayEltRint16          v       !s = wrap AD_Int16 v s
    aux ArrayEltRint32          v       !s = wrap AD_Int32 v s
    aux ArrayEltRint64          v       !s = wrap AD_Int64 v s
    aux ArrayEltRword           v       !s = wrap AD_Word v s
    aux ArrayEltRword8          v       !s = wrap AD_Word8 v s
    aux ArrayEltRword16         v       !s = wrap AD_Word16 v s
    aux ArrayEltRword32         v       !s = wrap AD_Word32 v s
    aux ArrayEltRword64         v       !s = wrap AD_Word64 v s
    aux ArrayEltRhalf           v       !s = wrap AD_Half v s
    aux ArrayEltRfloat          v       !s = wrap AD_Float v s
    aux ArrayEltRdouble         v       !s = wrap AD_Double v s
    aux ArrayEltRbool           v       !s = wrap AD_Bool v s
    aux ArrayEltRchar           v       !s = wrap AD_Char v s
    aux aeR@(ArrayEltRvec ae)   v       !s = let !n@(I# n#) = width aeR in AD_Vec n# (aux ae v (n*s))
    aux (ArrayEltRpair ae1 ae2) (v1,v2) !s = AD_Pair (aux ae1 v1 s) (aux ae2 v2 s)

    {-# INLINE width #-}
    width :: forall n a. KnownNat n => ArrayEltR (Vec n a) -> Int
    width _ = fromInteger (natVal' (proxy# :: Proxy# n))


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
    aux ArrayEltRunit           AD_Unit         !_ = ()
    aux ArrayEltRint            (AD_Int v)      !s = wrap v s
    aux ArrayEltRint8           (AD_Int8 v)     !s = wrap v s
    aux ArrayEltRint16          (AD_Int16 v)    !s = wrap v s
    aux ArrayEltRint32          (AD_Int32 v)    !s = wrap v s
    aux ArrayEltRint64          (AD_Int64 v)    !s = wrap v s
    aux ArrayEltRword           (AD_Word v)     !s = wrap v s
    aux ArrayEltRword8          (AD_Word8 v)    !s = wrap v s
    aux ArrayEltRword16         (AD_Word16 v)   !s = wrap v s
    aux ArrayEltRword32         (AD_Word32 v)   !s = wrap v s
    aux ArrayEltRword64         (AD_Word64 v)   !s = wrap v s
    aux ArrayEltRhalf           (AD_Half v)     !s = wrap v s
    aux ArrayEltRfloat          (AD_Float v)    !s = wrap v s
    aux ArrayEltRdouble         (AD_Double v)   !s = wrap v s
    aux ArrayEltRbool           (AD_Bool v)     !s = wrap v s
    aux ArrayEltRchar           (AD_Char v)     !s = wrap v s
    aux (ArrayEltRvec ae)       (AD_Vec n# v)   !s = aux ae v (s * I# n#)
    aux (ArrayEltRpair ae1 ae2) (AD_Pair v1 v2) !s = (aux ae1 v1 s, aux ae2 v2 s)

