{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Storable
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

import Data.Array.Accelerate.IO.Data.Patterns

-- standard libraries
import Data.Vector.Storable
import System.IO.Unsafe

import GHC.Base


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
fromVectors :: forall sh e. (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array $ R.Array (fromElt sh) (aux (eltType @e) vecs 1)
  where
    {-# INLINE wrap #-}
    wrap :: Storable a => Vector a -> Int -> UniqueArray a
    wrap v s
      = $boundsCheck "fromVectors" "shape mismatch" (vsize `quot` s == size sh)
      $ unsafePerformIO $ newUniqueArray fp
      where
        (fp,vsize) = unsafeToForeignPtr0 v

    {-# INLINE aux #-}
    aux :: TupleType a -> Vectors a -> Int -> ArrayData a
    aux TupRunit             () !_ = ()
    aux TupInt               v  !s = wrap v s
    aux TupInt8              v  !s = wrap v s
    aux TupInt16             v  !s = wrap v s
    aux TupInt32             v  !s = wrap v s
    aux TupInt64             v  !s = wrap v s
    aux TupWord              v  !s = wrap v s
    aux TupWord8             v  !s = wrap v s
    aux TupWord16            v  !s = wrap v s
    aux TupWord32            v  !s = wrap v s
    aux TupWord64            v  !s = wrap v s
    aux TupHalf              v  !s = wrap v s
    aux TupFloat             v  !s = wrap v s
    aux TupDouble            v  !s = wrap v s
    aux TupBool              v  !s = wrap v s
    aux TupChar              v  !s = wrap v s
    aux (TupVecInt n# tp)    v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecInt8 n# tp)   v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecInt16 n# tp)  v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecInt32 n# tp)  v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecInt64 n# tp)  v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecWord n# tp)   v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecWord8 n# tp)  v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecWord16 n# tp) v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecWord32 n# tp) v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecWord64 n# tp) v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecHalf n# tp)   v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecFloat n# tp)  v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecDouble n# tp) v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecBool n# tp)   v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupVecChar n# tp)   v  !s = aux (TupRsingle $ SingleScalarType tp) v (I# n# * s)
    aux (TupRpair t1 t2) (v1, v2) !s = (aux t1 v1 s, aux t2 v2 s)


-- | /O(1)/. Turn the Accelerate array into a collection of storable 'Vector's.
-- The element type of the array @e@ will determine the structure of the output
-- collection. See 'Vectors'.
--
-- Data will be output in row-major order.
--
{-# INLINE toVectors #-}
toVectors :: forall sh e. (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array (R.Array sh adata)) = aux (eltType @e) adata 1
  where
    {-# INLINE wrap #-}
    wrap :: Storable a => UniqueArray a -> Int -> Vector a
    wrap ua k = unsafeFromForeignPtr0 (unsafeGetValue (uniqueArrayData ua))
                                      (R.size (shapeR @sh) sh * k)

    {-# INLINE aux #-}
    aux :: TupleType a -> ArrayData a -> Int -> Vectors a
    aux TupRunit             ()  !_ = ()
    aux TupInt               arr !s = wrap arr s
    aux TupInt8              arr !s = wrap arr s
    aux TupInt16             arr !s = wrap arr s
    aux TupInt32             arr !s = wrap arr s
    aux TupInt64             arr !s = wrap arr s
    aux TupWord              arr !s = wrap arr s
    aux TupWord8             arr !s = wrap arr s
    aux TupWord16            arr !s = wrap arr s
    aux TupWord32            arr !s = wrap arr s
    aux TupWord64            arr !s = wrap arr s
    aux TupHalf              arr !s = wrap arr s
    aux TupFloat             arr !s = wrap arr s
    aux TupDouble            arr !s = wrap arr s
    aux TupBool              arr !s = wrap arr s
    aux TupChar              arr !s = wrap arr s
    aux (TupVecInt n# tp)    arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecInt8 n# tp)   arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecInt16 n# tp)  arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecInt32 n# tp)  arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecInt64 n# tp)  arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecWord n# tp)   arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecWord8 n# tp)  arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecWord16 n# tp) arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecWord32 n# tp) arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecWord64 n# tp) arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecHalf n# tp)   arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecFloat n# tp)  arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecDouble n# tp) arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecBool n# tp)   arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupVecChar n# tp)   arr !s = aux (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
    aux (TupRpair t1 t2) (a1, a2) !s = (aux t1 a1 s, aux t2 a2 s)
