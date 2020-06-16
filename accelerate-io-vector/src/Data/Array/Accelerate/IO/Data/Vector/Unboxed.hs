{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Unboxed
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Efficient conversion between 'Data.Vector.Unboxed' vectors and Accelerate
-- 'Array's.
--

module Data.Array.Accelerate.IO.Data.Vector.Unboxed (

  Unbox(..),
  toUnboxed,
  fromUnboxed,

) where

import Data.Vector.Unboxed.Base                                     hiding ( Unbox )
import qualified Data.Vector.Unboxed                                as U

import Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            as A hiding ( Vector )
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Int
import Data.Word


-- | /O(n)/ (typically). Convert an Unboxed vector into an Accelerate array.
--
-- If the underlying vectors are pinned then this can be done without copying.
--
-- See also: <https://ghc.haskell.org/trac/ghc/ticket/5556>
--
-- @since 1.1.0.0@
--
{-# INLINE fromUnboxed #-}
fromUnboxed :: Unbox e => Vector e -> Array DIM1 e
fromUnboxed v = Array $ R.Array ((), U.length v) (arrayDataOfUnboxed v)


-- | /O(1)/ (typically). Convert an Accelerate array into an Unboxed vector.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying. The resulting vector will be pinned.
--
-- @since 1.1.0.0@
--
{-# INLINE toUnboxed #-}
toUnboxed :: forall sh e. (Shape sh, Unbox e) => Array sh e -> Vector e
toUnboxed (Array (R.Array sh adata)) = unboxedOfArrayData (R.size (shapeR @sh) sh) adata


-- Instances
-- ---------

class (U.Unbox e, A.Elt e) => Unbox e where
  arrayDataOfUnboxed :: U.Vector e -> ArrayData (EltRepr e)
  unboxedOfArrayData :: Int -> ArrayData (EltRepr e) -> U.Vector e

instance Unbox Int where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Int (vectorOfUniqueArray n arr)

instance Unbox Int8 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int8 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Int8 (vectorOfUniqueArray n arr)

instance Unbox Int16 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int16 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Int16 (vectorOfUniqueArray n arr)

instance Unbox Int32 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int32 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Int32 (vectorOfUniqueArray n arr)

instance Unbox Int64 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int64 v)  = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Int64 (vectorOfUniqueArray n arr)

instance Unbox Word where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Word (vectorOfUniqueArray n arr)

instance Unbox Word8 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word8 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Word8 (vectorOfUniqueArray n arr)

instance Unbox Word16 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word16 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Word16 (vectorOfUniqueArray n arr)

instance Unbox Word32 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word32 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Word32 (vectorOfUniqueArray n arr)

instance Unbox Word64 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word64 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Word64 (vectorOfUniqueArray n arr)

instance Unbox Float where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Float v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Float (vectorOfUniqueArray n arr)

instance Unbox Double where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Double v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Double (vectorOfUniqueArray n arr)

instance Unbox Char where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Char v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Char (vectorOfUniqueArray n arr)

instance Unbox Bool where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Bool v) = uniqueArrayOfVector v
  unboxedOfArrayData !n arr = V_Bool (vectorOfUniqueArray n arr)

instance Unbox () where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed V_Unit{} = ()
  unboxedOfArrayData !n () = V_Unit n

-- TODO: Are there any helpers for
instance (Unbox a, Unbox b) => Unbox (a, b) where
  {-# INLINE arrayDataOfUnboxed #-}
  arrayDataOfUnboxed (V_2 _ a b) =
    (((), arrayDataOfUnboxed a), arrayDataOfUnboxed b)
  --
  {-# INLINE unboxedOfArrayData #-}
  unboxedOfArrayData !n (((), a), b) =
    V_2 n (unboxedOfArrayData n a)
          (unboxedOfArrayData n b)

instance (Unbox a, Unbox b, Unbox c) => Unbox (a, b, c) where
  {-# INLINE arrayDataOfUnboxed #-}
  arrayDataOfUnboxed (V_3 _ a b c) =
    ((((), arrayDataOfUnboxed a), arrayDataOfUnboxed b), arrayDataOfUnboxed c)
  --
  {-# INLINE unboxedOfArrayData #-}
  unboxedOfArrayData !n ((((), a), b), c) =
    V_3 n (unboxedOfArrayData n a)
          (unboxedOfArrayData n b)
          (unboxedOfArrayData n c)

instance (Unbox a, Unbox b, Unbox c, Unbox d) => Unbox (a, b, c, d) where
  {-# INLINE arrayDataOfUnboxed #-}
  arrayDataOfUnboxed (V_4 _ a b c d) =
    (((((),
         arrayDataOfUnboxed a),
        arrayDataOfUnboxed b),
       arrayDataOfUnboxed c),
      arrayDataOfUnboxed d)
  --
  {-# INLINE unboxedOfArrayData #-}
  unboxedOfArrayData !n (((((), a), b), c), d) =
    V_4 n (unboxedOfArrayData n a)
          (unboxedOfArrayData n b)
          (unboxedOfArrayData n c)
          (unboxedOfArrayData n d)

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) => Unbox (a, b, c, d, e) where
  {-# INLINE arrayDataOfUnboxed #-}
  arrayDataOfUnboxed (V_5 _ a b c d e) =
    ((((((),
          arrayDataOfUnboxed a),
         arrayDataOfUnboxed b),
        arrayDataOfUnboxed c),
       arrayDataOfUnboxed d),
      arrayDataOfUnboxed e)
  --
  {-# INLINE unboxedOfArrayData #-}
  unboxedOfArrayData !n ((((((), a), b), c), d), e) =
    V_5 n (unboxedOfArrayData n a)
          (unboxedOfArrayData n b)
          (unboxedOfArrayData n c)
          (unboxedOfArrayData n d)
          (unboxedOfArrayData n e)

instance (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) => Unbox (a, b, c, d, e, f) where
  {-# INLINE arrayDataOfUnboxed #-}
  arrayDataOfUnboxed (V_6 _ a b c d e f) =
    (((((((),
           arrayDataOfUnboxed a),
          arrayDataOfUnboxed b),
         arrayDataOfUnboxed c),
        arrayDataOfUnboxed d),
       arrayDataOfUnboxed e),
      arrayDataOfUnboxed f)
  --
  {-# INLINE unboxedOfArrayData #-}
  unboxedOfArrayData !n (((((((), a), b), c), d), e), f) =
    V_6 n (unboxedOfArrayData n a)
          (unboxedOfArrayData n b)
          (unboxedOfArrayData n c)
          (unboxedOfArrayData n d)
          (unboxedOfArrayData n e)
          (unboxedOfArrayData n f)

-- TODO: What should be done with the commented out code below?

{--
#if MIN_VERSION_vector(0,12,0)
instance Unbox a => Unbox (Complex a) where
#else
instance (RealFloat a, Unbox a) => Unbox (Complex a) where
#endif
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Complex v2) = arrayDataOfUnboxed v2
  unboxedOfArrayData !n v2 = V_Complex (unboxedOfArrayData n v2)
--}


{--
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
type instance Vectors Bool   = Vector Bool
type instance Vectors (a,b)  = (Vectors a, Vectors b)


-- | /O(n)/ (typically). Convert a collection of unboxed vectors into an
-- Accelerate array.
--
-- Remember that unboxed vectors can be zipped and unzipped in O(1), so creating
-- the required 'Vectors' structure is free.
--
-- If the underlying vectors are pinned then this can be done without copying.
--
-- @since 1.1.0.0@
--
fromVectors :: (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array (fromElt sh) (aux arrayElt vecs)
  where
    wrap :: forall a. P.Prim a => P.Vector a -> UniqueArray a
    wrap v@(P.Vector _ l _)
      = $boundsCheck "fromVectors" "shape mismatch" (size sh == l)
      $ uniqueArrayOfVector v
    --
    aux :: ArrayEltR e -> Vectors e -> ArrayData e
    aux ArrayEltRunit           ()           = AD_Unit
    aux ArrayEltRint            (V_Int v)    = AD_Int    (wrap v)
    aux ArrayEltRint8           (V_Int8 v)   = AD_Int8   (wrap v)
    aux ArrayEltRint16          (V_Int16 v)  = AD_Int16  (wrap v)
    aux ArrayEltRint32          (V_Int32 v)  = AD_Int32  (wrap v)
    aux ArrayEltRint64          (V_Int64 v)  = AD_Int64  (wrap v)
    aux ArrayEltRword           (V_Word v)   = AD_Word   (wrap v)
    aux ArrayEltRword8          (V_Word8 v)  = AD_Word8  (wrap v)
    aux ArrayEltRword16         (V_Word16 v) = AD_Word16 (wrap v)
    aux ArrayEltRword32         (V_Word32 v) = AD_Word32 (wrap v)
    aux ArrayEltRword64         (V_Word64 v) = AD_Word64 (wrap v)
    aux ArrayEltRfloat          (V_Float v)  = AD_Float  (wrap v)
    aux ArrayEltRdouble         (V_Double v) = AD_Double (wrap v)
    aux ArrayEltRchar           (V_Char v)   = AD_Char   (wrap v)
    aux ArrayEltRbool           (V_Bool v)   = AD_Bool   (wrap v)
    aux (ArrayEltRpair ad1 ad2) (v1,v2)      = AD_Pair (aux ad1 v1) (aux ad2 v2)
    --
    aux _ _ = $internalError "fromVectors" "unsupported type"


-- | /O(1)/ (typically). Convert an Accelerate array into a collection of
-- unboxed vectors.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- Remember that unboxed vectors can be zipped and unzipped in O(1), so you can
-- convert the result of this function into a more natural unboxed tuple
-- representation for free.
--
-- @since 1.1.0.0@
--
toVectors :: (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array sh adata) = aux arrayElt adata
  where
    n :: Int
    n = R.size sh

    wrap :: forall a. P.Prim a => UniqueArray a -> P.Vector a
    wrap ua = vectorOfUniqueArray n ua

    aux :: ArrayEltR e -> ArrayData e -> Vectors e
    aux ArrayEltRunit           AD_Unit         = ()
    aux ArrayEltRint            (AD_Int v)      = V_Int    (wrap v)
    aux ArrayEltRint8           (AD_Int8 v)     = V_Int8   (wrap v)
    aux ArrayEltRint16          (AD_Int16 v)    = V_Int16  (wrap v)
    aux ArrayEltRint32          (AD_Int32 v)    = V_Int32  (wrap v)
    aux ArrayEltRint64          (AD_Int64 v)    = V_Int64  (wrap v)
    aux ArrayEltRword           (AD_Word v)     = V_Word   (wrap v)
    aux ArrayEltRword8          (AD_Word8 v)    = V_Word8  (wrap v)
    aux ArrayEltRword16         (AD_Word16 v)   = V_Word16 (wrap v)
    aux ArrayEltRword32         (AD_Word32 v)   = V_Word32 (wrap v)
    aux ArrayEltRword64         (AD_Word64 v)   = V_Word64 (wrap v)
    aux ArrayEltRfloat          (AD_Float v)    = V_Float  (wrap v)
    aux ArrayEltRdouble         (AD_Double v)   = V_Double (wrap v)
    aux ArrayEltRchar           (AD_Char v)     = V_Char   (wrap v)
    aux ArrayEltRbool           (AD_Bool v)     = V_Bool   (wrap v)
    aux (ArrayEltRpair ad1 ad2) (AD_Pair v1 v2) = (aux ad1 v1, aux ad2 v2)
    --
    aux _ _ = $internalError "toVectors" "unsupported type"
--}

