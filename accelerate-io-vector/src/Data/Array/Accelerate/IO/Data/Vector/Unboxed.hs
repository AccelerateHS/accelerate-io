{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Unboxed
-- Copyright   : [2017..2020] The Accelerate Team
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
import Data.Array.Accelerate.Sugar.Array                            as A hiding ( Vector )
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Elt
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R

import Data.Int
import Data.Word


-- | /O(n)/ (typically). Convert an Unboxed vector into an Accelerate array.
--
-- If the underlying vectors are pinned then this can be done without copying.
--
-- See also: <https://ghc.haskell.org/trac/ghc/ticket/5556>
--
-- @since 0.1.0.0@
--
{-# INLINE fromUnboxed #-}
fromUnboxed :: Unbox e => Vector e -> Array DIM1 e
fromUnboxed v = Array (R.Array ((), U.length v) (arrayDataOfUnboxed v))


-- | /O(1)/ (typically). Convert an Accelerate array into an Unboxed vector.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying. The resulting vector will be pinned.
--
-- @since 0.1.0.0@
--
{-# INLINE toUnboxed #-}
toUnboxed :: forall sh e. (Shape sh, Unbox e) => Array sh e -> Vector e
toUnboxed (Array (R.Array sh adata)) = unboxedOfArrayData (R.size (shapeR @sh) sh) adata


-- Instances
-- ---------

class (U.Unbox e, Elt e) => Unbox e where
  arrayDataOfUnboxed :: U.Vector e -> ArrayData (EltR e)
  unboxedOfArrayData :: Int -> ArrayData (EltR e) -> U.Vector e

instance Unbox () where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed V_Unit{} = ()
  unboxedOfArrayData !n () = V_Unit n

instance Unbox Int where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Int (vectorOfUniqueArray n v)

instance Unbox Int8 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int8 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Int8 (vectorOfUniqueArray n v)

instance Unbox Int16 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int16 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Int16 (vectorOfUniqueArray n v)

instance Unbox Int32 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int32 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Int32 (vectorOfUniqueArray n v)

instance Unbox Int64 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Int64 v)  = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Int64 (vectorOfUniqueArray n v)

instance Unbox Word where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Word (vectorOfUniqueArray n v)

instance Unbox Word8 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word8 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Word8 (vectorOfUniqueArray n v)

instance Unbox Word16 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word16 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Word16 (vectorOfUniqueArray n v)

instance Unbox Word32 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word32 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Word32 (vectorOfUniqueArray n v)

instance Unbox Word64 where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Word64 v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Word64 (vectorOfUniqueArray n v)

instance Unbox Float where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Float v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Float (vectorOfUniqueArray n v)

instance Unbox Double where
  {-# INLINE arrayDataOfUnboxed #-}
  {-# INLINE unboxedOfArrayData #-}
  arrayDataOfUnboxed (V_Double v) = uniqueArrayOfVector v
  unboxedOfArrayData !n v = V_Double (vectorOfUniqueArray n v)

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
    (((((), arrayDataOfUnboxed a)
          , arrayDataOfUnboxed b)
          , arrayDataOfUnboxed c)
          , arrayDataOfUnboxed d)
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
    ((((((), arrayDataOfUnboxed a)
           , arrayDataOfUnboxed b)
           , arrayDataOfUnboxed c)
           , arrayDataOfUnboxed d)
           , arrayDataOfUnboxed e)
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
    (((((((), arrayDataOfUnboxed a)
            , arrayDataOfUnboxed b)
            , arrayDataOfUnboxed c)
            , arrayDataOfUnboxed d)
            , arrayDataOfUnboxed e)
            , arrayDataOfUnboxed f)
  --
  {-# INLINE unboxedOfArrayData #-}
  unboxedOfArrayData !n (((((((), a), b), c), d), e), f) =
    V_6 n (unboxedOfArrayData n a)
          (unboxedOfArrayData n b)
          (unboxedOfArrayData n c)
          (unboxedOfArrayData n d)
          (unboxedOfArrayData n e)
          (unboxedOfArrayData n f)

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

