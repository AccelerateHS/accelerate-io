{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Unboxed
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Efficient conversion between 'Data.Vector.Unboxed' vectors and Accelerate
-- 'Array's.
--

module Data.Array.Accelerate.IO.Data.Vector.Unboxed (

  toUnboxed,
  fromUnboxed,

  ToUnboxed(..),
  FromUnboxed(..),

) where

import Data.Vector.Unboxed.Base
import qualified Data.Vector.Unboxed                                as U

import Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            hiding ( Vector )
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Int
import Data.Word


-- | /O(n)/ (typically). Convert an Unboxed vector into an Accelerate array
-- Accelerate array.
--
-- If the underlying vectors are pinned then this can be done without copying.
--
-- @since 1.1.0.0@
--
fromUnboxed :: (Shape sh, FromUnboxed e) => sh -> Vector e -> Array sh e
fromUnboxed sh v
  = $boundsCheck "fromUnboxed" "shape mismatch" (size sh == U.length v)
  $ Array (fromElt sh) (fromUnboxed' v)


-- | /O(1)/ (typically). Convert an Accelerate array into an Unboxed vector.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- @since 1.1.0.0@
--
toUnboxed :: (Shape sh, ToUnboxed e) => Array sh e -> Vector e
toUnboxed (Array sh adata)
  = toUnboxed' (R.size sh) adata


-- Instances
-- ---------

class (Unbox e, Elt e) => FromUnboxed e where
  fromUnboxed' :: Vector e -> ArrayData (EltRepr e)

instance FromUnboxed Int    where fromUnboxed' (V_Int v)    = AD_Int    (uniqueArrayOfVector v)
instance FromUnboxed Int8   where fromUnboxed' (V_Int8 v)   = AD_Int8   (uniqueArrayOfVector v)
instance FromUnboxed Int16  where fromUnboxed' (V_Int16 v)  = AD_Int16  (uniqueArrayOfVector v)
instance FromUnboxed Int32  where fromUnboxed' (V_Int32 v)  = AD_Int32  (uniqueArrayOfVector v)
instance FromUnboxed Int64  where fromUnboxed' (V_Int64 v)  = AD_Int64  (uniqueArrayOfVector v)
instance FromUnboxed Word   where fromUnboxed' (V_Word v)   = AD_Word   (uniqueArrayOfVector v)
instance FromUnboxed Word8  where fromUnboxed' (V_Word8 v)  = AD_Word8  (uniqueArrayOfVector v)
instance FromUnboxed Word16 where fromUnboxed' (V_Word16 v) = AD_Word16 (uniqueArrayOfVector v)
instance FromUnboxed Word32 where fromUnboxed' (V_Word32 v) = AD_Word32 (uniqueArrayOfVector v)
instance FromUnboxed Word64 where fromUnboxed' (V_Word64 v) = AD_Word64 (uniqueArrayOfVector v)
instance FromUnboxed Float  where fromUnboxed' (V_Float v)  = AD_Float  (uniqueArrayOfVector v)
instance FromUnboxed Double where fromUnboxed' (V_Double v) = AD_Double (uniqueArrayOfVector v)
instance FromUnboxed Char   where fromUnboxed' (V_Char v)   = AD_Char   (uniqueArrayOfVector v)
instance FromUnboxed Bool   where fromUnboxed' (V_Bool v)   = AD_Bool   (uniqueArrayOfVector v)

instance FromUnboxed ()  where
  fromUnboxed' V_Unit{} = AD_Unit

instance (FromUnboxed a, FromUnboxed b) => FromUnboxed (a, b) where
  fromUnboxed' (V_2 _ a b) =
    AD_Unit `AD_Pair` fromUnboxed' a
            `AD_Pair` fromUnboxed' b

instance (FromUnboxed a, FromUnboxed b, FromUnboxed c) => FromUnboxed (a, b, c) where
  fromUnboxed' (V_3 _ a b c) =
    AD_Unit `AD_Pair` fromUnboxed' a
            `AD_Pair` fromUnboxed' b
            `AD_Pair` fromUnboxed' c

instance (FromUnboxed a, FromUnboxed b, FromUnboxed c, FromUnboxed d) => FromUnboxed (a, b, c, d) where
  fromUnboxed' (V_4 _ a b c d) =
    AD_Unit `AD_Pair` fromUnboxed' a
            `AD_Pair` fromUnboxed' b
            `AD_Pair` fromUnboxed' c
            `AD_Pair` fromUnboxed' d

instance (FromUnboxed a, FromUnboxed b, FromUnboxed c, FromUnboxed d, FromUnboxed e) => FromUnboxed (a, b, c, d, e) where
  fromUnboxed' (V_5 _ a b c d e) =
    AD_Unit `AD_Pair` fromUnboxed' a
            `AD_Pair` fromUnboxed' b
            `AD_Pair` fromUnboxed' c
            `AD_Pair` fromUnboxed' d
            `AD_Pair` fromUnboxed' e

instance (FromUnboxed a, FromUnboxed b, FromUnboxed c, FromUnboxed d, FromUnboxed e, FromUnboxed f) => FromUnboxed (a, b, c, d, e, f) where
  fromUnboxed' (V_6 _ a b c d e f) =
    AD_Unit `AD_Pair` fromUnboxed' a
            `AD_Pair` fromUnboxed' b
            `AD_Pair` fromUnboxed' c
            `AD_Pair` fromUnboxed' d
            `AD_Pair` fromUnboxed' e
            `AD_Pair` fromUnboxed' f


class (Unbox e, Elt e) => ToUnboxed e where
  toUnboxed' :: Int -> ArrayData (EltRepr e) -> Vector e

instance ToUnboxed Int    where toUnboxed' n (AD_Int v)    = V_Int    (vectorOfUniqueArray n v)
instance ToUnboxed Int8   where toUnboxed' n (AD_Int8 v)   = V_Int8   (vectorOfUniqueArray n v)
instance ToUnboxed Int16  where toUnboxed' n (AD_Int16 v)  = V_Int16  (vectorOfUniqueArray n v)
instance ToUnboxed Int32  where toUnboxed' n (AD_Int32 v)  = V_Int32  (vectorOfUniqueArray n v)
instance ToUnboxed Int64  where toUnboxed' n (AD_Int64 v)  = V_Int64  (vectorOfUniqueArray n v)
instance ToUnboxed Word   where toUnboxed' n (AD_Word v)   = V_Word   (vectorOfUniqueArray n v)
instance ToUnboxed Word8  where toUnboxed' n (AD_Word8 v)  = V_Word8  (vectorOfUniqueArray n v)
instance ToUnboxed Word16 where toUnboxed' n (AD_Word16 v) = V_Word16 (vectorOfUniqueArray n v)
instance ToUnboxed Word32 where toUnboxed' n (AD_Word32 v) = V_Word32 (vectorOfUniqueArray n v)
instance ToUnboxed Word64 where toUnboxed' n (AD_Word64 v) = V_Word64 (vectorOfUniqueArray n v)
instance ToUnboxed Float  where toUnboxed' n (AD_Float v)  = V_Float  (vectorOfUniqueArray n v)
instance ToUnboxed Double where toUnboxed' n (AD_Double v) = V_Double (vectorOfUniqueArray n v)
instance ToUnboxed Char   where toUnboxed' n (AD_Char v)   = V_Char   (vectorOfUniqueArray n v)
instance ToUnboxed Bool   where toUnboxed' n (AD_Bool v)   = V_Bool   (vectorOfUniqueArray n v)

instance ToUnboxed () where
  toUnboxed' n AD_Unit = V_Unit n

instance (ToUnboxed a, ToUnboxed b) => ToUnboxed (a, b) where
  toUnboxed' n (AD_Unit `AD_Pair` a `AD_Pair` b) =
    V_2 n (toUnboxed' n a) (toUnboxed' n b)

instance (ToUnboxed a, ToUnboxed b, ToUnboxed c) => ToUnboxed (a, b, c) where
  toUnboxed' n (AD_Unit `AD_Pair` a `AD_Pair` b `AD_Pair` c) =
    V_3 n (toUnboxed' n a) (toUnboxed' n b) (toUnboxed' n c)

instance (ToUnboxed a, ToUnboxed b, ToUnboxed c, ToUnboxed d) => ToUnboxed (a, b, c, d) where
  toUnboxed' n (AD_Unit `AD_Pair` a `AD_Pair` b `AD_Pair` c `AD_Pair` d) =
    V_4 n (toUnboxed' n a) (toUnboxed' n b) (toUnboxed' n c) (toUnboxed' n d)

instance (ToUnboxed a, ToUnboxed b, ToUnboxed c, ToUnboxed d, ToUnboxed e) => ToUnboxed (a, b, c, d, e) where
  toUnboxed' n (AD_Unit `AD_Pair` a `AD_Pair` b `AD_Pair` c `AD_Pair` d `AD_Pair` e) =
    V_5 n (toUnboxed' n a) (toUnboxed' n b) (toUnboxed' n c) (toUnboxed' n d) (toUnboxed' n e)

instance (ToUnboxed a, ToUnboxed b, ToUnboxed c, ToUnboxed d, ToUnboxed e, ToUnboxed f) => ToUnboxed (a, b, c, d, e, f) where
  toUnboxed' n (AD_Unit `AD_Pair` a `AD_Pair` b `AD_Pair` c `AD_Pair` d `AD_Pair` e `AD_Pair` f) =
    V_6 n (toUnboxed' n a) (toUnboxed' n b) (toUnboxed' n c) (toUnboxed' n d) (toUnboxed' n e) (toUnboxed' n f)


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

