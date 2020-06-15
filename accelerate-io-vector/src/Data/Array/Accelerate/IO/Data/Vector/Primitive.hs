{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TODO: Why is the pattern checker complaining that these pattern matches are
--       redundant?
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Primitive
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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

import Data.Array.Accelerate.IO.Data.Patterns

import Data.Int
import Data.Word


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
-- See also: <https://ghc.haskell.org/trac/ghc/ticket/5556>
--
-- @since 1.1.0.0@
--
{-# INLINE fromVectors #-}
fromVectors :: forall sh e. (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array $ R.Array (fromElt sh) (aux (eltType @e) vecs)
  where
    {-# INLINE wrap #-}
    wrap :: Prim a => Vector a -> UniqueArray a
    wrap v@(Vector _ l _)
      = $boundsCheck "fromVectors" "shape mismatch" (size sh == l)
      $ uniqueArrayOfVector v

    {-# INLINE aux #-}
    aux :: TupleType a -> Vectors a -> ArrayData a
    aux TupRunit         _        = ()
    aux TupInt           v        = wrap v
    aux TupInt8          v        = wrap v
    aux TupInt16         v        = wrap v
    aux TupInt32         v        = wrap v
    aux TupInt64         v        = wrap v
    aux TupWord          v        = wrap v
    aux TupWord8         v        = wrap v
    aux TupWord16        v        = wrap v
    aux TupWord32        v        = wrap v
    aux TupWord64        v        = wrap v
    aux TupFloat         v        = wrap v
    aux TupDouble        v        = wrap v
    aux TupChar          v        = wrap v
    aux (TupRpair t1 t2) (v1, v2) = (aux t1 v1, aux t2 v2)
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
{-# INLINE toVectors #-}
toVectors :: forall sh e. (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array (R.Array sh adata)) = aux (eltType @e) adata
  where
    n :: Int
    !n = R.size (shapeR @sh) sh

    {-# INLINE wrap #-}
    wrap :: Prim a => UniqueArray a -> Vector a
    wrap ua = vectorOfUniqueArray n ua

    {-# INLINE aux #-}
    aux :: TupleType a -> ArrayData a -> Vectors a
    aux TupRunit         _        = ()
    aux TupInt           v        = wrap v
    aux TupInt8          v        = wrap v
    aux TupInt16         v        = wrap v
    aux TupInt32         v        = wrap v
    aux TupInt64         v        = wrap v
    aux TupWord          v        = wrap v
    aux TupWord8         v        = wrap v
    aux TupWord16        v        = wrap v
    aux TupWord32        v        = wrap v
    aux TupWord64        v        = wrap v
    aux TupFloat         v        = wrap v
    aux TupDouble        v        = wrap v
    aux TupChar          v        = wrap v
    aux (TupRpair t1 t2) (v1, v2) = (aux t1 v1, aux t2 v2)
    --
    aux _ _ = $internalError "toVectors" "unsupported type"

