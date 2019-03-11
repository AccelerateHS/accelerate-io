{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal
  where

import Data.Primitive                                               ( sizeOf )

import Data.Vector.Primitive

import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.IO.Data.Primitive.ByteArray

import System.IO.Unsafe


-- Convert a primitive vector into a unique array
--
{-# INLINE uniqueArrayOfVector #-}
uniqueArrayOfVector :: forall a. Prim a => Vector a -> UniqueArray a
uniqueArrayOfVector (Vector o l ba)
  = unsafePerformIO
  $ newUniqueArray =<< foreignPtrOfByteArray o (l * sizeOf (undefined::a)) ba

-- Convert a unique array into a primitive vector
--
{-# INLINE vectorOfUniqueArray #-}
vectorOfUniqueArray :: forall a. Prim a => Int -> UniqueArray a -> Vector a
vectorOfUniqueArray n ua
  = unsafePerformIO
  $ Vector 0 n `fmap` byteArrayOfForeignPtr (n * sizeOf (undefined::a)) (unsafeGetValue (uniqueArrayData ua))

