{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Foreign.ForeignPtr
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Foreign.ForeignPtr
  where

import Data.Array.Accelerate.Array.Data                             ( ArrayData, GArrayDataR )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import qualified Data.Array.Accelerate.Representation.Array         as R

import Data.Array.Accelerate.IO.Foreign.Internal

import Foreign.ForeignPtr
import System.IO.Unsafe


-- | A family of types which represent a collection of 'ForeignPtr's. The
-- structure of the collection depends on the element type @e@.
--
type ForeignPtrs e = GArrayDataR ForeignPtr e


-- | /O(1)/. Treat the set of 'ForeignPtrs' as an Accelerate array. The type of
-- elements @e@ in the output Accelerate array determines the structure of the
-- collection.
--
-- Data is considered to be in row-major order. You must ensure that each of the
-- input pointers contains the right number of elements.
--
-- The data may not be modified through the 'ForeignPtr's afterwards.
--
-- You should make sure that the data is suitably aligned.
--
-- @since 1.1.0.0@
--
{-# INLINE fromForeignPtrs #-}
fromForeignPtrs :: forall sh e. (Shape sh, Elt e) => sh -> ForeignPtrs (EltR e) -> Array sh e
fromForeignPtrs sh fps = Array (R.Array (fromElt sh) (go (eltR @e) fps))
  where
    go :: TypeR a -> ForeignPtrs a -> ArrayData a
    go TupRunit           ()       = ()
    go (TupRpair aR1 aR2) (a1, a2) = (go aR1 a1, go aR2 a2)
    go (TupRsingle t)     a
      | ScalarArrayDict{} <- scalarArrayDict t
      = unsafePerformIO $ newUniqueArray a


-- | /O(1)/. Yield the 'ForeignPtr's underlying the given Accelerate 'Array'.
-- The element type @e@ will determine the structure of the output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 1.1.0.0@
--
{-# INLINE toForeignPtrs #-}
toForeignPtrs :: forall sh e. (Shape sh, Elt e) => Array sh e -> ForeignPtrs (EltR e)
toForeignPtrs (Array (R.Array _ adata)) = go (eltR @e) adata
  where
    go :: TypeR a -> ArrayData a -> ForeignPtrs a
    go TupRunit           ()       = ()
    go (TupRpair aR1 aR2) (a1, a2) = (go aR1 a1, go aR2 a2)
    go (TupRsingle t)     a
      | ScalarArrayDict{} <- scalarArrayDict t
      = unsafeGetValue (uniqueArrayData a)

