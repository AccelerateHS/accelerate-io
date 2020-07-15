{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Foreign.Ptr
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Foreign.Ptr
  where

import Data.Array.Accelerate.Array.Data                             ( ArrayData, GArrayDataR )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Type
import qualified Data.Array.Accelerate.Representation.Array         as R

import Data.Array.Accelerate.IO.Foreign.Internal

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import System.IO.Unsafe


-- | A family of types which represent a collection of 'Ptr's. The
-- structure of the collection depends on the element type @e@.
--
type Ptrs e = GArrayDataR Ptr e


-- | /O(1)/. Treat the set of 'Ptrs' as an Accelerate array. The type of
-- elements @e@ in the output Accelerate array determines the structure of the
-- collection.
--
-- Data is considered to be in row-major order. You must ensure that each of the
-- input pointers contains the right number of elements.
--
-- The data may not be modified through the 'Ptrs' afterwards.
--
-- You are responsible for ensuring that the data remains alive for the duration
-- of the Accelerate computation, and for freeing it afterwards.
--
-- You should make sure that the data is suitably aligned.
--
-- @since 1.1.0.0@
--
{-# INLINE fromPtrs #-}
fromPtrs :: forall sh e. (Shape sh, Elt e) => sh -> Ptrs (EltR e) -> Array sh e
fromPtrs sh ps = Array (R.Array (fromElt sh) (go (eltR @e) ps))
  where
    go :: TypeR a -> Ptrs a -> ArrayData a
    go TupRunit           ()       = ()
    go (TupRpair aR1 aR2) (a1, a2) = (go aR1 a1, go aR2 a2)
    go (TupRsingle t)     p
      | ScalarArrayDict{} <- scalarArrayDict t
      = unsafePerformIO $ newUniqueArray =<< newForeignPtr_ p


-- | /O(1)/. Yield the underlying 'Ptrs' backing the given Accelerate array. The
-- element type @e@ will determine the structure of the output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 1.1.0.0@
--
{-# INLINE toPtrs #-}
toPtrs :: forall sh e. (Shape sh, Elt e) => Array sh e -> Ptrs (EltR e)
toPtrs (Array (R.Array _ adata)) = go (eltR @e) adata
  where
    go :: TypeR a -> ArrayData a -> Ptrs a
    go TupRunit           ()       = ()
    go (TupRpair aR1 aR2) (a1, a2) = (go aR1 a1, go aR2 a2)
    go (TupRsingle t)     a
      | ScalarArrayDict{} <- scalarArrayDict t
      = unsafeForeignPtrToPtr (unsafeGetValue (uniqueArrayData a))

