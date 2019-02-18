{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
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

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Type

import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

import GHC.Base
import GHC.TypeLits


-- | A family of types which represent a collection of 'Ptr's. The
-- structure of the collection depends on the element type @e@.
--
type Ptrs e = ArrayPtrs e


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
fromPtrs :: (Shape sh, Elt e) => sh -> Ptrs (EltRepr e) -> Array sh e
fromPtrs sh ps = Array (fromElt sh) (aux arrayElt ps)
  where
    wrap :: (UniqueArray e -> r) -> Ptr e -> r
    wrap k p = k (unsafePerformIO $ newUniqueArray =<< newForeignPtr_ p)

    vec :: forall n e. KnownNat n => ArrayData e -> ArrayData (Vec n e)
    vec = let !(I# n#) = fromInteger (natVal' (proxy# :: Proxy# n))
          in  AD_Vec n#

    aux :: ArrayEltR e -> Ptrs e -> ArrayData e
    aux ArrayEltRunit           = const AD_Unit
    aux ArrayEltRint            = wrap AD_Int
    aux ArrayEltRint8           = wrap AD_Int8
    aux ArrayEltRint16          = wrap AD_Int16
    aux ArrayEltRint32          = wrap AD_Int32
    aux ArrayEltRint64          = wrap AD_Int64
    aux ArrayEltRword           = wrap AD_Word
    aux ArrayEltRword8          = wrap AD_Word8
    aux ArrayEltRword16         = wrap AD_Word16
    aux ArrayEltRword32         = wrap AD_Word32
    aux ArrayEltRword64         = wrap AD_Word64
    aux ArrayEltRhalf           = wrap AD_Half
    aux ArrayEltRfloat          = wrap AD_Float
    aux ArrayEltRdouble         = wrap AD_Double
    aux ArrayEltRbool           = wrap AD_Bool
    aux ArrayEltRchar           = wrap AD_Char
    aux (ArrayEltRvec ae)       = \v       -> vec (aux ae v)
    aux (ArrayEltRpair ae1 ae2) = \(v1,v2) -> AD_Pair (aux ae1 v1) (aux ae2 v2)


-- | /O(1)/. Yield the underlying 'Ptrs' backing the given Accelerate array. The
-- element type @e@ will determine the structure of the output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 1.1.0.0@
--
{-# INLINE toPtrs #-}
toPtrs :: (Shape sh, Elt e) => Array sh e -> Ptrs (EltRepr e)
toPtrs (Array _ adata) = ptrsOfArrayData adata

