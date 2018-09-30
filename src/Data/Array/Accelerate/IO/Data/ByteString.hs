{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.ByteString
-- Copyright   : [2010..2011] Sean Seefried
--               [2010..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Conversion between strict 'ByteString's and Accelerate 'Array's.
--

module Data.Array.Accelerate.IO.Data.ByteString (

  ByteStrings,
  fromByteStrings, toByteStrings,

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.ByteString                                              as B
import Data.ByteString.Internal                                     as B
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

#if !MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr
#endif
import GHC.Base
import GHC.TypeLits


-- | A family of types that represents a collection of 'ByteString's. The
-- structure of the collection depends on the element type @e@.
--
type family ByteStrings e

type instance ByteStrings ()        = ()
type instance ByteStrings Int       = ByteString
type instance ByteStrings Int8      = ByteString
type instance ByteStrings Int16     = ByteString
type instance ByteStrings Int32     = ByteString
type instance ByteStrings Int64     = ByteString
type instance ByteStrings Word      = ByteString
type instance ByteStrings Word8     = ByteString
type instance ByteStrings Word16    = ByteString
type instance ByteStrings Word32    = ByteString
type instance ByteStrings Word64    = ByteString
type instance ByteStrings Half      = ByteString
type instance ByteStrings Float     = ByteString
type instance ByteStrings Double    = ByteString
type instance ByteStrings Bool      = ByteString
type instance ByteStrings Char      = ByteString
type instance ByteStrings (Vec n a) = ByteStrings a
type instance ByteStrings (a,b)     = (ByteStrings a, ByteStrings b)


-- | /O(1)/. Treat a set of strict 'ByteStrings' as an Accelerate array. The
-- type of the elements @e@ in the output Accelerate array determines the
-- structure of the collection.
--
-- Data is considered to be in row-major order. You must ensure that each input
-- contains the right number of bytes (this is not checked).
--
-- The input data may not be modified through the 'ByteString's afterwards.
--
-- @since 1.1.0.0@
--
{-# INLINE fromByteStrings #-}
fromByteStrings :: (Shape sh, Elt e) => sh -> ByteStrings (EltRepr e) -> Array sh e
fromByteStrings sh bs = Array (fromElt sh) (aux arrayElt bs)
  where
    wrap :: (UniqueArray e -> r) -> ByteString -> r
    wrap k (B.toForeignPtr -> (ps,s,_)) =
      k (unsafePerformIO $ newUniqueArray (castForeignPtr (plusForeignPtr ps s)))

    vec :: forall n e. KnownNat n => ArrayData e -> ArrayData (Vec n e)
    vec = let !(I# n#) = fromInteger (natVal' (proxy# :: Proxy# n))
          in  AD_Vec n#

    aux :: ArrayEltR e -> ByteStrings e -> ArrayData e
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


-- | /O(1)/. Convert an Accelerate 'Array' into a collection of strict
-- 'ByteStrings'. The element type @e@ will determine the structure of the
-- output collection.
--
-- Data is considered to be in row-major order.
--
-- @since 1.1.0.0@
--
{-# INLINE toByteStrings #-}
toByteStrings :: (Shape sh, Elt e) => Array sh e -> ByteStrings (EltRepr e)
toByteStrings (Array sh adata) = aux arrayElt adata
  where
    n :: Int
    n = R.size sh

    wrap :: forall a. Storable a => UniqueArray a -> ByteString
    wrap (unsafeGetValue . uniqueArrayData -> fp) =
      B.fromForeignPtr (castForeignPtr fp) 0 (n * sizeOf (undefined::a))

    aux :: ArrayEltR e -> ArrayData e -> ByteStrings e
    aux ArrayEltRunit           AD_Unit         = ()
    aux ArrayEltRint            (AD_Int s)      = wrap s
    aux ArrayEltRint8           (AD_Int8 s)     = wrap s
    aux ArrayEltRint16          (AD_Int16 s)    = wrap s
    aux ArrayEltRint32          (AD_Int32 s)    = wrap s
    aux ArrayEltRint64          (AD_Int64 s)    = wrap s
    aux ArrayEltRword           (AD_Word s)     = wrap s
    aux ArrayEltRword8          (AD_Word8 s)    = wrap s
    aux ArrayEltRword16         (AD_Word16 s)   = wrap s
    aux ArrayEltRword32         (AD_Word32 s)   = wrap s
    aux ArrayEltRword64         (AD_Word64 s)   = wrap s
    aux ArrayEltRhalf           (AD_Half s)     = wrap s
    aux ArrayEltRfloat          (AD_Float s)    = wrap s
    aux ArrayEltRdouble         (AD_Double s)   = wrap s
    aux ArrayEltRbool           (AD_Bool s)     = wrap s
    aux ArrayEltRchar           (AD_Char s)     = wrap s
    aux (ArrayEltRvec ae)       (AD_Vec _ s)    = aux ae s
    aux (ArrayEltRpair ae1 ae2) (AD_Pair s1 s2) = (aux ae1 s1, aux ae2 s2)


#if !MIN_VERSION_base(4,10,0)
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr# c) (I# d#) = ForeignPtr (plusAddr# addr# d#) c
#endif

