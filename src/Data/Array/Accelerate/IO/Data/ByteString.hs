{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
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

  fromByteString, toByteString

) where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.ByteString                                              as B
import Data.ByteString.Internal                                     as B
import Data.Word
import Foreign.ForeignPtr
import System.IO.Unsafe

#if !MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr
import GHC.Base
#endif


-- | /O(1)/. Convert a strict 'ByteString' into an Accelerate 'Array'.
--
fromByteString :: Shape sh => sh -> ByteString -> Array sh Word8
fromByteString sh (B.toForeignPtr -> (ps,s,l))
  = $boundsCheck "fromByteString" "shape mismatch" (size sh == l)
  $ Array (fromElt sh) (AD_Word8 (unsafePerformIO (newUniqueArray (plusForeignPtr ps s))))


-- | /O(1)/. Convert an Accelerate 'Array' into a strict 'ByteString'.
--
toByteString :: Array sh Word8 -> ByteString
toByteString (Array sh (AD_Word8 ua))
  = B.fromForeignPtr (unsafeGetValue (uniqueArrayData ua)) 0 (R.size sh)


#if !MIN_VERSION_base(4,10,0)
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr# c) (I# d#) = ForeignPtr (plusAddr# addr# d#) c
#endif

