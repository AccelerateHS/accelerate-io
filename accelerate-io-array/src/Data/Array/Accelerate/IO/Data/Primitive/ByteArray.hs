{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Data.Primitive.ByteArray
  where

import Data.Primitive.ByteArray

import GHC.Base
import GHC.ForeignPtr


-- Return the ByteArray underlying a ForeignPtr, or a new byte array if
-- it is not a Plain ForeignPtr.
--
{-# INLINE byteArrayOfForeignPtr #-}
byteArrayOfForeignPtr :: Int -> ForeignPtr a -> IO ByteArray
byteArrayOfForeignPtr (I# bytes#) (ForeignPtr addr# c) = IO $ \s ->
  case c of
    PlainPtr mba# -> case unsafeFreezeByteArray# mba# s of
                       (# s', ba#  #) -> (# s', ByteArray ba# #)

    _             -> case newAlignedPinnedByteArray# bytes# 16# s      of { (# s1, mba# #) ->
                     case copyAddrToByteArray# addr# mba# 0# bytes# s1 of { s2 ->
                     case unsafeFreezeByteArray# mba# s2               of { (# s3, ba# #) ->
                       (# s3, ByteArray ba# #) }}}


-- Return the ByteArray as a ForeignPtr. This will attempt a non-copying
-- conversion, if the underlying byte array is pinned.
--
{-# INLINE foreignPtrOfByteArray #-}
foreignPtrOfByteArray :: Int -> Int -> ByteArray -> IO (ForeignPtr a)
foreignPtrOfByteArray (I# soff#) (I# bytes#) (ByteArray ba#) = IO $ \s ->
  case isByteArrayPinned# ba# of
    0# -> case newAlignedPinnedByteArray# bytes# 16# s    of { (# s1, mba# #) ->
          case copyByteArray# ba# 0# mba# soff# bytes# s1 of { s2 ->
            (# s2, ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr mba#) #) }}

    _  -> (# s, ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#)) #)


#if !MIN_VERSION_base(4,10,0)
isByteArrayPinned# :: ByteArray# -> Int#
isByteArrayPinned# _ = 0#
#endif

