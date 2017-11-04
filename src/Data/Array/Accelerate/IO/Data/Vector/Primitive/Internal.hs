{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Data.Vector.Primitive.Internal
  where

import Data.Primitive                                               ( sizeOf )
import Data.Primitive.ByteArray

import Data.Vector.Primitive

import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime

import GHC.Base
import GHC.ForeignPtr
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
  $ Vector 0 n <$> byteArrayOfForeignPtr (n * sizeOf (undefined::a)) (unsafeGetValue (uniqueArrayData ua))


-- Return the ByteArray underlying a ForeignPtr, or a new byte array if it is
-- not a Plain ForeignPtr.
--
byteArrayOfForeignPtr :: Int -> ForeignPtr a -> IO ByteArray
byteArrayOfForeignPtr (I# bytes#) (ForeignPtr addr# c) = IO $ \s ->
  case c of
    PlainPtr mba# -> case unsafeFreezeByteArray# mba# s of
                       (# s', ba#  #) -> (# s', ByteArray ba# #)

    _             -> case newAlignedPinnedByteArray# bytes# 16# s of
                       (# s1, mba# #) -> case copyAddrToByteArray# addr# mba# 0# bytes# s1 of
                                           s2 -> case unsafeFreezeByteArray# mba# s2 of
                                                   (# s3, ba# #) -> (# s3, ByteArray ba# #)


-- Return the ByteArray as a ForeignPtr. This will attempt a non-copying
-- conversion, if the underlying byte array is pinned.
--
foreignPtrOfByteArray :: Int -> Int -> ByteArray -> IO (ForeignPtr a)
foreignPtrOfByteArray (I# soff#) (I# bytes#) (ByteArray ba#) = IO $ \s ->
  case isByteArrayPinned# ba# of
    0# -> case newAlignedPinnedByteArray# bytes# 16# s of
            (# s1, mba# #) -> case copyByteArray# ba# 0# mba# soff# bytes# s1 of
                                s2 -> (# s2, ForeignPtr (byteArrayContents# (unsafeCoerce# mba#)) (PlainPtr mba#) #)

    _  -> (# s, ForeignPtr (byteArrayContents# ba#) (PlainPtr (unsafeCoerce# ba#)) #)


#if !MIN_VERSION_base(4,10,0)
isByteArrayPinned# :: ByteArray# -> Int#
isByteArrayPinned# _ = 0#
#endif

