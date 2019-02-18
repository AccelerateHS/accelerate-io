{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Generic.Mutable
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides instance for 'Data.Vector.Generic.Mutable.MVector', for
-- generic mutable vectors backed by Accelerate.
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.IO.Data.Vector.Generic.Mutable
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Type

import qualified Data.Vector.Generic.Mutable                        as V

import Control.Monad.Primitive
import Data.Typeable
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import Prelude                                                      hiding ( length )

import GHC.Base
import GHC.TypeLits
import GHC.ForeignPtr


-- | Dense, regular, mutable, multi-dimensional arrays
--
data MArray sh s e where
  MArray  :: (Shape sh, Elt e)
          => EltRepr sh                         -- extent of dimensions == shape
          -> MutableArrayData (EltRepr e)       -- mutable array payload
          -> MArray sh s e

deriving instance Typeable MArray

type MVector = MArray DIM1

instance Elt e => V.MVector MVector e where
  {-# INLINE basicLength      #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps    #-}
  {-# INLINE basicUnsafeNew   #-}
  {-# INLINE basicInitialize  #-}
  {-# INLINE basicUnsafeRead  #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicUnsafeCopy  #-}
  basicLength (MArray ((), n) _) = n

  basicUnsafeSlice j m (MArray _ mad) = MArray ((),m) (go arrayElt mad 1)
    where
      go :: ArrayEltR a -> MutableArrayData a -> Int -> MutableArrayData a
      go ArrayEltRunit           AD_Unit         !_ = AD_Unit
      go ArrayEltRint            (AD_Int v)      !s = AD_Int     (slice v s)
      go ArrayEltRint8           (AD_Int8 v)     !s = AD_Int8    (slice v s)
      go ArrayEltRint16          (AD_Int16 v)    !s = AD_Int16   (slice v s)
      go ArrayEltRint32          (AD_Int32 v)    !s = AD_Int32   (slice v s)
      go ArrayEltRint64          (AD_Int64 v)    !s = AD_Int64   (slice v s)
      go ArrayEltRword           (AD_Word v)     !s = AD_Word    (slice v s)
      go ArrayEltRword8          (AD_Word8 v)    !s = AD_Word8   (slice v s)
      go ArrayEltRword16         (AD_Word16 v)   !s = AD_Word16  (slice v s)
      go ArrayEltRword32         (AD_Word32 v)   !s = AD_Word32  (slice v s)
      go ArrayEltRword64         (AD_Word64 v)   !s = AD_Word64  (slice v s)
      go ArrayEltRhalf           (AD_Half v)     !s = AD_Half    (slice v s)
      go ArrayEltRfloat          (AD_Float v)    !s = AD_Float   (slice v s)
      go ArrayEltRdouble         (AD_Double v)   !s = AD_Double  (slice v s)
      go ArrayEltRbool           (AD_Bool v)     !s = AD_Bool    (slice v s)
      go ArrayEltRchar           (AD_Char v)     !s = AD_Char    (slice v s)
      go (ArrayEltRvec ae)       (AD_Vec n# v)   !s = AD_Vec n# (go ae v (I# n# * s))
      go (ArrayEltRpair ae1 ae2) (AD_Pair v1 v2) !s = AD_Pair (go ae1 v1 s) (go ae2 v2 s)

      slice :: forall a. Storable a => UniqueArray a -> Int -> UniqueArray a
      slice (UniqueArray uid (Lifetime lft w fp)) s =
        UniqueArray uid (Lifetime lft w (plusForeignPtr fp (j * s * sizeOf (undefined::a))))

  basicOverlaps (MArray ((),m) mad1) (MArray ((),n) mad2) = go arrayElt mad1 mad2 1
    where
      go :: ArrayEltR a -> MutableArrayData a -> MutableArrayData a -> Int -> Bool
      go ArrayEltRunit           AD_Unit           AD_Unit           !_ = False
      go ArrayEltRint            (AD_Int v1)       (AD_Int v2)       !s = overlaps v1 v2 s
      go ArrayEltRint8           (AD_Int8 v1)      (AD_Int8 v2)      !s = overlaps v1 v2 s
      go ArrayEltRint16          (AD_Int16 v1)     (AD_Int16 v2)     !s = overlaps v1 v2 s
      go ArrayEltRint32          (AD_Int32 v1)     (AD_Int32 v2)     !s = overlaps v1 v2 s
      go ArrayEltRint64          (AD_Int64 v1)     (AD_Int64 v2)     !s = overlaps v1 v2 s
      go ArrayEltRword           (AD_Word v1)      (AD_Word v2)      !s = overlaps v1 v2 s
      go ArrayEltRword8          (AD_Word8 v1)     (AD_Word8 v2)     !s = overlaps v1 v2 s
      go ArrayEltRword16         (AD_Word16 v1)    (AD_Word16 v2)    !s = overlaps v1 v2 s
      go ArrayEltRword32         (AD_Word32 v1)    (AD_Word32 v2)    !s = overlaps v1 v2 s
      go ArrayEltRword64         (AD_Word64 v1)    (AD_Word64 v2)    !s = overlaps v1 v2 s
      go ArrayEltRhalf           (AD_Half v1)      (AD_Half v2)      !s = overlaps v1 v2 s
      go ArrayEltRfloat          (AD_Float v1)     (AD_Float v2)     !s = overlaps v1 v2 s
      go ArrayEltRdouble         (AD_Double v1)    (AD_Double v2)    !s = overlaps v1 v2 s
      go ArrayEltRbool           (AD_Bool v1)      (AD_Bool v2)      !s = overlaps v1 v2 s
      go ArrayEltRchar           (AD_Char v1)      (AD_Char v2)      !s = overlaps v1 v2 s
      go (ArrayEltRvec ae)       (AD_Vec n# v1)    (AD_Vec _ v2)     !s = go ae v1 v2 (I# n# * s) -- the type ensures these must be the same vector width
      go (ArrayEltRpair ae1 ae2) (AD_Pair v11 v12) (AD_Pair v21 v22) !s = go ae1 v11 v21 s || go ae2 v12 v22 s

      overlaps :: forall a. Storable a => UniqueArray a -> UniqueArray a -> Int -> Bool
      overlaps (UniqueArray _ (Lifetime _ _ (ForeignPtr addr1# c1))) (UniqueArray _ (Lifetime _ _ (ForeignPtr addr2# c2))) s =
        let i = I# (addr2Int# addr1#)
            j = I# (addr2Int# addr2#)
            k = s * sizeOf (undefined::a)
        in
        same c1 c2 && (between i j (j + n*k) || between j i (i + m*k))

      same :: ForeignPtrContents -> ForeignPtrContents -> Bool
      same (PlainPtr  mba1#)   (PlainPtr  mba2#)   = isTrue# (sameMutableByteArray# mba1# mba2#)
      same (MallocPtr mba1# _) (MallocPtr mba2# _) = isTrue# (sameMutableByteArray# mba1# mba2#)
      -- same PlainForeignPtr{}   PlainForeignPtr{}   = False  -- probably?
      same _                   _                   = False  -- probably? should we still check whether the address ranges overlap?

      between :: Int -> Int -> Int -> Bool
      between x y z = x >= y && x < z

  basicUnsafeNew n = unsafePrimToPrim $ MArray ((),n) <$> newArrayData n

  basicInitialize (MArray ((),n) mad) = unsafePrimToPrim $ go (arrayElt :: ArrayEltR (EltRepr e)) (ptrsOfArrayData mad) 1
    where
      go :: ArrayEltR a -> ArrayPtrs a -> Int -> IO ()
      go ArrayEltRunit           () !_ = return ()
      go ArrayEltRint            p  !s = initialise p s
      go ArrayEltRint8           p  !s = initialise p s
      go ArrayEltRint16          p  !s = initialise p s
      go ArrayEltRint32          p  !s = initialise p s
      go ArrayEltRint64          p  !s = initialise p s
      go ArrayEltRword           p  !s = initialise p s
      go ArrayEltRword8          p  !s = initialise p s
      go ArrayEltRword16         p  !s = initialise p s
      go ArrayEltRword32         p  !s = initialise p s
      go ArrayEltRword64         p  !s = initialise p s
      go ArrayEltRhalf           p  !s = initialise p s
      go ArrayEltRfloat          p  !s = initialise p s
      go ArrayEltRdouble         p  !s = initialise p s
      go ArrayEltRbool           p  !s = initialise p s
      go ArrayEltRchar           p  !s = initialise p s
      go aeR@(ArrayEltRvec ae)   p  !s = go ae p (s * width aeR)
      go (ArrayEltRpair ae1 ae2) (p1,p2) !s = go ae1 p1 s >> go ae2 p2 s

      width :: forall n a. KnownNat n => ArrayEltR (Vec n a) -> Int
      width _ = fromInteger (natVal' (proxy# :: Proxy# n))

      initialise :: forall a. Storable a => Ptr a -> Int -> IO ()
      initialise p s = fillBytes p 0 (n * s * sizeOf (undefined::a))

  basicUnsafeRead  (MArray _ mad) i   = unsafePrimToPrim $ toElt <$> unsafeReadArrayData mad i
  basicUnsafeWrite (MArray _ mad) i v = unsafePrimToPrim $ unsafeWriteArrayData mad i (fromElt v)

  basicUnsafeCopy (MArray _ dst) (MArray ((),n) src) = unsafePrimToPrim $ go (arrayElt :: ArrayEltR (EltRepr e)) (ptrsOfArrayData dst) (ptrsOfArrayData src) 1
    where
      go :: ArrayEltR a -> ArrayPtrs a -> ArrayPtrs a -> Int -> IO ()
      go ArrayEltRunit           () () !_ = return ()
      go ArrayEltRint            u  v  !s = copy u v s
      go ArrayEltRint8           u  v  !s = copy u v s
      go ArrayEltRint16          u  v  !s = copy u v s
      go ArrayEltRint32          u  v  !s = copy u v s
      go ArrayEltRint64          u  v  !s = copy u v s
      go ArrayEltRword           u  v  !s = copy u v s
      go ArrayEltRword8          u  v  !s = copy u v s
      go ArrayEltRword16         u  v  !s = copy u v s
      go ArrayEltRword32         u  v  !s = copy u v s
      go ArrayEltRword64         u  v  !s = copy u v s
      go ArrayEltRhalf           u  v  !s = copy u v s
      go ArrayEltRfloat          u  v  !s = copy u v s
      go ArrayEltRdouble         u  v  !s = copy u v s
      go ArrayEltRbool           u  v  !s = copy u v s
      go ArrayEltRchar           u  v  !s = copy u v s
      go aeR@(ArrayEltRvec ae)   u  v  !s = go ae u v (s * width aeR)
      go (ArrayEltRpair ae1 ae2) (u1,u2) (v1,v2) !s = go ae1 u1 v1 s >> go ae2 u2 v2 s

      width :: forall n a. KnownNat n => ArrayEltR (Vec n a) -> Int
      width _ = fromInteger (natVal' (proxy# :: Proxy# n))

      copy :: forall a. Storable a => Ptr a -> Ptr a -> Int -> IO ()
      copy u v s = copyBytes u v (n * s * sizeOf (undefined::a))


#if !MIN_VERSION_base(4,10,0)
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr# c) (I# i#) = ForeignPtr (plusAddr# addr# i#) c
#endif

