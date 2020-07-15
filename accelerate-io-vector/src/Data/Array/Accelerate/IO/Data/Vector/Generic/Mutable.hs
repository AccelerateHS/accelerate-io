{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Generic.Mutable
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides instance for 'Data.Vector.Generic.Mutable.MVector', for
-- generic mutable vectors backed by Accelerate.
--
-- @since 0.1.0.0
--

module Data.Array.Accelerate.IO.Data.Vector.Generic.Mutable
  where

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type

import qualified Data.Vector.Generic.Mutable                        as V

import Control.Monad.Primitive
import Data.Typeable
import Foreign.Marshal.Utils
import Foreign.Storable
import Prelude                                                      hiding ( length )

import GHC.Base
import GHC.ForeignPtr


-- | Dense, regular, mutable, multi-dimensional arrays
--
data MArray sh s e where
  MArray  :: EltR sh                        -- extent of dimensions == shape
          -> MutableArrayData (EltR e)      -- mutable array payload
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

  basicUnsafeSlice j m (MArray _ mad) = MArray ((),m) (go (eltR @e) mad)
    where
      go :: TypeR a -> MutableArrayData a -> MutableArrayData a
      go TupRunit           ()       = ()
      go (TupRpair aR1 aR2) (a1, a2) = (go aR1 a1, go aR2 a2)
      go (TupRsingle aR)    a        = scalar aR a

      scalar :: ScalarType a -> MutableArrayData a -> MutableArrayData a
      scalar (SingleScalarType t) a = single t a 1
      scalar (VectorScalarType t) a = vector t a

      vector :: VectorType a -> MutableArrayData a -> MutableArrayData a
      vector (VectorType w t) a
        | SingleArrayDict <- singleArrayDict t
        = single t a w

      single :: SingleType a -> MutableArrayData a -> Int -> MutableArrayData a
      single (NumSingleType t) = num t

      num :: NumType a -> MutableArrayData a -> Int -> MutableArrayData a
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> MutableArrayData a -> Int -> MutableArrayData a
      integral TypeInt    = slice
      integral TypeInt8   = slice
      integral TypeInt16  = slice
      integral TypeInt32  = slice
      integral TypeInt64  = slice
      integral TypeWord   = slice
      integral TypeWord8  = slice
      integral TypeWord16 = slice
      integral TypeWord32 = slice
      integral TypeWord64 = slice

      floating :: FloatingType a -> MutableArrayData a -> Int -> MutableArrayData a
      floating TypeHalf   = slice
      floating TypeFloat  = slice
      floating TypeDouble = slice

      slice :: forall a. Storable a => UniqueArray a -> Int -> UniqueArray a
      slice (UniqueArray uid (Lifetime lft w fp)) s =
        UniqueArray uid (Lifetime lft w (plusForeignPtr fp (j * s * sizeOf (undefined::a))))

  basicOverlaps (MArray ((),m) mad1) (MArray ((),n) mad2) = go (eltR @e) mad1 mad2
    where
      go :: TypeR a -> MutableArrayData a -> MutableArrayData a -> Bool
      go TupRunit           ()       ()       = False
      go (TupRpair aR1 aR2) (a1, a2) (b1, b2) = go aR1 a1 b1 || go aR2 a2 b2
      go (TupRsingle aR)    a        b        = scalar aR a b

      scalar :: ScalarType a -> MutableArrayData a -> MutableArrayData a -> Bool
      scalar (SingleScalarType t) a b = single t a b 1
      scalar (VectorScalarType t) a b = vector t a b

      vector :: VectorType a -> MutableArrayData a -> MutableArrayData a -> Bool
      vector (VectorType w t) a b
        | SingleArrayDict <- singleArrayDict t
        = single t a b w

      single :: SingleType a -> MutableArrayData a -> MutableArrayData a -> Int -> Bool
      single (NumSingleType t) = num t

      num :: NumType a -> MutableArrayData a -> MutableArrayData a -> Int -> Bool
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> MutableArrayData a -> MutableArrayData a -> Int -> Bool
      integral TypeInt    = overlaps
      integral TypeInt8   = overlaps
      integral TypeInt16  = overlaps
      integral TypeInt32  = overlaps
      integral TypeInt64  = overlaps
      integral TypeWord   = overlaps
      integral TypeWord8  = overlaps
      integral TypeWord16 = overlaps
      integral TypeWord32 = overlaps
      integral TypeWord64 = overlaps

      floating :: FloatingType a -> MutableArrayData a -> MutableArrayData a -> Int -> Bool
      floating TypeHalf   = overlaps
      floating TypeFloat  = overlaps
      floating TypeDouble = overlaps

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

  basicUnsafeNew n = unsafePrimToPrim $ MArray ((),n) <$> newArrayData (eltR @e) n

  basicInitialize (MArray ((),n) mad) = unsafePrimToPrim $ go (eltR @e) mad
    where
      go :: TypeR a -> MutableArrayData a -> IO ()
      go TupRunit           ()       = return ()
      go (TupRpair aR1 aR2) (a1, a2) = go aR1 a1 >> go aR2 a2
      go (TupRsingle aR)    a        = scalar aR a

      scalar :: ScalarType a -> MutableArrayData a -> IO ()
      scalar (SingleScalarType t) a = single t a 1
      scalar (VectorScalarType t) a = vector t a

      vector :: VectorType a -> MutableArrayData a -> IO ()
      vector (VectorType w t) a
        | SingleArrayDict <- singleArrayDict t
        = single t a w

      single :: SingleType a -> MutableArrayData a -> Int -> IO ()
      single (NumSingleType t) = num t

      num :: NumType a -> MutableArrayData a -> Int -> IO ()
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> MutableArrayData a -> Int -> IO ()
      integral TypeInt    = initialise
      integral TypeInt8   = initialise
      integral TypeInt16  = initialise
      integral TypeInt32  = initialise
      integral TypeInt64  = initialise
      integral TypeWord   = initialise
      integral TypeWord8  = initialise
      integral TypeWord16 = initialise
      integral TypeWord32 = initialise
      integral TypeWord64 = initialise

      floating :: FloatingType a -> MutableArrayData a -> Int -> IO ()
      floating TypeHalf   = initialise
      floating TypeFloat  = initialise
      floating TypeDouble = initialise

      initialise :: forall a. Storable a => UniqueArray a -> Int -> IO ()
      initialise ua s = withUniqueArrayPtr ua $ \p -> fillBytes p 0 (n * s * sizeOf (undefined::a))

  basicUnsafeRead  (MArray _ mad) i   = unsafePrimToPrim $ toElt <$> readArrayData (eltR @e) mad i
  basicUnsafeWrite (MArray _ mad) i v = unsafePrimToPrim $ writeArrayData (eltR @e) mad i (fromElt v)

  basicUnsafeCopy (MArray _ dst) (MArray ((),n) src) = unsafePrimToPrim $ go (eltR @e) dst src
    where
      go :: TypeR a -> MutableArrayData a -> MutableArrayData a -> IO ()
      go TupRunit           ()       ()       = return ()
      go (TupRpair aR1 aR2) (a1, a2) (b1, b2) = go aR1 a1 b1 >> go aR2 a2 b2
      go (TupRsingle aR)    a        b        = scalar aR a b

      scalar :: ScalarType a -> MutableArrayData a -> MutableArrayData a -> IO ()
      scalar (SingleScalarType t) a b = single t a b 1
      scalar (VectorScalarType t) a b = vector t a b

      vector :: VectorType a -> MutableArrayData a -> MutableArrayData a -> IO ()
      vector (VectorType w t) a b
        | SingleArrayDict <- singleArrayDict t
        = single t a b w

      single :: SingleType a -> MutableArrayData a -> MutableArrayData a -> Int -> IO ()
      single (NumSingleType t) = num t

      num :: NumType a -> MutableArrayData a -> MutableArrayData a -> Int -> IO ()
      num (IntegralNumType t) = integral t
      num (FloatingNumType t) = floating t

      integral :: IntegralType a -> MutableArrayData a -> MutableArrayData a -> Int -> IO ()
      integral TypeInt    = copy
      integral TypeInt8   = copy
      integral TypeInt16  = copy
      integral TypeInt32  = copy
      integral TypeInt64  = copy
      integral TypeWord   = copy
      integral TypeWord8  = copy
      integral TypeWord16 = copy
      integral TypeWord32 = copy
      integral TypeWord64 = copy

      floating :: FloatingType a -> MutableArrayData a -> MutableArrayData a -> Int -> IO ()
      floating TypeHalf   = copy
      floating TypeFloat  = copy
      floating TypeDouble = copy

      copy :: forall a. Storable a => UniqueArray a -> UniqueArray a -> Int -> IO ()
      copy uu uv s =
        withUniqueArrayPtr uu $ \u ->
        withUniqueArrayPtr uv $ \v ->
          copyBytes u v (n * s * sizeOf (undefined::a))

#if !MIN_VERSION_base(4,10,0)
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr# c) (I# i#) = ForeignPtr (plusAddr# addr# i#) c
#endif

