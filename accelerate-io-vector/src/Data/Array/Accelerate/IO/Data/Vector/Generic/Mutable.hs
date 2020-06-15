{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
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
import GHC.ForeignPtr

-- | Pattern synonyms for pattern matching on 'TupleType' instances.
--
-- TODO: These should be in some common place where very accelerate-io-* package
--       can access them. Should these be part of Accelerate itself?
-- TODO: Missing one of these patterns doesn't trigger a warning about
--       non-exhaustive patterns, how could this be resolved?
--

{-# COMPLETE TupRunit, TupRpair, TupInt, TupInt, TupInt8, TupInt8, TupInt16,
             TupInt16, TupInt32, TupInt32, TupInt64, TupInt64, TupWord, TupWord,
             TupWord8, TupWord8, TupWord16, TupWord16, TupWord32, TupWord32,
             TupWord64, TupWord64, TupHalf, TupHalf, TupFloat, TupFloat,
             TupDouble, TupDouble, TupBool, TupBool, TupChar, TupChar,
             TupVecInt, TupVecInt, TupVecInt8, TupVecInt8, TupVecInt16,
             TupVecInt16, TupVecInt32, TupVecInt32, TupVecInt64, TupVecInt64,
             TupVecWord, TupVecWord, TupVecWord8, TupVecWord8, TupVecWord16,
             TupVecWord16, TupVecWord32, TupVecWord32, TupVecWord64,
             TupVecWord64, TupVecHalf, TupVecHalf, TupVecFloat, TupVecFloat,
             TupVecDouble, TupVecDouble, TupVecBool, TupVecBool, TupVecChar,
             TupVecChar #-}
pattern TupInt    :: () => a ~ Int => TupleType a
pattern TupInt    = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))
pattern TupInt8   :: () => a ~ Int8 => TupleType a
pattern TupInt8   = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt8)))
pattern TupInt16  :: () => a ~ Int16 => TupleType a
pattern TupInt16  = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt16)))
pattern TupInt32  :: () => a ~ Int32 => TupleType a
pattern TupInt32  = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt32)))
pattern TupInt64  :: () => a ~ Int64 => TupleType a
pattern TupInt64  = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt64)))
pattern TupWord   :: () => a ~ Word => TupleType a
pattern TupWord   = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord)))
pattern TupWord8  :: () => a ~ Word8 => TupleType a
pattern TupWord8  = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord8)))
pattern TupWord16 :: () => a ~ Word16 => TupleType a
pattern TupWord16 = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord16)))
pattern TupWord32 :: () => a ~ Word32 => TupleType a
pattern TupWord32 = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord32)))
pattern TupWord64 :: () => a ~ Word64 => TupleType a
pattern TupWord64 = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord64)))
pattern TupHalf   :: () => a ~ Half => TupleType a
pattern TupHalf   = TupRsingle (SingleScalarType (NumSingleType (FloatingNumType TypeHalf)))
pattern TupFloat  :: () => a ~ Float => TupleType a
pattern TupFloat  = TupRsingle (SingleScalarType (NumSingleType (FloatingNumType TypeFloat)))
pattern TupDouble :: () => a ~ Double => TupleType a
pattern TupDouble = TupRsingle (SingleScalarType (NumSingleType (FloatingNumType TypeDouble)))
pattern TupBool   :: () => a ~ Bool => TupleType a
pattern TupBool   = TupRsingle (SingleScalarType (NonNumSingleType TypeBool))
pattern TupChar   :: () => a ~ Char => TupleType a
pattern TupChar   = TupRsingle (SingleScalarType (NonNumSingleType TypeChar))
-- TODO: I can't get the type checker to accept these VectorScalarTypes without
--       having to manually match all possible options, how should this be done
--       instead?
pattern TupVecInt    :: () => (a ~ Int, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecInt    n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeInt))))
pattern TupVecInt8   :: () => (a ~ Int8, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecInt8   n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeInt8))))
pattern TupVecInt16  :: () => (a ~ Int16, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecInt16  n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeInt16))))
pattern TupVecInt32  :: () => (a ~ Int32, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecInt32  n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeInt32))))
pattern TupVecInt64  :: () => (a ~ Int64, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecInt64  n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeInt64))))
pattern TupVecWord   :: () => (a ~ Word, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecWord   n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeWord))))
pattern TupVecWord8  :: () => (a ~ Word8, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecWord8  n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeWord8))))
pattern TupVecWord16 :: () => (a ~ Word16, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecWord16 n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeWord16))))
pattern TupVecWord32 :: () => (a ~ Word32, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecWord32 n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeWord32))))
pattern TupVecWord64 :: () => (a ~ Word64, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecWord64 n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (IntegralNumType TypeWord64))))
pattern TupVecHalf   :: () => (a ~ Half, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecHalf   n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (FloatingNumType TypeHalf))))
pattern TupVecFloat  :: () => (a ~ Float, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecFloat  n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (FloatingNumType TypeFloat))))
pattern TupVecDouble :: () => (a ~ Double, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecDouble n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NumSingleType (FloatingNumType TypeDouble))))
pattern TupVecBool   :: () => (a ~ Bool, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecBool   n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NonNumSingleType TypeBool)))
pattern TupVecChar   :: () => (a ~ Char, v ~ Vec n a) => Int# -> SingleType a -> TupleType v
pattern TupVecChar   n# tp <- TupRsingle (VectorScalarType (VectorType (I# n#) tp@(NonNumSingleType TypeChar)))

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

  basicUnsafeSlice j m (MArray _ mad) = MArray ((),m) (go (eltType @e) mad 1)
    where
      go :: TupleType a -> MutableArrayData a -> Int -> MutableArrayData a
      go TupRunit             ()  !_ = ()
      go TupInt               arr !s = slice arr s
      go TupInt8              arr !s = slice arr s
      go TupInt16             arr !s = slice arr s
      go TupInt32             arr !s = slice arr s
      go TupInt64             arr !s = slice arr s
      go TupWord              arr !s = slice arr s
      go TupWord8             arr !s = slice arr s
      go TupWord16            arr !s = slice arr s
      go TupWord32            arr !s = slice arr s
      go TupWord64            arr !s = slice arr s
      go TupHalf              arr !s = slice arr s
      go TupFloat             arr !s = slice arr s
      go TupDouble            arr !s = slice arr s
      go TupBool              arr !s = slice arr s
      go TupChar              arr !s = slice arr s
      go (TupVecInt n# tp)    arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt8 n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt16 n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt32 n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt64 n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord16 n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord32 n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord64 n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecHalf n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecFloat n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecDouble n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecBool n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecChar n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupRpair t1 t2) (a1, a2) !s = (go t1 a1 s, go t2 a2 s)

      slice :: forall a. Storable a => UniqueArray a -> Int -> UniqueArray a
      slice (UniqueArray uid (Lifetime lft w fp)) s =
        UniqueArray uid (Lifetime lft w (plusForeignPtr fp (j * s * sizeOf (undefined::a))))

  basicOverlaps (MArray ((), m) mad1) (MArray ((), n) mad2) = go (eltType @e) mad1 mad2 1
    where
      go :: TupleType a -> MutableArrayData a -> MutableArrayData a -> Int -> Bool
      go TupRunit             () () !_ = False
      go TupInt               a1 a2 !s = overlaps a1 a2 s
      go TupInt8              a1 a2 !s = overlaps a1 a2 s
      go TupInt16             a1 a2 !s = overlaps a1 a2 s
      go TupInt32             a1 a2 !s = overlaps a1 a2 s
      go TupInt64             a1 a2 !s = overlaps a1 a2 s
      go TupWord              a1 a2 !s = overlaps a1 a2 s
      go TupWord8             a1 a2 !s = overlaps a1 a2 s
      go TupWord16            a1 a2 !s = overlaps a1 a2 s
      go TupWord32            a1 a2 !s = overlaps a1 a2 s
      go TupWord64            a1 a2 !s = overlaps a1 a2 s
      go TupHalf              a1 a2 !s = overlaps a1 a2 s
      go TupFloat             a1 a2 !s = overlaps a1 a2 s
      go TupDouble            a1 a2 !s = overlaps a1 a2 s
      go TupBool              a1 a2 !s = overlaps a1 a2 s
      go TupChar              a1 a2 !s = overlaps a1 a2 s
      go (TupVecInt n# tp)    a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt8 n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt16 n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt32 n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt64 n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord16 n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord32 n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord64 n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecHalf n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecFloat n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecDouble n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecBool n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecChar n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupRpair t1 t2) (l1, r1) (l2, r2) !s = go t1 l1 l2 s || go t2 r1 r2 s

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

  basicUnsafeNew n = unsafePrimToPrim $ MArray ((), n) <$> newArrayData (eltType @e) n

  basicInitialize (MArray ((),n) mad) = unsafePrimToPrim $ go (eltType @e) mad 1
    where
      go :: TupleType a -> MutableArrayData a -> Int -> IO ()
      go TupRunit             ()  !_ = return ()
      go TupInt               arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupInt8              arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupInt16             arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupInt32             arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupInt64             arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupWord              arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupWord8             arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupWord16            arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupWord32            arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupWord64            arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupHalf              arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupFloat             arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupDouble            arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupBool              arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go TupChar              arr !s = initialise (unsafeUniqueArrayPtr arr) s
      go (TupVecInt n# tp)    arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt8 n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt16 n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt32 n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecInt64 n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord16 n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord32 n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecWord64 n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecHalf n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecFloat n# tp)  arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecDouble n# tp) arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecBool n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupVecChar n# tp)   arr !s = go (TupRsingle $ SingleScalarType tp) arr (I# n# * s)
      go (TupRpair t1 t2) (a1, a2) !s = go t1 a1 s >> go t2 a2 s

      initialise :: forall a. Storable a => Ptr a -> Int -> IO ()
      initialise p s = fillBytes p 0 (n * s * sizeOf (undefined::a))

  basicUnsafeRead  (MArray _ mad) i   = unsafePrimToPrim $ toElt <$> unsafeReadArrayData (eltType @e) mad i
  basicUnsafeWrite (MArray _ mad) i v = unsafePrimToPrim $ unsafeWriteArrayData (eltType @e) mad i (fromElt v)

  basicUnsafeCopy (MArray _ dst) (MArray ((), n) src) = unsafePrimToPrim $ go (eltType @e) dst src 1
    where
      go :: TupleType a -> MutableArrayData a -> MutableArrayData a -> Int -> IO ()
      go TupRunit             () () !_ = return ()
      go TupInt               a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupInt8              a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupInt16             a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupInt32             a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupInt64             a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupWord              a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupWord8             a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupWord16            a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupWord32            a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupWord64            a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupHalf              a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupFloat             a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupDouble            a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupBool              a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go TupChar              a1 a2 !s = copy (unsafeUniqueArrayPtr a1) (unsafeUniqueArrayPtr a2) s
      go (TupVecInt n# tp)    a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt8 n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt16 n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt32 n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecInt64 n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord16 n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord32 n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecWord64 n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecHalf n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecFloat n# tp)  a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecDouble n# tp) a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecBool n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupVecChar n# tp)   a1 a2 !s = go (TupRsingle $ SingleScalarType tp) a1 a2 (I# n# * s)
      go (TupRpair t1 t2) (l1, r1) (l2, r2) !s = go t1 l1 l2 s >> go t2 r1 r2 s

      copy :: forall a. Storable a => Ptr a -> Ptr a -> Int -> IO ()
      copy u v s = copyBytes u v (n * s * sizeOf (undefined::a))


#if !MIN_VERSION_base(4,10,0)
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr# c) (I# i#) = ForeignPtr (plusAddr# addr# i#) c
#endif

