{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash       #-}

-- | Pattern synonyms for working with 'TupleType'
--
-- TODO: This module should be either moved to some common place where all
--       @accelerate-io-*@ packages can access them, or the patterns should just
--       be inlined

module Data.Array.Accelerate.IO.Data.Patterns where

import Data.Array.Accelerate.Type
import GHC.Base

-- TODO: Missing one of these patterns doesn't trigger a warning about
--       non-exhaustive patterns, how could this be resolved?

{-# COMPLETE TupInt, TupInt, TupInt8, TupInt8, TupInt16,
             TupInt16, TupInt32, TupInt32, TupInt64, TupInt64, TupWord, TupWord,
             TupWord8, TupWord8, TupWord16, TupWord16, TupWord32, TupWord32,
             TupWord64, TupWord64, TupHalf, TupHalf, TupFloat, TupFloat,
             TupDouble, TupDouble, TupBool, TupBool, TupChar, TupChar #-}
{-# COMPLETE TupVecInt, TupVecInt, TupVecInt8, TupVecInt8, TupVecInt16,
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
