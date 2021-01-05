{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Codec.Serialise
-- Copyright   : [2012..2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Orphan instance for binary serialisation of 'Array'
--

module Data.Array.Accelerate.IO.Codec.Serialise ()
  where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Representation.Shape                   ( ShapeR(..) )
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R
import qualified Data.Array.Accelerate.Representation.Shape         as R

import Data.Array.Accelerate.IO.Data.ByteString


instance (Shape sh, Elt e) => Serialise (Array sh e) where
  {-# INLINE encode #-}
  encode arr@(Array arrR)
    =  encodeListLen (fromIntegral (R.rank (shapeR @sh)) + fieldsArrayR (eltR @e))
    <> encodeShapeR (shapeR @sh) (R.shape arrR)
    <> encodeArrayR (eltR @e) (toByteStrings arr)
    where
      encodeShapeR :: ShapeR t -> t -> Encoding
      encodeShapeR ShapeRz          ()       = mempty
      encodeShapeR (ShapeRsnoc shR) (sh, sz) = encodeShapeR shR sh <> encodeInt sz

      encodeArrayR :: TypeR t -> ByteStrings t -> Encoding
      encodeArrayR TupRunit           ()       = mempty
      encodeArrayR (TupRpair aR1 aR2) (a1, a2) = encodeArrayR aR1 a1 <> encodeArrayR aR2 a2
      encodeArrayR (TupRsingle aR)    a        = scalar aR a
        where
          scalar :: ScalarType t -> ByteStrings t -> Encoding
          scalar (SingleScalarType t) = single t
          scalar (VectorScalarType t) = vector t

          vector :: VectorType t -> ByteStrings t -> Encoding
          vector (VectorType _ t)
            | SingleArrayDict <- singleArrayDict t
            = single t

          single :: SingleType t -> ByteStrings t -> Encoding
          single (NumSingleType t) = num t

          num :: NumType t -> ByteStrings t -> Encoding
          num (IntegralNumType t) = integral t
          num (FloatingNumType t) = floating t

          integral :: IntegralType t -> ByteStrings t -> Encoding
          integral TypeInt    = encodeBytes
          integral TypeInt8   = encodeBytes
          integral TypeInt16  = encodeBytes
          integral TypeInt32  = encodeBytes
          integral TypeInt64  = encodeBytes
          integral TypeWord   = encodeBytes
          integral TypeWord8  = encodeBytes
          integral TypeWord16 = encodeBytes
          integral TypeWord32 = encodeBytes
          integral TypeWord64 = encodeBytes

          floating :: FloatingType t -> ByteStrings t -> Encoding
          floating TypeHalf   = encodeBytes
          floating TypeFloat  = encodeBytes
          floating TypeDouble = encodeBytes

      fieldsArrayR :: TypeR t -> Word
      fieldsArrayR TupRunit           = 0
      fieldsArrayR TupRsingle{}       = 1
      fieldsArrayR (TupRpair eR1 eR2) = fieldsArrayR eR1 + fieldsArrayR eR2

  {-# INLINE decode #-}
  decode = do
    _  <- decodeListLen
    sh <- decodeShapeR (shapeR @sh)
    bs <- decodeArrayR (eltR @e)
    return $! fromByteStrings (toElt sh) bs
    where
      decodeShapeR :: ShapeR t -> Decoder s t
      decodeShapeR ShapeRz          = return ()
      decodeShapeR (ShapeRsnoc shR) = do
        sh <- decodeShapeR shR
        sz <- decodeInt
        return (sh, sz)

      decodeArrayR :: TypeR t -> Decoder s (ByteStrings t)
      decodeArrayR TupRunit           = return ()
      decodeArrayR (TupRpair aR1 aR2) = do
        a1 <- decodeArrayR aR1
        a2 <- decodeArrayR aR2
        return (a1, a2)
      decodeArrayR (TupRsingle aR) = scalar aR
        where
          scalar :: ScalarType t -> Decoder s (ByteStrings t)
          scalar (SingleScalarType t) = single t
          scalar (VectorScalarType t) = vector t

          vector :: VectorType t -> Decoder s (ByteStrings t)
          vector (VectorType _ t)
            | SingleArrayDict <- singleArrayDict t
            = single t

          single :: SingleType t -> Decoder s (ByteStrings t)
          single (NumSingleType t) = num t

          num :: NumType t -> Decoder s (ByteStrings t)
          num (IntegralNumType t) = integral t
          num (FloatingNumType t) = floating t

          integral :: IntegralType t -> Decoder s (ByteStrings t)
          integral TypeInt    = decodeBytes
          integral TypeInt8   = decodeBytes
          integral TypeInt16  = decodeBytes
          integral TypeInt32  = decodeBytes
          integral TypeInt64  = decodeBytes
          integral TypeWord   = decodeBytes
          integral TypeWord8  = decodeBytes
          integral TypeWord16 = decodeBytes
          integral TypeWord32 = decodeBytes
          integral TypeWord64 = decodeBytes

          floating :: FloatingType t -> Decoder s (ByteStrings t)
          floating TypeHalf   = decodeBytes
          floating TypeFloat  = decodeBytes
          floating TypeDouble = decodeBytes

