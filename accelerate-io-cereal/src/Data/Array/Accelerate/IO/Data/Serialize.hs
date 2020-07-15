{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Serialize
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Orphan instance for binary serialisation of 'Array'
--

module Data.Array.Accelerate.IO.Data.Serialize ()
  where

import Data.Serialize

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Type
import qualified Data.Array.Accelerate.Representation.Array         as R

import Data.Array.Accelerate.IO.Data.ByteString


instance (Shape sh, Elt e) => Serialize (Array sh e) where
  {-# INLINE get #-}
  get = do
    sh <- getShapeR (shapeR @sh)
    bs <- getArrayR (eltR @e)
    return $ fromByteStrings (toElt sh) bs
    where
      getShapeR :: ShapeR t -> Get t
      getShapeR ShapeRz = return ()
      getShapeR (ShapeRsnoc shR) = do
        sz <- get
        sh <- getShapeR shR
        return (sh, sz)

      getArrayR :: TypeR a -> Get (ByteStrings a)
      getArrayR TupRunit           = return ()
      getArrayR (TupRpair aR1 aR2) = do
        a1 <- getArrayR aR1
        a2 <- getArrayR aR2
        return (a1, a2)
      getArrayR (TupRsingle aR) = scalar aR
        where
          scalar :: ScalarType a -> Get (ByteStrings a)
          scalar (SingleScalarType t) = single t
          scalar (VectorScalarType t) = vector t

          vector :: VectorType a -> Get (ByteStrings a)
          vector (VectorType _ t)
            | SingleArrayDict <- singleArrayDict t
            = single t

          single :: SingleType a -> Get (ByteStrings a)
          single (NumSingleType t) = num t

          num :: NumType a -> Get (ByteStrings a)
          num (IntegralNumType t) = integral t
          num (FloatingNumType t) = floating t

          integral :: IntegralType a -> Get (ByteStrings a)
          integral TypeInt    = get
          integral TypeInt8   = get
          integral TypeInt16  = get
          integral TypeInt32  = get
          integral TypeInt64  = get
          integral TypeWord   = get
          integral TypeWord8  = get
          integral TypeWord16 = get
          integral TypeWord32 = get
          integral TypeWord64 = get

          floating :: FloatingType a -> Get (ByteStrings a)
          floating TypeHalf   = get
          floating TypeFloat  = get
          floating TypeDouble = get

  {-# INLINE put #-}
  put arr@(Array (R.Array sh _)) = do
    putShapeR (shapeR @sh) sh
    putArrayR (eltR @e) (toByteStrings arr)
    where
      putShapeR :: ShapeR t -> t -> Put
      putShapeR ShapeRz          ()       = return ()
      putShapeR (ShapeRsnoc stR) (st, sz) = put sz >> putShapeR stR st

      putArrayR :: TypeR a -> ByteStrings a -> Put
      putArrayR TupRunit           ()       = return ()
      putArrayR (TupRpair aR1 aR2) (a1, a2) = putArrayR aR1 a1 >> putArrayR aR2 a2
      putArrayR (TupRsingle aR)    a        = scalar aR a
        where
          scalar :: ScalarType a -> ByteStrings a -> Put
          scalar (SingleScalarType t) = single t
          scalar (VectorScalarType t) = vector t

          vector :: VectorType a -> ByteStrings a -> Put
          vector (VectorType _ t)
            | SingleArrayDict <- singleArrayDict t
            = single t

          single :: SingleType a -> ByteStrings a -> Put
          single (NumSingleType t) = num t

          num :: NumType a -> ByteStrings a -> Put
          num (IntegralNumType t) = integral t
          num (FloatingNumType t) = floating t

          integral :: IntegralType a -> ByteStrings a -> Put
          integral TypeInt    = put
          integral TypeInt8   = put
          integral TypeInt16  = put
          integral TypeInt32  = put
          integral TypeInt64  = put
          integral TypeWord   = put
          integral TypeWord8  = put
          integral TypeWord16 = put
          integral TypeWord32 = put
          integral TypeWord64 = put

          floating :: FloatingType a -> ByteStrings a -> Put
          floating TypeHalf   = put
          floating TypeFloat  = put
          floating TypeDouble = put

