{-# LANGUAGE GADTs #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Foreign.Internal
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Foreign.Internal
  where

import Data.Array.Accelerate.Array.Data                             ( GArrayDataR, ScalarArrayDataR )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Type

import Foreign.Ptr
import Foreign.ForeignPtr


data ScalarArrayDict a where
  ScalarArrayDict :: ( GArrayDataR Ptr a ~ Ptr (ScalarArrayDataR a)
                     , GArrayDataR ForeignPtr a ~ ForeignPtr (ScalarArrayDataR a)
                     , GArrayDataR UniqueArray a ~ UniqueArray (ScalarArrayDataR a)
                     , ScalarArrayDataR a ~ ScalarArrayDataR b )
                   => {-# UNPACK #-} !Int
                   -> SingleType b
                   -> ScalarArrayDict a

data SingleArrayDict a where
  SingleArrayDict :: ( GArrayDataR Ptr a ~ Ptr (ScalarArrayDataR a)
                     , GArrayDataR ForeignPtr a ~ ForeignPtr (ScalarArrayDataR a)
                     , GArrayDataR UniqueArray a ~ UniqueArray (ScalarArrayDataR a)
                     , ScalarArrayDataR a ~ a )
                  => SingleArrayDict a

scalarArrayDict :: ScalarType a -> ScalarArrayDict a
scalarArrayDict = scalar
  where
    scalar :: ScalarType a -> ScalarArrayDict a
    scalar (VectorScalarType t) = vector t
    scalar (SingleScalarType t)
      | SingleArrayDict <- singleArrayDict t
      = ScalarArrayDict 1 t

    vector :: VectorType a -> ScalarArrayDict a
    vector (VectorType w s)
      | SingleArrayDict <- singleArrayDict s
      = ScalarArrayDict w s

singleArrayDict :: SingleType a -> SingleArrayDict a
singleArrayDict = single
  where
    single :: SingleType a -> SingleArrayDict a
    single (NumSingleType t) = num t

    num :: NumType a -> SingleArrayDict a
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType a -> SingleArrayDict a
    integral TypeInt    = SingleArrayDict
    integral TypeInt8   = SingleArrayDict
    integral TypeInt16  = SingleArrayDict
    integral TypeInt32  = SingleArrayDict
    integral TypeInt64  = SingleArrayDict
    integral TypeWord   = SingleArrayDict
    integral TypeWord8  = SingleArrayDict
    integral TypeWord16 = SingleArrayDict
    integral TypeWord32 = SingleArrayDict
    integral TypeWord64 = SingleArrayDict

    floating :: FloatingType a -> SingleArrayDict a
    floating TypeHalf   = SingleArrayDict
    floating TypeFloat  = SingleArrayDict
    floating TypeDouble = SingleArrayDict

