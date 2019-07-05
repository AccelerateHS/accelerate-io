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

import Data.Primitive.ByteArray
import Data.Serialize
import Foreign.C.Types
import qualified Data.ByteString                          as B

import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.IO.Data.ByteString


instance (Shape sh, Elt e) => Serialize (Array sh e) where
  {-# INLINE get #-}
  get = do
    sh <- toElt <$> getTupleType (eltType @sh)
    fromByteStrings sh <$> getByteStrings (arrayElt @(EltRepr e))

  {-# INLINE put #-}
  put arr = do
    putTupleType (eltType @sh) (fromElt (shape arr))
    putByteStrings (arrayElt @(EltRepr e)) (toByteStrings arr)

{-# INLINEABLE putByteStrings #-}
putByteStrings :: ArrayEltR t -> ByteStrings t -> Put
putByteStrings ArrayEltRunit           = return
putByteStrings ArrayEltRint            = put
putByteStrings ArrayEltRint8           = put
putByteStrings ArrayEltRint16          = put
putByteStrings ArrayEltRint32          = put
putByteStrings ArrayEltRint64          = put
putByteStrings ArrayEltRword           = put
putByteStrings ArrayEltRword8          = put
putByteStrings ArrayEltRword16         = put
putByteStrings ArrayEltRword32         = put
putByteStrings ArrayEltRword64         = put
putByteStrings ArrayEltRhalf           = put
putByteStrings ArrayEltRfloat          = put
putByteStrings ArrayEltRdouble         = put
putByteStrings ArrayEltRbool           = put
putByteStrings ArrayEltRchar           = put
putByteStrings (ArrayEltRvec ae)       = putByteStrings ae
putByteStrings (ArrayEltRpair ae1 ae2) = \(v1,v2) -> putByteStrings ae1 v1 >>
                                                     putByteStrings ae2 v2

{-# INLINEABLE getByteStrings #-}
getByteStrings :: ArrayEltR e -> Get (ByteStrings e)
getByteStrings ArrayEltRunit           = return ()
getByteStrings ArrayEltRint            = get
getByteStrings ArrayEltRint8           = get
getByteStrings ArrayEltRint16          = get
getByteStrings ArrayEltRint32          = get
getByteStrings ArrayEltRint64          = get
getByteStrings ArrayEltRword           = get
getByteStrings ArrayEltRword8          = get
getByteStrings ArrayEltRword16         = get
getByteStrings ArrayEltRword32         = get
getByteStrings ArrayEltRword64         = get
getByteStrings ArrayEltRhalf           = get
getByteStrings ArrayEltRfloat          = get
getByteStrings ArrayEltRdouble         = get
getByteStrings ArrayEltRbool           = get
getByteStrings ArrayEltRchar           = get
getByteStrings (ArrayEltRvec ae)       = getByteStrings ae
getByteStrings (ArrayEltRpair ae1 ae2) = (,) <$> getByteStrings ae1
                                             <*> getByteStrings ae2

putTupleType :: TupleType t -> t -> Put
putTupleType TypeRunit         ()    = return ()
putTupleType (TypeRpair ta tb) (a,b) = putTupleType ta a >> putTupleType tb b
putTupleType (TypeRscalar t)   a     = putScalarType t a

putScalarType :: ScalarType t -> t -> Put
putScalarType (SingleScalarType t) = putSingleType t
putScalarType (VectorScalarType t) = putVectorType t

putVectorType :: VectorType (Vec n a) -> Vec n a -> Put
putVectorType (VectorType n t) (Vec ba#) =
  let ba    = ByteArray ba#
      bytes = n * sizeOfSingleType t
      go !i
        | i >= bytes = return ()
        | otherwise  = put (indexByteArray ba i :: Word8) >> go (i+1)
   in
   go 0

putSingleType :: SingleType t -> t -> Put
putSingleType (NumSingleType    t) = putNumType t
putSingleType (NonNumSingleType t) = putNonNumType t

putNumType :: NumType t -> t -> Put
putNumType (IntegralNumType t) = putIntegralType t
putNumType (FloatingNumType t) = putFloatingType t

putIntegralType :: IntegralType t -> t -> Put
putIntegralType TypeInt{}    = put
putIntegralType TypeInt8{}   = put
putIntegralType TypeInt16{}  = put
putIntegralType TypeInt32{}  = put
putIntegralType TypeInt64{}  = put
putIntegralType TypeWord{}   = put
putIntegralType TypeWord8{}  = put
putIntegralType TypeWord16{} = put
putIntegralType TypeWord32{} = put
putIntegralType TypeWord64{} = put

putFloatingType :: FloatingType t -> t -> Put
putFloatingType TypeHalf{}   = \(Half (CUShort w)) -> put w
putFloatingType TypeFloat{}  = put
putFloatingType TypeDouble{} = put

putNonNumType :: NonNumType t -> t -> Put
putNonNumType TypeBool{} = put
putNonNumType TypeChar{} = put

getTupleType :: TupleType t -> Get t
getTupleType TypeRunit         = return ()
getTupleType (TypeRpair ta tb) = (,) <$> getTupleType ta <*> getTupleType tb
getTupleType (TypeRscalar t)   = getScalarType t

getScalarType :: ScalarType t -> Get t
getScalarType (SingleScalarType t) = getSingleType t
getScalarType (VectorScalarType t) = getVectorType t

-- TLM: this could be more efficient 2019-07-05
getVectorType :: VectorType (Vec n a) -> Get (Vec n a)
getVectorType (VectorType n t) = do
  let m = n * sizeOfSingleType t
  bytes  <- getBytes m
  return $! case byteArrayFromListN m (B.unpack bytes) of
              ByteArray ba# -> Vec ba#

getSingleType :: SingleType t -> Get t
getSingleType (NumSingleType    t) = getNumType t
getSingleType (NonNumSingleType t) = getNonNumType t

getNumType :: NumType t -> Get t
getNumType (IntegralNumType t) = getIntegralType t
getNumType (FloatingNumType t) = getFloatingType t

getIntegralType :: IntegralType t -> Get t
getIntegralType TypeInt{}    = get
getIntegralType TypeInt8{}   = get
getIntegralType TypeInt16{}  = get
getIntegralType TypeInt32{}  = get
getIntegralType TypeInt64{}  = get
getIntegralType TypeWord{}   = get
getIntegralType TypeWord8{}  = get
getIntegralType TypeWord16{} = get
getIntegralType TypeWord32{} = get
getIntegralType TypeWord64{} = get

getFloatingType :: FloatingType t -> Get t
getFloatingType TypeHalf{}   = Half . CUShort <$> get
getFloatingType TypeFloat{}  = get
getFloatingType TypeDouble{} = get

getNonNumType :: NonNumType t -> Get t
getNonNumType TypeBool{} = get
getNonNumType TypeChar{} = get

