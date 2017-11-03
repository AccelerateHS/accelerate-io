{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Unboxed
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Efficient conversion between 'Data.Vector.Unboxed' vectors and Accelerate
-- 'Array's.
--

module Data.Array.Accelerate.IO.Data.Vector.Unboxed (

  Vectors,
  toVectors,
  fromVectors,

) where

import Data.Primitive                                               ( sizeOf )
import Data.Vector.Unboxed
import Data.Vector.Unboxed.Base
import qualified Data.Vector.Primitive                              as P

import Data.Array.Accelerate.IO.Data.Vector.Internal

import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar                            hiding ( Vector )
import Data.Array.Accelerate.Array.Unique
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.Array.Representation         as R

import Data.Int
import Data.Word
import System.IO.Unsafe

-- | A family of types which represent a collection of Primitive Vectors. The
-- structure of the collection depends on the element type @e@ of the
-- corresponding Accelerate array.
--
type family Vectors e :: *

type instance Vectors ()     = ()
type instance Vectors Int    = Vector Int
type instance Vectors Int8   = Vector Int8
type instance Vectors Int16  = Vector Int16
type instance Vectors Int32  = Vector Int32
type instance Vectors Int64  = Vector Int64
type instance Vectors Word   = Vector Word
type instance Vectors Word8  = Vector Word8
type instance Vectors Word16 = Vector Word16
type instance Vectors Word32 = Vector Word32
type instance Vectors Word64 = Vector Word64
type instance Vectors Float  = Vector Float
type instance Vectors Double = Vector Double
type instance Vectors Char   = Vector Char
type instance Vectors Bool   = Vector Bool
type instance Vectors (a,b)  = (Vectors a, Vectors b)


-- | /O(n)/ (typically). Convert a collection of unboxed vectors into an
-- Accelerate array.
--
-- Remember that unboxed vectors can be zipped and unzipped in O(1), so creating
-- the required 'Vectors' structure is free.
--
-- If the underlying vectors are pinned then this can be done without copying.
--
-- @since 1.1.0.0@
--
fromVectors :: (Shape sh, Elt e) => sh -> Vectors (EltRepr e) -> Array sh e
fromVectors sh vecs = Array (fromElt sh) (aux arrayElt vecs)
  where
    wrap :: forall a. P.Prim a => P.Vector a -> UniqueArray a
    wrap (P.Vector o l ba)
      = $boundsCheck "fromVectors" "shape mismatch" (size sh == l)
      $ unsafePerformIO $ newUniqueArray =<< foreignPtrOfByteArray o (l * sizeOf (undefined::a)) ba
    --
    aux :: ArrayEltR e -> Vectors e -> ArrayData e
    aux ArrayEltRunit           ()           = AD_Unit
    aux ArrayEltRint            (V_Int v)    = AD_Int    (wrap v)
    aux ArrayEltRint8           (V_Int8 v)   = AD_Int8   (wrap v)
    aux ArrayEltRint16          (V_Int16 v)  = AD_Int16  (wrap v)
    aux ArrayEltRint32          (V_Int32 v)  = AD_Int32  (wrap v)
    aux ArrayEltRint64          (V_Int64 v)  = AD_Int64  (wrap v)
    aux ArrayEltRword           (V_Word v)   = AD_Word   (wrap v)
    aux ArrayEltRword8          (V_Word8 v)  = AD_Word8  (wrap v)
    aux ArrayEltRword16         (V_Word16 v) = AD_Word16 (wrap v)
    aux ArrayEltRword32         (V_Word32 v) = AD_Word32 (wrap v)
    aux ArrayEltRword64         (V_Word64 v) = AD_Word64 (wrap v)
    aux ArrayEltRfloat          (V_Float v)  = AD_Float  (wrap v)
    aux ArrayEltRdouble         (V_Double v) = AD_Double (wrap v)
    aux ArrayEltRchar           (V_Char v)   = AD_Char   (wrap v)
    aux ArrayEltRbool           (V_Bool v)   = AD_Bool   (wrap v)
    aux (ArrayEltRpair ad1 ad2) (v1,v2)      = AD_Pair (aux ad1 v1) (aux ad2 v2)
    --
    aux _ _ = $internalError "fromVectors" "unsupported type"


-- | /O(1)/ (typically). Convert an Accelerate array into a collection of
-- unboxed vectors.
--
-- If the array data was allocated by Accelerate, this can typically be done
-- without copying.
--
-- Remember that unboxed vectors can be zipped and unzipped in O(1), so you can
-- convert the result of this function into a more natural unboxed tuple
-- representation for free.
--
-- @since 1.1.0.0@
--
toVectors :: (Shape sh, Elt e) => Array sh e -> Vectors (EltRepr e)
toVectors (Array sh adata) = aux arrayElt adata
  where
    n :: Int
    n = R.size sh

    wrap :: forall a. P.Prim a => UniqueArray a -> P.Vector a
    wrap ua = P.Vector 0 n $ unsafePerformIO
            $ byteArrayOfForeignPtr (n * sizeOf (undefined::a)) (unsafeGetValue (uniqueArrayData ua))

    aux :: ArrayEltR e -> ArrayData e -> Vectors e
    aux ArrayEltRunit           AD_Unit         = ()
    aux ArrayEltRint            (AD_Int v)      = V_Int    (wrap v)
    aux ArrayEltRint8           (AD_Int8 v)     = V_Int8   (wrap v)
    aux ArrayEltRint16          (AD_Int16 v)    = V_Int16  (wrap v)
    aux ArrayEltRint32          (AD_Int32 v)    = V_Int32  (wrap v)
    aux ArrayEltRint64          (AD_Int64 v)    = V_Int64  (wrap v)
    aux ArrayEltRword           (AD_Word v)     = V_Word   (wrap v)
    aux ArrayEltRword8          (AD_Word8 v)    = V_Word8  (wrap v)
    aux ArrayEltRword16         (AD_Word16 v)   = V_Word16 (wrap v)
    aux ArrayEltRword32         (AD_Word32 v)   = V_Word32 (wrap v)
    aux ArrayEltRword64         (AD_Word64 v)   = V_Word64 (wrap v)
    aux ArrayEltRfloat          (AD_Float v)    = V_Float  (wrap v)
    aux ArrayEltRdouble         (AD_Double v)   = V_Double (wrap v)
    aux ArrayEltRchar           (AD_Char v)     = V_Char   (wrap v)
    aux ArrayEltRbool           (AD_Bool v)     = V_Bool   (wrap v)
    aux (ArrayEltRpair ad1 ad2) (AD_Pair v1 v2) = (aux ad1 v1, aux ad2 v2)
    --
    aux _ _ = $internalError "toVectors" "unsupported type"


