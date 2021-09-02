{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Array.IArray
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Convert between immutable 'IArray's and Accelerate 'Array's.
--

module Data.Array.Accelerate.IO.Data.Array.IArray (

  IxShapeRepr,
  fromIArray,
  toIArray,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.IO.Data.Array.Internal

import Data.Array.IArray                                        ( IArray )
import qualified Data.Array.IArray                              as IArray


-- | /O(n)/. Convert an 'IArray' to an Accelerate 'Array'.
--
-- The index type @ix@ of the 'IArray' corresponds to the shape @sh@ of the
-- Accelerate 'Array' in the following way:
--
-- > DIM0 ~ ()
-- > DIM1 ~ Int
-- > DIM2 ~ (Int,Int)
-- > DIM3 ~ (Int,Int,Int)
--
-- ...and so forth.
--
{-# INLINE fromIArray #-}
fromIArray
    :: (HasCallStack, IxShapeRepr (EltR ix) ~ EltR sh, IArray a e, IArray.Ix ix, Shape sh, Elt ix, Elt e)
    => a ix e
    -> Array sh e
fromIArray iarr = fromFunction sh (\ix -> iarr IArray.! fromIxShapeRepr (offset lo' ix))
  where
    (lo,hi) = IArray.bounds iarr
    lo'     = toIxShapeRepr lo
    hi'     = toIxShapeRepr hi
    sh      = rangeToShape (lo', hi')

    -- IArray does not necessarily start indexing from zero. Thus, we need to
    -- add some offset to the Accelerate indices to map them onto the valid
    -- index range of the IArray
    --
    offset :: forall sh. Shape sh => sh -> sh -> sh
    offset ix0 ix = toElt $ go (eltR @sh) (fromElt ix0) (fromElt ix)
      where
        go :: TypeR ix -> ix -> ix -> ix
        go TupRunit                                                                    ()       ()    = ()
        go (TupRpair tl tr)                                                            (l0, r0) (l,r) = (go tl l0 l, go tr r0 r)
        go (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) i0       i     = i0+i
        go _ _ _ =
          internalError "error in index offset"


-- | /O(n)/. Convert an Accelerate 'Array' to an 'IArray'.
--
-- See 'fromIArray' for a discussion on the expected shape types.
--
{-# INLINE toIArray #-}
toIArray
    :: forall ix sh a e. (HasCallStack, IxShapeRepr (EltR ix) ~ EltR sh, IArray a e, IArray.Ix ix, Shape sh, Elt e, Elt ix)
    => Maybe ix           -- ^ if 'Just' this as the index lower bound, otherwise the array is indexed from zero
    -> Array sh e
    -> a ix e
toIArray mix0 arr = IArray.array bnds0 [(offset ix, arr ! toIxShapeRepr ix) | ix <- IArray.range bnds]
  where
    (u,v)         = shapeToRange (shape arr)
    bnds@(lo,hi)  = (fromIxShapeRepr u, fromIxShapeRepr v)
    bnds0         = (offset lo, offset hi)

    offset :: ix -> ix
    offset ix =
      case mix0 of
        Nothing  -> ix
        Just ix0 -> offset' ix0 ix

    offset' :: ix -> ix -> ix
    offset' ix0 ix
      = fromIxShapeRepr
      . (toElt :: EltR sh -> sh)
      $ go (eltR @sh) (fromElt (toIxShapeRepr ix0 :: sh)) (fromElt (toIxShapeRepr ix :: sh))
      where
        go :: TypeR sh' -> sh' -> sh' -> sh'
        go TupRunit                                                                    ()       ()    = ()
        go (TupRpair tl tr)                                                            (l0,r0)  (l,r) = (go tl l0 l, go tr r0 r)
        go (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) i0       i     = i0+i
        go _ _ _ =
          internalError "error in index offset"

