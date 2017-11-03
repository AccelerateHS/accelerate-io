{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.IArray
-- Copyright   : [2016] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Convert immutable arrays of the <https://hackage.haskell.org/package/array
-- array> library into Accelerate 'Array's.
--

module Data.Array.Accelerate.IO.IArray (

  -- ** 'Data.Array.IArray.IArray'
  fromIArray,
  toIArray,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.IArray                                        ( IArray )
import qualified Data.Array.IArray                              as IArray


-- | /O(n)/. Convert an 'IArray' to an Accelerate 'Array'.
--
-- While the type signature mentions Accelerate internals, in practice
-- satisfying the type equality is straight forward. The index type @ix@ must be
-- the unit type @()@ for singleton arrays, or an @Int@ or tuple of @Int@'s for
-- multidimensional arrays. For example:
--
-- > DIM0 ~ ()
-- > DIM1 ~ Int
-- > DIM2 ~ (Int,Int)
-- > DIM3 ~ (Int,Int,Int)
--
fromIArray
    :: (IxShapeRepr (EltRepr ix) ~ EltRepr sh, IArray a e, IArray.Ix ix, Shape sh, Elt ix, Elt e)
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
    offset ix0 ix = toElt $ go (eltType (undefined::sh)) (fromElt ix0) (fromElt ix)
      where
        go :: TupleType ix -> ix -> ix -> ix
        go UnitTuple                                                 ()       ()    = ()
        go (PairTuple tl tr)                                         (l0, r0) (l,r) = (go tl l0 l, go tr r0 r)
        go (SingleTuple (NumScalarType (IntegralNumType TypeInt{}))) i0       i     = i0+i
        go _ _ _
          = error "Data.Array.Accelerate.IO.IArray: error in index offset"


-- | /O(n)/. Convert an Accelerate 'Array' to an 'IArray'.
--
-- See 'fromIArray' for a discussion on the expected shape types.
--
toIArray
    :: (IxShapeRepr (EltRepr ix) ~ EltRepr sh, IArray a e, IArray.Ix ix, Shape sh, Elt ix)
    => Array sh e
    -> a ix e
toIArray arr = IArray.array bnds [(ix, arr ! toIxShapeRepr ix) | ix <- IArray.range bnds]
  where
    (lo,hi) = shapeToRange (shape arr)
    bnds    = (fromIxShapeRepr lo, fromIxShapeRepr hi)


type family IxShapeRepr e where
  IxShapeRepr ()    = ()
  IxShapeRepr Int   = ((),Int)
  IxShapeRepr (t,h) = (IxShapeRepr t, h)

fromIxShapeRepr :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix) => sh -> ix
fromIxShapeRepr sh = toElt (go (eltType (undefined::ix)) (fromElt sh))
  where
    go :: forall ix'. TupleType ix' -> IxShapeRepr ix' -> ix'
    go UnitTuple ()                                                         = ()
    go (SingleTuple     (NumScalarType (IntegralNumType TypeInt{}))) ((),h) = h
    go (PairTuple tt _) (t, h)                                              = (go tt t, h)
    go _ _ = error "Data.Array.Accelerate.IO.IArray: not a valid IArray.Ix"

toIxShapeRepr :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix) => ix -> sh
toIxShapeRepr ix = toElt (go (eltType (undefined::ix)) (fromElt ix))
  where
    go :: forall ix'. TupleType ix' -> ix' -> IxShapeRepr ix'
    go UnitTuple        ()                                             = ()
    go (SingleTuple     (NumScalarType (IntegralNumType TypeInt{}))) h = ((), h)
    go (PairTuple tt _) (t, h)                                         = (go tt t, h)
    go _ _ = error "Data.Array.Accelerate.IO.IArray: not a valid IArray.Ix"

