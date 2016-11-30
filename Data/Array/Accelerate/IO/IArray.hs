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


-- | Convert an 'IArray' to an accelerated array.
--
-- While the type signature mentions Accelerate internals that are not exported,
-- in practice satisfying the type equality is straight forward. The index type
-- @ix@ must be the unit type @()@ for singleton arrays, or an @Int@ or tuple of
-- @Int@'s for multidimensional arrays.
--
fromIArray
    :: (IxShapeRepr (EltRepr ix) ~ EltRepr sh, IArray a e, IArray.Ix ix, Shape sh, Elt ix, Elt e)
    => a ix e
    -> Array sh e
fromIArray iarr = fromFunction sh (\ix -> iarr IArray.! fromIxShapeRepr ix)
  where
    (lo,hi) = IArray.bounds iarr
    sh      = rangeToShape (toIxShapeRepr lo, toIxShapeRepr hi)

-- | Convert an accelerated array to an 'IArray'.
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
    go _ _ = error "Not a valid IArray.Ix"

toIxShapeRepr :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix) => ix -> sh
toIxShapeRepr ix = toElt (go (eltType (undefined::ix)) (fromElt ix))
  where
    go :: forall ix'. TupleType ix' -> ix' -> IxShapeRepr ix'
    go UnitTuple        ()                                             = ()
    go (SingleTuple     (NumScalarType (IntegralNumType TypeInt{}))) h = ((), h)
    go (PairTuple tt _) (t, h)                                         = (go tt t, h)
    go _ _ = error "Not a valid IArray.Ix"

