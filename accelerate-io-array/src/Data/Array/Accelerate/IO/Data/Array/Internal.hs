{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Array.Internal
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Data.Array.Internal
  where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Type


type family IxShapeRepr e where
  IxShapeRepr ()    = ()
  IxShapeRepr Int   = ((),Int)
  IxShapeRepr (t,h) = (IxShapeRepr t, h)

fromIxShapeRepr
    :: forall ix sh. (HasCallStack, IxShapeRepr (EltR ix) ~ EltR sh, Shape sh, Elt ix)
    => sh
    -> ix
fromIxShapeRepr sh = toElt (go (eltR @ix) (fromElt sh))
  where
    go :: forall ix'. TypeR ix' -> IxShapeRepr ix' -> ix'
    go TupRunit                                                                    ()     = ()
    go (TupRpair tt _)                                                             (t, h) = (go tt t, h)
    go (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) ((),h) = h
    go _ _ =
      internalError "not a valid IArray.Ix"

toIxShapeRepr
    :: forall ix sh. (HasCallStack, IxShapeRepr (EltR ix) ~ EltR sh, Shape sh, Elt ix)
    => ix
    -> sh
toIxShapeRepr ix = toElt (go (eltR @ix) (fromElt ix))
  where
    go :: forall ix'. TypeR ix' -> ix' -> IxShapeRepr ix'
    go TupRunit                                                                    ()     = ()
    go (TupRpair tt _)                                                             (t, h) = (go tt t, h)
    go (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) h      = ((),h)
    go _ _ =
      internalError "not a valid IArray.Ix"

