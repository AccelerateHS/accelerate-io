{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Array.Internal
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Data.Array.Internal
  where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type


type family IxShapeRepr e where
  IxShapeRepr ()    = ()
  IxShapeRepr Int   = ((),Int)
  IxShapeRepr (t,h) = (IxShapeRepr t, h)

fromIxShapeRepr :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix) => sh -> ix
fromIxShapeRepr sh = toElt (go (eltType @ix) (fromElt sh))
  where
    go :: forall ix'. TupleType ix' -> IxShapeRepr ix' -> ix'
    go TypeRunit                                                                    ()     = ()
    go (TypeRpair tt _)                                                             (t, h) = (go tt t, h)
    go (TypeRscalar (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) ((),h) = h
    go _ _ =
      $internalError "fromIxShapeRepr" "not a valid IArray.Ix"

toIxShapeRepr :: forall ix sh. (IxShapeRepr (EltRepr ix) ~ EltRepr sh, Shape sh, Elt ix) => ix -> sh
toIxShapeRepr ix = toElt (go (eltType @ix) (fromElt ix))
  where
    go :: forall ix'. TupleType ix' -> ix' -> IxShapeRepr ix'
    go TypeRunit                                                                    ()     = ()
    go (TypeRpair tt _)                                                             (t, h) = (go tt t, h)
    go (TypeRscalar (SingleScalarType (NumSingleType (IntegralNumType TypeInt{})))) h      = ((),h)
    go _ _ =
      $internalError "toIxShapeRepr" "not a valid IArray.Ix"

