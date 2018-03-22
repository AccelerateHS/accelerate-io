{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Data.Vector.Generic
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides an instance for 'Data.Vector.Generic.Vector', for
-- immutable vectors from the @vector@ package backed by Accelerate arrays.
--
-- This allows computations written with the @vector@ library to read from and
-- store into, arrays which can then be passed directly to an Accelerate
-- computation.
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.IO.Data.Vector.Generic
  where

import Data.Array.Accelerate.Array.Sugar                            as A
import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.IO.Data.Vector.Generic.Mutable         as A

import qualified Data.Vector.Generic                                as V
import qualified Data.Vector.Generic.Mutable                        as M


type instance V.Mutable Vector = MVector

instance Elt e => V.Vector Vector e where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE basicUnsafeCopy   #-}
  basicUnsafeFreeze (MArray sh mad)  = return (Array sh mad)
  basicUnsafeThaw   (Array sh ad)    = return (MArray sh ad)
  basicLength       (Array ((),n) _) = n

  basicUnsafeSlice i n (Array sh ad) =
    case M.basicUnsafeSlice i n (MArray sh ad :: MVector s e) of
      MArray sh' mad' -> Array sh' mad'

  basicUnsafeIndexM (Array _ ad) i   = return $ toElt (unsafeIndexArrayData ad i)

  basicUnsafeCopy dst (Array sh ad)  = M.basicUnsafeCopy dst (MArray sh ad)

