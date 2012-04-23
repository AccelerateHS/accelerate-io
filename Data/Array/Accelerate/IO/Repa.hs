{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |
-- Module      : Data.Array.Accelerate.IO.Repa
-- Copyright   : [2012] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.IO.Repa (

  A, Shapes,
  fromRepa, toRepa,
  computeAccS, computeAccP

) where

import Control.Monad
import Control.Monad.ST.Unsafe

import qualified Data.Array.Repa                        as R
import qualified Data.Array.Repa.Eval                   as R
import qualified Data.Array.Accelerate.Array.Data       as A
import qualified Data.Array.Accelerate.Array.Sugar      as A


-- | Index conversion and equivalence statement between Repa and Accelerate
-- array shapes. That is, a n-dimensional Repa array will produce an
-- n-dimensional accelerate array of the same extent, and vice-versa.
--
class (R.Shape r, A.Shape a) => Shapes r a | a -> r, r -> a where
  -- these are really equivalent representations, so unsafeCoerce would probably
  -- work, but bad programmers get no cookies.
  toR   :: a -> r
  toA   :: r -> a

instance Shapes R.Z A.Z where
  {-# INLINE toR #-}
  toR A.Z = R.Z
  {-# INLINE toA #-}
  toA R.Z = A.Z

instance Shapes sr sa => Shapes (sr R.:. Int) (sa A.:. Int) where
  {-# INLINE toR #-}
  toR (sa A.:. sz) = toR sa R.:. sz
  {-# INLINE toA #-}
  toA (sr R.:. sz) = toA sr A.:. sz


-- | An implementation based on `Data.Array.Accelerate` arrays. The Accelerate
-- array implementation is based on type families and picks an efficient,
-- unboxed representation for every element type. Moreover, these arrays can be
-- handed efficiently (without copying) to Accelerate programs for, example,
-- further parallel computation on the GPU.
--
data A
data instance R.Array A sh e
  = AAccelerate {-# UNPACK #-} !sh
                {-# UNPACK #-} !(A.ArrayData (A.EltRepr e))

-- Repr ------------------------------------------------------------------------

-- | Reading elements of the Accelerate array
--
instance A.Elt e => R.Repr A e where
  {-# INLINE extent #-}
  extent (AAccelerate sh _)
    = sh

  {-# INLINE linearIndex #-}
  linearIndex (AAccelerate sh adata) ix
    | ix >= 0 && ix < R.size sh
    = A.toElt (adata `A.indexArrayData` ix)

    | otherwise
    = error "Repa: accelerate array out of bounds"

  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearIndex (AAccelerate _ adata) ix
    = A.toElt (adata `A.indexArrayData` ix)

  {-# INLINE deepSeqArray #-}
  deepSeqArray (AAccelerate sh adata) x
    = sh `R.deepSeq` adata `seq` x


-- | Filling Accelerate arrays
--
instance A.Elt e => R.Fillable A e where
  data MArr A e
    = forall s. MAArr (A.MutableArrayData s (A.EltRepr e))

  {-# INLINE newMArr #-}
  newMArr n
    = MAArr `liftM` unsafeSTToIO (A.newArrayData n)

  {-# INLINE unsafeWriteMArr #-}
  unsafeWriteMArr (MAArr mad) n e
    = unsafeSTToIO
    $ A.writeArrayData mad n (A.fromElt e)

  {-# INLINE unsafeFreezeMArr #-}
  unsafeFreezeMArr sh (MAArr mad)
    = do adata  <- unsafeSTToIO $ A.unsafeFreezeArrayData mad
         return $! AAccelerate sh adata

-- Conversions -----------------------------------------------------------------

-- | /O(1)/. Wrap an Accelerate array.
--
toRepa
    :: Shapes sh sh'
    => A.Array sh' e -> R.Array A sh e
{-# INLINE toRepa #-}
toRepa arr@(A.Array _ adata)
  = AAccelerate (toR (A.shape arr)) adata

-- | /O(1)/. Unpack to an Accelerate array.
--
fromRepa
    :: (Shapes sh sh', A.Elt e)
    => R.Array A sh e -> A.Array sh' e
{-# INLINE fromRepa #-}
fromRepa (AAccelerate sh adata)
  = A.Array (A.fromElt (toA sh)) adata


-- Computations ----------------------------------------------------------------

-- | Sequential computation of array elements
--
computeAccS
    :: R.Fill r A sh e
    => R.Array r sh e -> R.Array A sh e
{-# INLINE computeAccS #-}
computeAccS = R.computeS

-- | Parallel computation of array elements
--
computeAccP
    :: (A.Elt e, R.Fill r A sh e, Monad m)
    => R.Array r sh e -> m (R.Array A sh e)
{-# INLINE computeAccP #-}
computeAccP = R.computeP
