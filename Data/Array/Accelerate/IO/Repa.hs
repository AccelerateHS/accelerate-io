{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
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

  -- ** Data.Array.Repa
  --
  -- | This provides an efficient non-copying Repa manifest array representation
  -- that can be passed directly to Accelerate.
  --
  -- The standard rules for dealing with manifest Repa arrays apply:
  --
  --  * If you want to have Repa 'R.computeP' directly into an Accelerate array,
  --    the source array must have a delayed representation.
  --
  --  * If you want to copy between manifest arrays, use 'R.copyP' instead.
  --
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
-- n-dimensional Accelerate array of the same extent, and vice-versa.
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


-- | The representation tag for manifest arrays based on Data.Array.Accelerate.
--
-- The Accelerate array implementation is based on type families and picks an
-- efficient, unboxed representation for every element type. Moreover, these
-- arrays can be handed efficiently (without copying) to Accelerate programs
-- for further computation.
--
data A

-- Repr ------------------------------------------------------------------------

-- | Reading elements of the Accelerate array
--
instance A.Elt e => R.Source A e where
  data Array A sh e
    = AAccelerate !sh !(A.ArrayData (A.EltRepr e))


  {-# INLINE extent #-}
  extent (AAccelerate sh _)
    = sh

  {-# INLINE linearIndex #-}
  linearIndex (AAccelerate sh adata) ix
    | ix >= 0 && ix < R.size sh
    = A.toElt (adata `A.unsafeIndexArrayData` ix)

    | otherwise
    = error "Repa: accelerate array out of bounds"

  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearIndex (AAccelerate _ adata) ix
    = A.toElt (adata `A.unsafeIndexArrayData` ix)

  {-# INLINE deepSeqArray #-}
  deepSeqArray (AAccelerate sh adata) x
    = sh `R.deepSeq` adata `seq` x


-- | Filling Accelerate arrays
--
instance A.Elt e => R.Target A e where
  data MVec A e
    = forall s. MAVec (A.MutableArrayData s (A.EltRepr e))

  {-# INLINE newMVec #-}
  newMVec n
    = MAVec `liftM` unsafeSTToIO (A.newArrayData n)

  {-# INLINE unsafeWriteMVec #-}
  unsafeWriteMVec (MAVec mad) n e
    = unsafeSTToIO
    $ A.unsafeWriteArrayData mad n (A.fromElt e)

  {-# INLINE unsafeFreezeMVec #-}
  unsafeFreezeMVec sh (MAVec mad)
    = do adata  <- unsafeSTToIO $ A.unsafeFreezeArrayData mad
         return $! AAccelerate sh adata

  {-# INLINE deepSeqMVec #-}
  deepSeqMVec (MAVec arr) x             -- maybe?
    = arr `seq` x

  {-# INLINE touchMVec #-}
  touchMVec _                           -- maybe?
    = return ()


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
    :: (R.Load r sh e, A.Elt e)
    => R.Array r sh e -> R.Array A sh e
{-# INLINE computeAccS #-}
computeAccS = R.computeS

-- | Parallel computation of array elements
--
computeAccP
    :: (R.Load r sh e, A.Elt e, Monad m)
    => R.Array r sh e
    -> m (R.Array A sh e)
{-# INLINE computeAccP #-}
computeAccP = R.computeP

