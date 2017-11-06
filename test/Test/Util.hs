{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Test.Util
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Util where

import Data.Array.Accelerate                                        ( Shape, Arrays, Acc )
import Data.Array.Accelerate.Trafo                                  ( Afunction, AfunctionR )
import Data.Array.Accelerate.Array.Sugar                            ( rank )
import Data.Array.Accelerate.Data.Complex
import qualified Data.Array.Accelerate.Hedgehog.Gen.Shape           as Gen

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Data.Int
import Data.Word
import Prelude                                                      as P


type Run  = forall a. Arrays a => Acc a -> a
type RunN = forall f. Afunction f => f -> AfunctionR f

floating :: P.RealFloat a => Gen a
floating = Gen.realFloat (Range.linearFracFrom 0 (-1) 1)

complex :: Gen a -> Gen (Complex a)
complex f = (:+) <$> f <*> f

shape :: forall sh. (Gen.Shape sh, Shape sh) => Gen sh
shape = Gen.shape (Range.linear 1 (512 `quot` (2 ^ r)))
  where
    r = rank (undefined::sh)


int :: Gen Int
int = Gen.int Range.linearBounded

i8 :: Gen Int8
i8 = Gen.int8 Range.linearBounded

i16 :: Gen Int16
i16 = Gen.int16 Range.linearBounded

i32 :: Gen Int32
i32 = Gen.int32 Range.linearBounded

i64 :: Gen Int64
i64 = Gen.int64 Range.linearBounded

word :: Gen Word
word = Gen.word Range.linearBounded

w8 :: Gen Word8
w8 = Gen.word8 Range.linearBounded

w16 :: Gen Word16
w16 = Gen.word16 Range.linearBounded

w32 :: Gen Word32
w32 = Gen.word32 Range.linearBounded

w64 :: Gen Word64
w64 = Gen.word64 Range.linearBounded

f32 :: Gen Float
f32 = Gen.float (Range.linearFracFrom 0 flt_min flt_max)

f64 :: Gen Double
f64 = Gen.double (Range.linearFracFrom 0 flt_min flt_max)

flt_max :: RealFloat a => a
flt_max = x
  where
    n      = floatDigits x
    b      = floatRadix x
    (_, u) = floatRange x
    x      = encodeFloat (b^n - 1) (u - n)

flt_min :: RealFloat a => a
flt_min = x
  where
    n      = floatDigits x
    b      = floatRadix x
    (l, _) = floatRange x
    x      = encodeFloat (b^n - 1) (l - n - 1)

