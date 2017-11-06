{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Test.Array.IArray
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Array.IArray
  where

import Test.Util
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Array.Accelerate                                        as A ( Array, Shape, Elt, DIM1, DIM2 )
import Data.Array.Accelerate.Array.Sugar                            as A ( EltRepr )
import Data.Array.Accelerate.IO.Data.Array.IArray                   as A
import qualified Data.Array.Accelerate.Hedgehog.Gen.Array           as Gen
import qualified Data.Array.Accelerate.Hedgehog.Gen.Shape           as Gen

import Data.Array.IArray                                            as I hiding ( indices, elems )

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Data.Functor.Identity
import Data.Proxy
import Prelude


ix1 :: Gen (Int,Int)
ix1 = do
  lo <- Gen.int (Range.linearFrom 0 (-128) 128)
  hi <- Gen.int (Range.linear lo (lo+256))
  return (lo,hi)

ix2 :: Gen ((Int,Int), (Int,Int))
ix2 = do
  l0 <- Gen.int (Range.linearFrom 0 (-64) (64))
  l1 <- Gen.int (Range.linearFrom 0 (-64) (64))
  h0 <- Gen.int (Range.linear l0 (l0+128))
  h1 <- Gen.int (Range.linear l1 (l1+128))
  return ((l0,l1), (h0,h1))

iarray :: (Ix ix, IArray a e) => Gen (ix,ix) -> Gen e -> Gen (a ix e)
iarray ix e = do
  (lo,hi) <- ix
  let n       = rangeSize (lo,hi)
      indices = range (lo,hi)
  --
  elems <- Gen.list (Range.singleton n) e
  return $ array (lo,hi) (zip indices elems)


test_i2a
    :: forall ix sh a e. (Ix ix, IArray a e, Elt ix, Shape sh, Elt e, Show (a ix e), Eq (a ix e), IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Proxy a
    -> Proxy sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_i2a _ _ ix e =
  property $ do
    arr <- forAll (iarray ix e)
    let (lo,_) = bounds arr
    tripping arr (fromIArray :: a ix e -> A.Array sh e) (Identity . toIArray lo)

test_a2i
    :: forall ix sh a e. (Ix ix, IArray a e, Elt ix, Shape sh, Elt e, Show (a ix e), Eq (a ix e), Gen.Shape sh, Eq sh, Eq e, IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Proxy a
    -> Proxy sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_a2i _ _ ix e =
  property $ do
    sh      <- forAll (shape :: Gen sh)
    arr     <- forAll (Gen.array sh e)
    (lo,_)  <- forAll ix
    --
    tripping arr (toIArray lo :: A.Array sh e -> a ix e) (Identity . fromIArray)


test_array_iarray :: TestTree
test_array_iarray =
  testGroup "Data.Array.IArray"
    [ testGroup "iarray->accelerate"
      [ testGroup "DIM1"
        [ testProperty "Int"           $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 int
        , testProperty "Int8"          $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i8
        , testProperty "Int16"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i16
        , testProperty "Int32"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i32
        , testProperty "Int64"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i64
        , testProperty "Word"          $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 word
        , testProperty "Word8"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w8
        , testProperty "Word16"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w16
        , testProperty "Word32"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w32
        , testProperty "Word64"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w64
        , testProperty "Float"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 f32
        , testProperty "Double"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 f64
        , testProperty "Complex Float" $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 (complex f32)
        ]
      , testGroup "DIM2"
        [ testProperty "Int"           $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 int
        , testProperty "Int8"          $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i8
        , testProperty "Int16"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i16
        , testProperty "Int32"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i32
        , testProperty "Int64"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i64
        , testProperty "Word"          $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 word
        , testProperty "Word8"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w8
        , testProperty "Word16"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w16
        , testProperty "Word32"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w32
        , testProperty "Word64"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w64
        , testProperty "Float"         $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 f32
        , testProperty "Double"        $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 f64
        , testProperty "Complex Float" $ test_i2a (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 (complex f32)
        ]
      ]
    , testGroup "accelerate->iarray"
      [ testGroup "DIM1"
        [ testProperty "Int"           $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 int
        , testProperty "Int8"          $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i8
        , testProperty "Int16"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i16
        , testProperty "Int32"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i32
        , testProperty "Int64"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 i64
        , testProperty "Word"          $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 word
        , testProperty "Word8"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w8
        , testProperty "Word16"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w16
        , testProperty "Word32"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w32
        , testProperty "Word64"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 w64
        , testProperty "Float"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 f32
        , testProperty "Double"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 f64
        , testProperty "Complex Float" $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM1) ix1 (complex f32)
        ]
      , testGroup "DIM2"
        [ testProperty "Int"           $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 int
        , testProperty "Int8"          $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i8
        , testProperty "Int16"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i16
        , testProperty "Int32"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i32
        , testProperty "Int64"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 i64
        , testProperty "Word"          $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 word
        , testProperty "Word8"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w8
        , testProperty "Word16"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w16
        , testProperty "Word32"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w32
        , testProperty "Word64"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 w64
        , testProperty "Float"         $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 f32
        , testProperty "Double"        $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 f64
        , testProperty "Complex Float" $ test_a2i (Proxy::Proxy I.Array) (Proxy::Proxy DIM2) ix2 (complex f32)
        ]
      ]
    ]

