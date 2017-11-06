{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Test.Array.Unboxed
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Array.Unboxed
  where

import Test.Array.IArray

import Test.Util
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Array.Accelerate                                        as A ( Array, Shape, Elt, DIM1, DIM2 )
import Data.Array.Accelerate.Array.Sugar                            as A ( EltRepr )
import Data.Array.Accelerate.IO.Data.Array.Unboxed                  as A
import qualified Data.Array.Accelerate.Hedgehog.Gen.Array           as Gen
import qualified Data.Array.Accelerate.Hedgehog.Gen.Shape           as Gen

import Data.Array.Unboxed                                           as U

import Hedgehog

import Data.Proxy
import Data.Functor.Identity


test_u2a
    :: forall ix sh e. (Ix ix, IArray UArray e, Elt ix, Shape sh, Elt e, Show (UArray ix e), Eq (UArray ix e), IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Proxy sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_u2a _ ix e =
  property $ do
    arr <- forAll (iarray ix e)
    let (lo,_) = bounds arr
    tripping arr (fromUArray :: UArray ix e -> A.Array sh e) (Identity . toUArray lo)

test_a2u
    :: forall ix sh e. (Ix ix, IArray UArray e, Elt ix, Shape sh, Elt e, Show (UArray ix e), Eq (UArray ix e), Gen.Shape sh, Eq sh, Eq e, IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Proxy sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_a2u _ ix e =
  property $ do
    sh      <- forAll (shape :: Gen sh)
    arr     <- forAll (Gen.array sh e)
    (lo,_)  <- forAll ix
    --
    tripping arr (toUArray lo :: A.Array sh e -> UArray ix e) (Identity . fromUArray)

test_array_unboxed :: TestTree
test_array_unboxed =
  testGroup "Data.Array.Unboxed"
    [ testGroup "uarray->accelerate"
      [ testGroup "DIM1"
        [ testProperty "Int"    $ test_u2a (Proxy::Proxy DIM1) ix1 int
        , testProperty "Int8"   $ test_u2a (Proxy::Proxy DIM1) ix1 i8
        , testProperty "Int16"  $ test_u2a (Proxy::Proxy DIM1) ix1 i16
        , testProperty "Int32"  $ test_u2a (Proxy::Proxy DIM1) ix1 i32
        , testProperty "Int64"  $ test_u2a (Proxy::Proxy DIM1) ix1 i64
        , testProperty "Word"   $ test_u2a (Proxy::Proxy DIM1) ix1 word
        , testProperty "Word8"  $ test_u2a (Proxy::Proxy DIM1) ix1 w8
        , testProperty "Word16" $ test_u2a (Proxy::Proxy DIM1) ix1 w16
        , testProperty "Word32" $ test_u2a (Proxy::Proxy DIM1) ix1 w32
        , testProperty "Word64" $ test_u2a (Proxy::Proxy DIM1) ix1 w64
        , testProperty "Float"  $ test_u2a (Proxy::Proxy DIM1) ix1 f32
        , testProperty "Double" $ test_u2a (Proxy::Proxy DIM1) ix1 f64
        ]
      , testGroup "DIM2"
        [ testProperty "Int"    $ test_u2a (Proxy::Proxy DIM2) ix2 int
        , testProperty "Int8"   $ test_u2a (Proxy::Proxy DIM2) ix2 i8
        , testProperty "Int16"  $ test_u2a (Proxy::Proxy DIM2) ix2 i16
        , testProperty "Int32"  $ test_u2a (Proxy::Proxy DIM2) ix2 i32
        , testProperty "Int64"  $ test_u2a (Proxy::Proxy DIM2) ix2 i64
        , testProperty "Word"   $ test_u2a (Proxy::Proxy DIM2) ix2 word
        , testProperty "Word8"  $ test_u2a (Proxy::Proxy DIM2) ix2 w8
        , testProperty "Word16" $ test_u2a (Proxy::Proxy DIM2) ix2 w16
        , testProperty "Word32" $ test_u2a (Proxy::Proxy DIM2) ix2 w32
        , testProperty "Word64" $ test_u2a (Proxy::Proxy DIM2) ix2 w64
        , testProperty "Float"  $ test_u2a (Proxy::Proxy DIM2) ix2 f32
        , testProperty "Double" $ test_u2a (Proxy::Proxy DIM2) ix2 f64
        ]
      ]
    , testGroup "accelerate->uarray"
      [ testGroup "DIM1"
        [ testProperty "Int"    $ test_a2u (Proxy::Proxy DIM1) ix1 int
        , testProperty "Int8"   $ test_a2u (Proxy::Proxy DIM1) ix1 i8
        , testProperty "Int16"  $ test_a2u (Proxy::Proxy DIM1) ix1 i16
        , testProperty "Int32"  $ test_a2u (Proxy::Proxy DIM1) ix1 i32
        , testProperty "Int64"  $ test_a2u (Proxy::Proxy DIM1) ix1 i64
        , testProperty "Word"   $ test_a2u (Proxy::Proxy DIM1) ix1 word
        , testProperty "Word8"  $ test_a2u (Proxy::Proxy DIM1) ix1 w8
        , testProperty "Word16" $ test_a2u (Proxy::Proxy DIM1) ix1 w16
        , testProperty "Word32" $ test_a2u (Proxy::Proxy DIM1) ix1 w32
        , testProperty "Word64" $ test_a2u (Proxy::Proxy DIM1) ix1 w64
        , testProperty "Float"  $ test_a2u (Proxy::Proxy DIM1) ix1 f32
        , testProperty "Double" $ test_a2u (Proxy::Proxy DIM1) ix1 f64
        ]
      , testGroup "DIM2"
        [ testProperty "Int"    $ test_a2u (Proxy::Proxy DIM2) ix2 int
        , testProperty "Int8"   $ test_a2u (Proxy::Proxy DIM2) ix2 i8
        , testProperty "Int16"  $ test_a2u (Proxy::Proxy DIM2) ix2 i16
        , testProperty "Int32"  $ test_a2u (Proxy::Proxy DIM2) ix2 i32
        , testProperty "Int64"  $ test_a2u (Proxy::Proxy DIM2) ix2 i64
        , testProperty "Word"   $ test_a2u (Proxy::Proxy DIM2) ix2 word
        , testProperty "Word8"  $ test_a2u (Proxy::Proxy DIM2) ix2 w8
        , testProperty "Word16" $ test_a2u (Proxy::Proxy DIM2) ix2 w16
        , testProperty "Word32" $ test_a2u (Proxy::Proxy DIM2) ix2 w32
        , testProperty "Word64" $ test_a2u (Proxy::Proxy DIM2) ix2 w64
        , testProperty "Float"  $ test_a2u (Proxy::Proxy DIM2) ix2 f32
        , testProperty "Double" $ test_a2u (Proxy::Proxy DIM2) ix2 f64
        ]
      ]
    ]

