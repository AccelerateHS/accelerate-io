{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Test.Array.Unboxed
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Array.Unboxed
  where

import Test.Array.IArray

import Test.Util
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Array.Accelerate                                        ( Shape, Elt )
import Data.Array.Accelerate.Array.Sugar                            ( EltRepr )
import Data.Array.Accelerate.IO.Data.Array.Unboxed                  as A
import qualified Data.Array.Accelerate                              as A

import Data.Array.Unboxed                                           as U hiding ( array )

import Hedgehog


test_u2a
    :: forall ix sh e. (Ix ix, IArray UArray e, Elt ix, Shape sh, Elt e, Eq e, Show (UArray ix e), Eq (UArray ix e), IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Gen sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_u2a _ ix e =
  property $ do
    ua <- forAll (iarray ix e :: Gen (UArray ix e))
    let
        (lo,_)  = bounds ua
        acc     = fromUArray ua :: A.Array sh e
        ua'     = toUArray (Just lo) acc
    --
    U.elems ua === A.toList acc   -- elements convert correctly
    ua         === ua'            -- indices round-trip correctly

test_a2u
    :: forall ix sh e. (Ix ix, IArray UArray e, Elt ix, Shape sh, Elt e, Show (UArray ix e), Eq (UArray ix e), Eq sh, Eq e, IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Gen sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_a2u dim _ e =
  property $ do
    sh      <- forAll dim
    arr     <- forAll (array sh e)
    --
    A.toList arr === U.elems (toUArray Nothing arr :: UArray ix e)


test_array_unboxed :: TestTree
test_array_unboxed =
  testGroup "Data.Array.Unboxed"
    [ testGroup "uarray->accelerate"
      [ testGroup "DIM1"
        [ testProperty "Int"    $ test_u2a dim1 ix1 int
        , testProperty "Int8"   $ test_u2a dim1 ix1 i8
        , testProperty "Int16"  $ test_u2a dim1 ix1 i16
        , testProperty "Int32"  $ test_u2a dim1 ix1 i32
        , testProperty "Int64"  $ test_u2a dim1 ix1 i64
        , testProperty "Word"   $ test_u2a dim1 ix1 word
        , testProperty "Word8"  $ test_u2a dim1 ix1 w8
        , testProperty "Word16" $ test_u2a dim1 ix1 w16
        , testProperty "Word32" $ test_u2a dim1 ix1 w32
        , testProperty "Word64" $ test_u2a dim1 ix1 w64
        , testProperty "Float"  $ test_u2a dim1 ix1 f32
        , testProperty "Double" $ test_u2a dim1 ix1 f64
        ]
      , testGroup "DIM2"
        [ testProperty "Int"    $ test_u2a dim2 ix2 int
        , testProperty "Int8"   $ test_u2a dim2 ix2 i8
        , testProperty "Int16"  $ test_u2a dim2 ix2 i16
        , testProperty "Int32"  $ test_u2a dim2 ix2 i32
        , testProperty "Int64"  $ test_u2a dim2 ix2 i64
        , testProperty "Word"   $ test_u2a dim2 ix2 word
        , testProperty "Word8"  $ test_u2a dim2 ix2 w8
        , testProperty "Word16" $ test_u2a dim2 ix2 w16
        , testProperty "Word32" $ test_u2a dim2 ix2 w32
        , testProperty "Word64" $ test_u2a dim2 ix2 w64
        , testProperty "Float"  $ test_u2a dim2 ix2 f32
        , testProperty "Double" $ test_u2a dim2 ix2 f64
        ]
      ]
    , testGroup "accelerate->uarray"
      [ testGroup "DIM1"
        [ testProperty "Int"    $ test_a2u dim1 ix1 int
        , testProperty "Int8"   $ test_a2u dim1 ix1 i8
        , testProperty "Int16"  $ test_a2u dim1 ix1 i16
        , testProperty "Int32"  $ test_a2u dim1 ix1 i32
        , testProperty "Int64"  $ test_a2u dim1 ix1 i64
        , testProperty "Word"   $ test_a2u dim1 ix1 word
        , testProperty "Word8"  $ test_a2u dim1 ix1 w8
        , testProperty "Word16" $ test_a2u dim1 ix1 w16
        , testProperty "Word32" $ test_a2u dim1 ix1 w32
        , testProperty "Word64" $ test_a2u dim1 ix1 w64
        , testProperty "Float"  $ test_a2u dim1 ix1 f32
        , testProperty "Double" $ test_a2u dim1 ix1 f64
        ]
      , testGroup "DIM2"
        [ testProperty "Int"    $ test_a2u dim2 ix2 int
        , testProperty "Int8"   $ test_a2u dim2 ix2 i8
        , testProperty "Int16"  $ test_a2u dim2 ix2 i16
        , testProperty "Int32"  $ test_a2u dim2 ix2 i32
        , testProperty "Int64"  $ test_a2u dim2 ix2 i64
        , testProperty "Word"   $ test_a2u dim2 ix2 word
        , testProperty "Word8"  $ test_a2u dim2 ix2 w8
        , testProperty "Word16" $ test_a2u dim2 ix2 w16
        , testProperty "Word32" $ test_a2u dim2 ix2 w32
        , testProperty "Word64" $ test_a2u dim2 ix2 w64
        , testProperty "Float"  $ test_a2u dim2 ix2 f32
        , testProperty "Double" $ test_a2u dim2 ix2 f64
        ]
      ]
    ]

