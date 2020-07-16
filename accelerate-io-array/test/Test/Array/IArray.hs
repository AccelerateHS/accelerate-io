{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Test.Array.IArray
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Array.IArray
  where

import Test.Util
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Array.Accelerate                                        ( Shape, Elt )
import Data.Array.Accelerate.Array.Sugar                            ( EltRepr )
import Data.Array.Accelerate.IO.Data.Array.IArray                   as A
import qualified Data.Array.Accelerate                              as A

import Data.Array.IArray                                            hiding ( array, indices, elems )
import qualified Data.Array.IArray                                  as I

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Data.Proxy
import Prelude


iarray :: (Ix ix, IArray a e) => Gen (ix,ix) -> Gen e -> Gen (a ix e)
iarray ix e = do
  (lo,hi) <- ix
  let n       = rangeSize (lo,hi)
      indices = range (lo,hi)
  --
  elems <- Gen.list (Range.singleton n) e
  return $ I.array (lo,hi) (zip indices elems)


test_i2a
    :: forall ix sh a e. (Ix ix, IArray a e, Elt ix, Shape sh, Elt e, Eq e, Show (a ix e), Eq (a ix e), IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Proxy a
    -> Gen sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_i2a _ _ ix e =
  property $ do
    ia  <- forAll (iarray ix e :: Gen (a ix e))
    let
        (lo,_) = bounds ia
        acc    = fromIArray ia :: A.Array sh e
        ia'    = toIArray (Just lo) acc
    --
    I.elems ia === A.toList acc  -- elements convert correctly
    ia         === ia'           -- indices round-trip correctly

test_a2i
    :: forall ix sh a e. (Ix ix, IArray a e, Elt ix, Shape sh, Elt e, Eq e, Show (a ix e), Eq sh, Eq e, IxShapeRepr (EltRepr ix) ~ EltRepr sh)
    => Proxy a
    -> Gen sh
    -> Gen (ix,ix)
    -> Gen e
    -> Property
test_a2i _ dim _ e =
  property $ do
    sh  <- forAll dim
    acc <- forAll (array sh e)
    --
    A.toList acc === I.elems (toIArray Nothing acc :: a ix e)


test_array_iarray :: TestTree
test_array_iarray =
  testGroup "Data.Array.IArray"
    [ testGroup "iarray->accelerate"
      [ testGroup "DIM1"
        [ testProperty "Int"           $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 int
        , testProperty "Int8"          $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 i8
        , testProperty "Int16"         $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 i16
        , testProperty "Int32"         $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 i32
        , testProperty "Int64"         $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 i64
        , testProperty "Word"          $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 word
        , testProperty "Word8"         $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 w8
        , testProperty "Word16"        $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 w16
        , testProperty "Word32"        $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 w32
        , testProperty "Word64"        $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 w64
        , testProperty "Float"         $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 f32
        , testProperty "Double"        $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 f64
        , testProperty "Complex Float" $ test_i2a (Proxy::Proxy I.Array) dim1 ix1 (complex f32)
        ]
      , testGroup "DIM2"
        [ testProperty "Int"           $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 int
        , testProperty "Int8"          $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 i8
        , testProperty "Int16"         $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 i16
        , testProperty "Int32"         $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 i32
        , testProperty "Int64"         $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 i64
        , testProperty "Word"          $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 word
        , testProperty "Word8"         $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 w8
        , testProperty "Word16"        $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 w16
        , testProperty "Word32"        $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 w32
        , testProperty "Word64"        $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 w64
        , testProperty "Float"         $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 f32
        , testProperty "Double"        $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 f64
        , testProperty "Complex Float" $ test_i2a (Proxy::Proxy I.Array) dim2 ix2 (complex f32)
        ]
      ]
    , testGroup "accelerate->iarray"
      [ testGroup "DIM1"
        [ testProperty "Int"           $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 int
        , testProperty "Int8"          $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 i8
        , testProperty "Int16"         $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 i16
        , testProperty "Int32"         $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 i32
        , testProperty "Int64"         $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 i64
        , testProperty "Word"          $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 word
        , testProperty "Word8"         $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 w8
        , testProperty "Word16"        $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 w16
        , testProperty "Word32"        $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 w32
        , testProperty "Word64"        $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 w64
        , testProperty "Float"         $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 f32
        , testProperty "Double"        $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 f64
        , testProperty "Complex Float" $ test_a2i (Proxy::Proxy I.Array) dim1 ix1 (complex f32)
        ]
      , testGroup "DIM2"
        [ testProperty "Int"           $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 int
        , testProperty "Int8"          $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 i8
        , testProperty "Int16"         $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 i16
        , testProperty "Int32"         $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 i32
        , testProperty "Int64"         $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 i64
        , testProperty "Word"          $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 word
        , testProperty "Word8"         $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 w8
        , testProperty "Word16"        $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 w16
        , testProperty "Word32"        $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 w32
        , testProperty "Word64"        $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 w64
        , testProperty "Float"         $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 f32
        , testProperty "Double"        $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 f64
        , testProperty "Complex Float" $ test_a2i (Proxy::Proxy I.Array) dim2 ix2 (complex f32)
        ]
      ]
    ]

