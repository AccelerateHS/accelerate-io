{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Test.Vector.Unboxed
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Vector.Unboxed
  where

import Test.Util
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Array.Accelerate                                        ( Shape, Elt, DIM0, DIM1, DIM2, Z(..), (:.)(..) )
import Data.Array.Accelerate.Array.Sugar                            ( reshape, rank )
import Data.Array.Accelerate.IO.Data.Vector.Unboxed                 as A
import qualified Data.Array.Accelerate.Hedgehog.Gen.Array           as Gen
import qualified Data.Array.Accelerate.Hedgehog.Gen.Shape           as Gen

import Data.Vector.Unboxed                                          as U

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Data.Proxy
import Data.Functor.Identity
import Text.Printf


unboxed :: U.Unbox e => Int -> Gen e -> Gen (U.Vector e)
unboxed n gen =
  U.fromListN n <$> Gen.list (Range.singleton n) gen

test_u2a
    :: (A.Unbox e, Show e, Eq e)
    => Gen e
    -> Property
test_u2a e =
  property $ do
    Z :. n <- forAll shape
    uvec   <- forAll (unboxed n e)
    --
    tripping uvec A.fromUnboxed (Identity . A.toUnboxed)

test_a2u
    :: forall sh e. (A.Unbox e, Gen.Shape sh, Shape sh, Elt e, Eq sh, Eq e)
    => Proxy sh
    -> Gen e
    -> Property
test_a2u _ e =
  property $ do
    sh  <- forAll (shape :: Gen sh)
    arr <- forAll (Gen.array sh e)
    --
    tripping arr A.toUnboxed (Identity . reshape sh . A.fromUnboxed)

test_a2u_dim
    :: forall sh. (Gen.Shape sh, Shape sh, Eq sh)
    => Proxy sh
    -> TestTree
test_a2u_dim dim =
  testGroup (printf "DIM%d" (rank (undefined::sh)))
    [ testProperty "Int"                    $ test_a2u dim int
    , testProperty "Int8"                   $ test_a2u dim i8
    , testProperty "Int16"                  $ test_a2u dim i16
    , testProperty "Int32"                  $ test_a2u dim i32
    , testProperty "Int64"                  $ test_a2u dim i64
    , testProperty "Word"                   $ test_a2u dim word
    , testProperty "Word8"                  $ test_a2u dim w8
    , testProperty "Word16"                 $ test_a2u dim w16
    , testProperty "Word32"                 $ test_a2u dim w32
    , testProperty "Word64"                 $ test_a2u dim w64
    , testProperty "Float"                  $ test_a2u dim f32
    , testProperty "Double"                 $ test_a2u dim f64
    , testProperty "Complex Float"          $ test_a2u dim (complex f32)
    , testProperty "(Double, Int16)"        $ test_a2u dim ((,) <$> f64 <*> i16)
    , testProperty "(Float, (Double,Int))"  $ test_a2u dim ((,) <$> f32 <*> ((,) <$> f64 <*> int))
    ]

test_vector_unboxed :: TestTree
test_vector_unboxed =
  testGroup "Data.Vector.Unboxed"
    [ testGroup "unboxed->accelerate"
      [ testProperty "Int"                  $ test_u2a int
      , testProperty "Int8"                 $ test_u2a i8
      , testProperty "Int16"                $ test_u2a i16
      , testProperty "Int32"                $ test_u2a i32
      , testProperty "Int64"                $ test_u2a i64
      , testProperty "Word"                 $ test_u2a word
      , testProperty "Word8"                $ test_u2a w8
      , testProperty "Word16"               $ test_u2a w16
      , testProperty "Word32"               $ test_u2a w32
      , testProperty "Word64"               $ test_u2a w64
      , testProperty "Float"                $ test_u2a f32
      , testProperty "Double"               $ test_u2a f64
      , testProperty "Complex Float"        $ test_u2a (complex f32)
      , testProperty "(Int,Float)"          $ test_u2a ((,) <$> int <*> f32)
      , testProperty "((Int8,Word),Double)" $ test_u2a ((,) <$> ((,) <$> i8 <*> word) <*> f64)
      ]
    , testGroup"accelerate->unboxed"
      [ test_a2u_dim (Proxy::Proxy DIM0)
      , test_a2u_dim (Proxy::Proxy DIM1)
      , test_a2u_dim (Proxy::Proxy DIM2)
      ]
    ]

