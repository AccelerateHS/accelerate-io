{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Test.Vector.Storable
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test.Vector.Storable
  where

import Test.Util
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.Array.Accelerate                                        ( Shape, Elt, Z(..), (:.)(..) )
import Data.Array.Accelerate.Array.Sugar                            ( rank, EltRepr )
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.IO.Data.Vector.Storable                as A
import qualified Data.Array.Accelerate                              as A

import Data.Vector.Storable                                         as S

import Hedgehog
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Range                                     as Range

import Data.Word
import Text.Printf
import Prelude                                                      as P


storable :: Storable e => Int -> Gen e -> Gen (S.Vector e)
storable n gen =
  S.fromListN n <$> Gen.list (Range.singleton n) gen

boolToWord8 :: Bool -> Word8
boolToWord8 True  = 1
boolToWord8 False = 0

test_s2a
    :: forall e. (Storable e, Elt e, Eq e, Vectors (EltRepr e) ~ Vector e)
    => Gen e
    -> Property
test_s2a e =
  property $ do
    sh@(Z :. n) <- forAll dim1
    svec        <- forAll (storable n e)
    --
    S.toList svec === A.toList (A.fromVectors sh svec)

test_s2a_t2
    :: forall a b. ( Storable a, Elt a, Eq a, Vectors (EltRepr a) ~ Vector a
                   , Storable b, Elt b, Eq b, Vectors (EltRepr b) ~ Vector b
                   )
    => Gen a
    -> Gen b
    -> Property
test_s2a_t2 a b =
  property $ do
    sh@(Z :. n) <- forAll dim1
    sa          <- forAll (storable n a)
    sb          <- forAll (storable n b)
    --
    P.zip (S.toList sa) (S.toList sb) === A.toList (A.fromVectors sh (((), sa), sb))


test_a2s
    :: forall sh e. (Shape sh, Storable e, Elt e, Eq sh, Eq e, Vectors (EltRepr e) ~ Vector e)
    => Gen sh
    -> Gen e
    -> Property
test_a2s dim e =
  property $ do
    sh  <- forAll dim
    arr <- forAll (array sh e)
    --
    A.toList arr === S.toList (A.toVectors arr)

test_a2s_t2
    :: forall sh a b. ( Shape sh, Eq sh, Eq a, Eq b, Elt a, Elt b, Storable a, Storable b
                      , Vectors (EltRepr (a,b)) ~ (((), Vector a), Vector b)
                      )
    => Gen sh
    -> Gen (a,b)
    -> Property
test_a2s_t2 dim e =
  property $ do
    sh  <- forAll dim
    arr <- forAll (array sh e)
    let
        (((), va), vb) = A.toVectors arr
    --
    A.toList arr === P.zip (S.toList va) (S.toList vb)


test_s2a_complex
    :: forall e. ( Storable e, Elt (Complex e), Eq e
                 , Vectors (EltRepr (Complex e)) ~ Vector e
                 )
    => Gen (Complex e)
    -> Property
test_s2a_complex e =
  property $ do
    sh@(Z :. n) <- forAll dim1
    svec        <- forAll (storable n e)
    --
    S.toList svec === A.toList (A.fromVectors sh (S.unsafeCast svec :: S.Vector e))

test_a2s_complex
    :: forall sh e. ( Shape sh, Storable e, Elt (Complex e), Eq sh, Eq e
                    , Vectors (EltRepr (Complex e)) ~ Vector e
                    )
    => Gen sh
    -> Gen (Complex e)
    -> Property
test_a2s_complex dim e =
  property $ do
    sh  <- forAll dim
    arr <- forAll (array sh e)
    --
    A.toList arr === S.toList (S.unsafeCast (A.toVectors arr) :: S.Vector (Complex e))


test_a2s_dim
    :: forall sh. (Shape sh, Eq sh)
    => Gen sh
    -> TestTree
test_a2s_dim dim =
  testGroup (printf "DIM%d" (rank @sh))
    [ testProperty "Int"                    $ test_a2s dim int
    , testProperty "Int8"                   $ test_a2s dim i8
    , testProperty "Int16"                  $ test_a2s dim i16
    , testProperty "Int32"                  $ test_a2s dim i32
    , testProperty "Int64"                  $ test_a2s dim i64
    , testProperty "Word"                   $ test_a2s dim word
    , testProperty "Word8"                  $ test_a2s dim w8
    , testProperty "Word16"                 $ test_a2s dim w16
    , testProperty "Word32"                 $ test_a2s dim w32
    , testProperty "Word64"                 $ test_a2s dim w64
    , testProperty "Char"                   $ test_a2s dim Gen.unicode
    -- , testProperty "Bool"                   $ test_a2s dim Gen.bool
    , testProperty "Float"                  $ test_a2s dim f32
    , testProperty "Double"                 $ test_a2s dim f64
    , testProperty "Complex Float"          $ test_a2s_complex dim (complex f32)
    , testProperty "(Double, Int16)"        $ test_a2s_t2 dim ((,) <$> f64 <*> i16)
    , testProperty "(Float, Float)"         $ test_a2s_t2 dim ((,) <$> f32 <*> f32)
    -- , testProperty "(Float, (Double,Int))"  $ test_a2s dim ((,) <$> f32 <*> ((,) <$> f64 <*> int))
    ]

test_vector_storable :: TestTree
test_vector_storable =
  testGroup "Data.Vector.Storable"
    [ testGroup "storable->accelerate"
      [ testProperty "Int"            $ test_s2a int
      , testProperty "Int8"           $ test_s2a i8
      , testProperty "Int16"          $ test_s2a i16
      , testProperty "Int32"          $ test_s2a i32
      , testProperty "Int64"          $ test_s2a i64
      , testProperty "Word"           $ test_s2a word
      , testProperty "Word8"          $ test_s2a w8
      , testProperty "Word16"         $ test_s2a w16
      , testProperty "Word32"         $ test_s2a w32
      , testProperty "Word64"         $ test_s2a w64
      , testProperty "Char"           $ test_s2a Gen.unicode
      , testProperty "Bool"           $ test_s2a (boolToWord8 <$> Gen.bool)
      , testProperty "Float"          $ test_s2a f32
      , testProperty "Double"         $ test_s2a f64
      , testProperty "Complex Float"  $ test_s2a_complex (complex f32)
      , testProperty "(Int,Float)"    $ test_s2a_t2 int f32
      , testProperty "(Int8,Word)"    $ test_s2a_t2 i8 word
      ]
    , testGroup"accelerate->storable"
      [ test_a2s_dim dim0
      , test_a2s_dim dim1
      , test_a2s_dim dim2
      ]
    ]

