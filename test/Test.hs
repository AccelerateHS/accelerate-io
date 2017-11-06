-- |
-- Module      : Test
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test where

import Test.Tasty

import Test.Array.IArray
import Test.Array.Unboxed
import Test.Vector.Storable
import Test.Vector.Unboxed

main :: IO ()
main
  = defaultMain
  $ testGroup "IO"
    [ test_vector_unboxed
    , test_vector_storable
    , test_array_iarray
    , test_array_unboxed
    ]

