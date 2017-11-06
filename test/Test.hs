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

import Test.Vector.Unboxed
import Test.Vector.Storable

main :: IO ()
main
  = defaultMain
  $ testGroup "IO"
    [ test_vector_unboxed
    , test_vector_storable
    ]

