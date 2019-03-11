-- |
-- Module      : Test
-- Copyright   : [2017..2019] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Test where

import Test.Tasty

import Test.Vector.Storable
import Test.Vector.Unboxed

main :: IO ()
main
  = defaultMain
  $ testGroup "IO"
    [ test_vector_unboxed
    , test_vector_storable
    ]

