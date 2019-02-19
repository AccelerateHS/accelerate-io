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

import Test.Array.IArray
import Test.Array.Unboxed

main :: IO ()
main
  = defaultMain
  $ testGroup "IO"
    [ test_array_iarray
    , test_array_unboxed
    ]

