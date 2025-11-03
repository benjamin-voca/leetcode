module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Problems.P3691

main :: IO ()
main = defaultMain $ testGroup "LeetCode Problems"
  [ Problems.P3691.tests
  ]
