-- | Test suite for problem P3691
module Problems.P3691 where

import Data.List (intercalate)
import Solutions.P3691
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Define test cases here
tests :: TestTree
tests =
  testGroup
    "P3691"
    [ testCase
        ( "test "
            ++ intercalate "" (map show $ fst is)
            ++ " "
            ++ show (fst $ snd is)
        )
        $ max_total_subarray_value
          (fst is)
          (fst $ snd is)
          @?= snd (snd is)
    | is <- testInputs
    ]

testInputs :: [([Int], (Int, Int))]
testInputs =
  [ ([1, 3, 2], (2, 4)),
    ([4, 2, 5, 1], (3, 12)),
    ([11, 8], (1, 3)),
    ([11, 8], (2, 3)),
    ([11, 8], (3, 3)),
    ([11, 8], (4, 3)),
    ([11, 8], (5, 3)),
    ([11, 8], (6, 3)),
    ([38, 32], (1, 6)),
    ([38, 32], (2, 6)),
    ([38, 32], (3, 6)),
    ([38, 32], (4, 6)),
    ([38, 32], (5, 6)),
    ([18, 11], (1, 7)),
    ([21, 11], (1, 10)),
    ([47, 39], (1, 8)),
    ([14, 29], (1, 15)),
    ([9, 9, 37], (1, 28)),
    ([9, 9, 37], (2, 56))
  ]
