# Quick Start Guide

## Creating a New Problem

1. **Generate template files:**
   ```bash
   python3 scripts/new-problem.py <problem-number> "<function-signature>"
   ```

   Example:
   ```bash
   python3 scripts/new-problem.py 1 "twoSum :: [Int] -> Int -> [(Int, Int)]"
   ```

   This creates:
   - `src/Solutions/P1.hs` - Your solution goes here
   - `test/Problems/P1.hs` - Your test cases go here (using `tasty`)
   - Updates `leetcode.cabal` and `test/Main.hs` automatically

2. **Implement your solution** in `src/Solutions/P<number>.hs`:
   ```haskell
   module Solutions.P1 where

   import Helpers

   twoSum :: [Int] -> Int -> [(Int, Int)]
   twoSum nums target = 
     -- Your implementation here
     []
   ```

3. **Write test cases** in `test/Problems/P<number>.hs` (using [tasty](https://hackage.haskell.org/package/tasty)):
   ```haskell
   module Problems.P1 where

   import Test.Tasty (TestTree, testGroup)
   import Test.Tasty.HUnit (testCase, (@?=), Assertion)
   import Solutions.P1

   tests :: TestTree
   tests = testGroup "P1"
     [ testCase "example 1" test1
     , testCase "example 2" test2
     ]

   test1 :: Assertion
   test1 = twoSum [2, 7, 11, 15] 9 @?= [(0, 1)]

   test2 :: Assertion
   test2 = twoSum [3, 2, 4] 6 @?= [(1, 2)]
   ```

4. **Run tests:**
   ```bash
   cabal test
   ```

## Using Helpers

Add utility functions to `src/Helpers.hs`:

```haskell
module Helpers where

-- Tree node definition
data TreeNode = Node 
  { val :: Int
  , left :: Maybe TreeNode
  , right :: Maybe TreeNode
  } deriving (Show, Eq)

-- List utilities
firstN :: Int -> [a] -> [a]
firstN = take
```

Use in solutions:
```haskell
import Helpers

solution :: [Int] -> Int
solution xs = sum (firstN 3 xs)
```

## Testing with Tasty

The project uses [tasty](https://hackage.haskell.org/package/tasty) for organized test suites. 

### Basic Test Patterns

**Simple equality test:**
```haskell
testCase "test name" $ solution input @?= expected
```

**Multiple assertions:**
```haskell
test1 :: Assertion
test1 = do
  solution [1,2,3] @?= 6
  solution [] @?= 0
  solution [0] @?= 0
```

**Grouped tests:**
```haskell
tests :: TestTree
tests = testGroup "MyProblem"
  [ testGroup "basic cases"
      [ testCase "case 1" $ solution 1 @?= 2
      , testCase "case 2" $ solution 2 @?= 4
      ]
  , testGroup "edge cases"
      [ testCase "empty" $ solution 0 @?= 0
      ]
  ]
```

## Building

```bash
# Build everything
cabal build

# Build and run executable
cabal run leetcode

# Run tests
cabal test

# Run tests with verbose output
cabal test --verbose

# Clean build artifacts
cabal clean
```

## Viewing Solution Statistics

```bash
# List all solutions
ls src/Solutions/

# Count problems solved
ls src/Solutions/ | wc -l

# Run all tests with details
cabal test --verbose
```

## Testing Examples

See `test/Problems/P3691.hs` for a template. Create tests like this:

```haskell
module Problems.P1 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.P1

tests :: TestTree
tests = testGroup "P1 - Two Sum"
  [ testCase "example from problem" $ 
      twoSum [2,7,11,15] 9 @?= [0, 1]
  , testCase "different order" $
      twoSum [3,2,4] 6 @?= [1, 2]
  , testCase "single valid pair" $
      twoSum [1, 5] 6 @?= [0, 1]
  ]
```

---

**Happy problem solving!** 🚀
