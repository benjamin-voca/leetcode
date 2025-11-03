# Tasty Testing Guide

This project uses [tasty](https://hackage.haskell.org/package/tasty) for test organization and execution.

## Overview

**Tasty** is a Haskell testing framework that provides:
- ✅ Clean test organization with `TestTree` and `testGroup`
- ✅ Multiple test runners and formatters
- ✅ Built-in support for HUnit assertions
- ✅ Colored output and detailed reporting
- ✅ Flexible filtering and running specific tests

## Basic Test Structure

Every test module follows this pattern:

```haskell
module Problems.P<number> where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.P<number>

-- Main test suite
tests :: TestTree
tests = testGroup "P<number>"
  [ -- Test cases go here
  ]
```

## Writing Tests

### Simple Assertions

```haskell
testCase "description" $ expression @?= expected
```

Examples:
```haskell
testCase "basic sum" $ solution [1,2,3] @?= 6
testCase "empty list" $ solution [] @?= 0
testCase "single element" $ solution [5] @?= 5
```

### Multi-line Assertions

```haskell
test1 :: Assertion
test1 = do
  solution [1,2,3] @?= 6
  solution [] @?= 0
```

Used in test group:
```haskell
tests = testGroup "P1"
  [ testCase "multiple checks" test1
  ]
```

### Grouped Tests

```haskell
tests :: TestTree
tests = testGroup "MyProblem"
  [ testGroup "basic cases"
      [ testCase "test 1" $ solution 1 @?= 2
      , testCase "test 2" $ solution 2 @?= 4
      ]
  , testGroup "edge cases"
      [ testCase "zero" $ solution 0 @?= 0
      , testCase "negative" $ solution (-1) @?= -2
      ]
  ]
```

## Real Example

Here's a complete test file:

```haskell
-- | Tests for problem P1 - Two Sum
module Problems.P1 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.P1

tests :: TestTree
tests = testGroup "P1 - Two Sum"
  [ testGroup "basic cases"
      [ testCase "standard case" standardCase
      , testCase "different order" differentOrder
      ]
  , testGroup "edge cases"
      [ testCase "minimum pair" minimumPair
      ]
  ]

standardCase :: Assertion
standardCase = twoSum [2,7,11,15] 9 @?= [0, 1]

differentOrder :: Assertion
differentOrder = twoSum [3,2,4] 6 @?= [1, 2]

minimumPair :: Assertion
minimumPair = twoSum [1, 5] 6 @?= [0, 1]
```

## Running Tests

### Run all tests
```bash
cabal test
```

Output:
```
LeetCode Problems
  P3691
    example test 1: OK
  P1
    basic cases
      standard case: OK
      different order: OK
    edge cases
      minimum pair: OK

All 4 tests passed (0.01s)
```

### Run specific test
```bash
cabal test -- -p "P1"
```

### Verbose output
```bash
cabal test --verbose
```

### List all tests
```bash
cabal test -- --list
```

## Assertion Reference

From `Test.Tasty.HUnit`:

| Operator | Meaning |
|----------|---------|
| `(@?=)` | Equality check |
| `(~=?)` | Floating point equality |
| `assertBool` | Boolean assertion |
| `assertEqual` | Labeled equality |
| `assertFailure` | Always fails |

Examples:
```haskell
-- Equality
result @?= expected

-- With label
("test name", expected) <- solutionWithIO

-- Boolean
assertBool "should be true" (solution x > 0)

-- Always fail
assertFailure "this should not happen"
```

## Test Organization Best Practices

1. **Group by functionality:**
   ```haskell
   testGroup "P1"
     [ testGroup "basic functionality" [...]
     , testGroup "edge cases" [...]
     , testGroup "error handling" [...]
     ]
   ```

2. **Name tests descriptively:**
   ```haskell
   testCase "should sum positive numbers" ...
   testCase "should handle empty list" ...
   testCase "should reject negative input" ...
   ```

3. **Use helper functions:**
   ```haskell
   test1 :: Assertion
   test1 = twoSum [2,7,11,15] 9 @?= [0, 1]
   
   test2 :: Assertion
   test2 = twoSum [3,2,4] 6 @?= [1, 2]
   
   tests = testGroup "P1"
     [ testCase "case 1" test1
     , testCase "case 2" test2
     ]
   ```

## Integration with test/Main.hs

The test runner in `test/Main.hs` automatically:
1. Imports all problem test modules
2. Combines them into a single test suite
3. Runs them with tasty's test runner

```haskell
module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Problems.P3691
import qualified Problems.P1

main :: IO ()
main = defaultMain $ testGroup "LeetCode Problems"
  [ Problems.P3691.tests
  , Problems.P1.tests
  ]
```

When you create a new problem, the Python script automatically:
- Creates the test module with tasty imports
- Adds it to test/Main.hs with proper imports

## Common Patterns

### Testing IO
```haskell
import Test.Tasty.HUnit (testCase, Assertion)

test1 :: Assertion
test1 = do
  result <- someIO
  result @?= expected
```

### Testing with dependencies
```haskell
import qualified Helpers as H

testCase "uses helpers" $ 
  H.process (solution input) @?= expected
```

### Testing multiple values
```haskell
testCase "multiple inputs" $ do
  solution 1 @?= 2
  solution 2 @?= 4
  solution 3 @?= 6
```

## Troubleshooting

**Tests not running:**
```bash
cabal clean
cabal build
cabal test
```

**Missing tasty library:**
```bash
cabal update
cabal install tasty tasty-hunit
```

**Test file not found:**
- Ensure module is listed in `other-modules` in cabal file
- Check spelling of module name
- Run `cabal test -- --list` to see discovered tests

---

For more information, see:
- [Tasty documentation](https://hackage.haskell.org/package/tasty)
- [HUnit documentation](https://hackage.haskell.org/package/HUnit)
