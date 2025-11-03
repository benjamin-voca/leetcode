# LeetCode Training Project (Haskell)

A structured Haskell project for practicing LeetCode problems with organized solution files, tests, and helper utilities.

## Project Structure

```
.
├── src/
│   ├── Helpers.hs              # Generic helper functions (lists, trees, etc.)
│   ├── MyLib.hs                # Core library
│   └── Solutions/
│       ├── P3691.hs            # Solution for problem 3691
│       └── ...                 # Other problem solutions
├── test/
│   ├── Main.hs                 # Test runner
│   └── Problems/
│       ├── P3691.hs            # Tests for problem 3691
│       └── ...                 # Other problem tests
├── app/
│   └── Main.hs                 # Executable entry point
├── scripts/
│   └── new-problem.py          # Problem template generator
└── leetcode.cabal              # Cabal build configuration
```

## Quick Start

### Creating a New Problem

Use the script to generate a new problem template:

```bash
python3 scripts/new-problem.py <problem-number> "<function-signature>"
```

**Example:**
```bash
python3 scripts/new-problem.py 1 "twoSum :: [Int] -> Int -> [Int]"
```

This will create:
- `src/Solutions/P1.hs` - Where you implement your solution
- `test/Problems/P1.hs` - Where you write your tests
- Automatically update `leetcode.cabal` and `test/Main.hs`

### Implementing a Solution

Edit `src/Solutions/P<number>.hs`:

```haskell
-- | Problem P1 Solution
-- Two Sum
-- https://leetcode.com/problems/two-sum/
module Solutions.P1 where

import Helpers

twoSum :: [Int] -> Int -> [Int]
twoSum nums target = 
  -- Your implementation here
  undefined
```

### Writing Tests

Edit `test/Problems/P<number>.hs` using [tasty](https://hackage.haskell.org/package/tasty):

```haskell
-- | Test suite for problem P1
module Problems.P1 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.P1

tests :: TestTree
tests = testGroup "P1 - Two Sum"
  [ testCase "example test" example1
  , testCase "another test" example2
  ]

example1 :: Assertion
example1 = twoSum [2, 7, 11, 15] 9 @?= [0, 1]

example2 :: Assertion
example2 = twoSum [3, 2, 4] 6 @?= [1, 2]
```

**Test operators:**
- `(@?=)` - Assertion for equality (from `tasty-hunit`)
- `testCase` - Create a single test
- `testGroup` - Group related tests
- `TestTree` - The test type

### Running Tests

Run all tests:
```bash
cabal test
```

Run specific test:
```bash
cabal test leetcode-test -- --verbose
```

### Building

Build the project:
```bash
cabal build
```

Run the executable:
```bash
cabal run leetcode
```

## Using Helpers

The `Helpers.hs` module is available to all solutions and tests. Add generic utility functions there:

```haskell
-- src/Helpers.hs
module Helpers where

-- Example: Tree utilities, list helpers, etc.
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- Add more helpers as needed
```

Import in your solutions:
```haskell
import Helpers

solution :: Int -> Int
solution n = if isEven n then n * 2 else n
```

## Dependency Management

To add dependencies, edit `leetcode.cabal`:

```cabal
test-suite leetcode-test
    build-depends:
        base ^>=4.21.0.0,
        tasty >= 1.4,              -- Test framework
        tasty-hunit >= 0.10,       -- HUnit support for tasty
        tasty-quickcheck >= 0.10,  -- QuickCheck support (optional)
        leetcode
```

Then run:
```bash
cabal update
cabal build
```

**Common test libraries:**
- `tasty` - Main test framework (already included)
- `tasty-hunit` - Unit testing (already included)
- `tasty-quickcheck` - Property-based testing (optional)
- `tasty-golden` - Golden file testing (optional)

## Tips

- **Problem Template**: The `new-problem.py` script automatically:
  - Creates solution and test files
  - Updates `leetcode.cabal` with new modules
  - Updates `test/Main.hs` to run new tests
  
- **Test Organization**: Each problem gets its own test file in `test/Problems/`

- **Helpers Library**: Common utilities in `src/Helpers.hs` are available to all solutions

- **Multiple Problems**: The project structure supports adding as many problems as needed without manual cabal changes

## Compilation

The project uses GHC2024 with `-Wall` warnings enabled. Make sure to:
- Fix all warnings
- Use meaningful module and function names
- Document complex functions with Haddock comments

---

Happy LeetCoding! 🚀
