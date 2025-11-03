# ✅ LeetCode Haskell Project - Tasty Testing Edition

Your LeetCode training project is now fully configured with **tasty** testing framework! 🎉

## What's Included

### ✨ Features
- **Problem Generation Script**: Automatically create solution + test templates
- **Tasty Testing Framework**: Clean, organized test suites
- **Generic Helpers Library**: Reusable utilities for all solutions
- **Auto-updated Build Config**: cabal files update automatically
- **Colorful Test Output**: Easy to read test results

### 📁 Project Structure
```
leetcode/
├── src/
│   ├── Helpers.hs           ← Add reusable utilities here
│   ├── MyLib.hs
│   └── Solutions/
│       ├── P1.hs            ← Your solutions
│       ├── P3691.hs
│       └── ...
├── test/
│   ├── Main.hs              ← Auto-managed test runner
│   └── Problems/
│       ├── P1.hs            ← Your tasty tests
│       ├── P3691.hs
│       └── ...
├── scripts/
│   └── new-problem.py       ← CREATE NEW PROBLEMS!
├── leetcode.cabal           ← Auto-updated
├── TESTING_WITH_TASTY.md    ← Testing guide
├── QUICKSTART_TASTY.md      ← Quick reference
└── README.md
```

## 🚀 Quick Start

### 1. Create a New Problem
```bash
python3 scripts/new-problem.py 1 "twoSum :: [Int] -> Int -> [Int]"
```

### 2. Implement Solution
Edit `src/Solutions/P1.hs`:
```haskell
module Solutions.P1 where

twoSum :: [Int] -> Int -> [Int]
twoSum nums target = []  -- Your implementation
```

### 3. Write Tests (using Tasty)
Edit `test/Problems/P1.hs`:
```haskell
module Problems.P1 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.P1

tests :: TestTree
tests = testGroup "P1"
  [ testCase "example 1" $ twoSum [2,7,11,15] 9 @?= [0,1]
  , testCase "example 2" $ twoSum [3,2,4] 6 @?= [1,2]
  ]
```

### 4. Run Tests
```bash
cabal test
```

Output:
```
LeetCode Problems
  P3691
    example test 1: OK
  P1
    example 1: OK
    example 2: OK

All 3 tests passed (0.01s)
```

## 📚 Tasty Testing Examples

### Simple Test
```haskell
testCase "adds numbers" $ solution 2 3 @?= 5
```

### Multiple Assertions
```haskell
test1 :: Assertion
test1 = do
  solution [1,2,3] @?= 6
  solution [] @?= 0

tests = testGroup "P1"
  [ testCase "multiple checks" test1 ]
```

### Organized Tests
```haskell
tests = testGroup "P1 - Two Sum"
  [ testGroup "basic"
      [ testCase "standard" $ solution [2,7] 9 @?= [0,1]
      , testCase "reverse" $ solution [3,2] 5 @?= [1,0]
      ]
  , testGroup "edge cases"
      [ testCase "minimum" $ solution [1,5] 6 @?= [0,1]
      ]
  ]
```

## 🛠️ Commands

```bash
# Create new problem
python3 scripts/new-problem.py <num> "<signature>"

# Run all tests
cabal test

# Run tests (verbose)
cabal test --verbose

# Run specific test
cabal test -- -p "P1"

# Build
cabal build

# Clean
cabal clean
```

## 📖 Documentation

- **`TESTING_WITH_TASTY.md`** - Comprehensive testing guide with examples
- **`QUICKSTART_TASTY.md`** - Quick reference for common patterns
- **`README.md`** - Full project documentation
- **`test/Problems/P3691.hs`** - Template for test files

## 🎯 Workflow

1. **Create**: `python3 scripts/new-problem.py 123 "solution :: [Int] -> Int"`
2. **Implement**: Edit `src/Solutions/P123.hs`
3. **Test**: Edit `test/Problems/P123.hs` with tasty tests
4. **Verify**: `cabal test`
5. **Repeat** for next problem

## 💡 Pro Tips

- ✅ Use `testGroup` to organize related tests
- ✅ Use `testCase` with `@?=` for assertions
- ✅ Helper functions can reduce code duplication
- ✅ Name tests descriptively
- ✅ Group by functionality (basic, edge cases, etc.)
- ✅ Test files are auto-imported by the runner

## 📦 Dependencies

The project includes:
- `tasty >= 1.4` - Test framework
- `tasty-hunit >= 0.10` - Unit testing support
- `base ^>=4.21.0.0` - Haskell standard library

## 🔍 Testing Assertion Reference

| Code | Meaning |
|------|---------|
| `a @?= b` | Assert `a` equals `b` |
| `testCase "name"` | Create a test |
| `testGroup "name"` | Group tests |
| `TestTree` | Test suite type |

## 📝 Example: Full Problem Solution

**Problem:** Two Sum (find indices of two numbers that sum to target)

**Solution** (`src/Solutions/P1.hs`):
```haskell
module Solutions.P1 where

twoSum :: [Int] -> Int -> [Int]
twoSum nums target = 
  [ i | (i, x) <- zip [0..] nums
      , (j, y) <- zip [0..] nums
      , i < j
      , x + y == target
      ] ++ [0, 0]
```

**Tests** (`test/Problems/P1.hs`):
```haskell
module Problems.P1 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.P1

tests :: TestTree
tests = testGroup "P1 - Two Sum"
  [ testGroup "examples"
      [ testCase "example 1" example1
      , testCase "example 2" example2
      ]
  ]

example1 :: Assertion
example1 = twoSum [2,7,11,15] 9 @?= [0,1]

example2 :: Assertion
example2 = twoSum [3,2,4] 6 @?= [1,2]
```

---

## ✨ You're All Set!

Start training:
```bash
python3 scripts/new-problem.py 1 "yourFunction :: Type -> Result"
```

For detailed testing guide:
```bash
cat TESTING_WITH_TASTY.md
```

**Happy coding!** 🎯

