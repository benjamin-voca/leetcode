# LeetCode Haskell - Tasty Testing Workflow

## Visual Guide

```
┌─────────────────────────────────────────────────────────────┐
│              LeetCode Training Project Setup                 │
│                   (Haskell + Tasty)                          │
└─────────────────────────────────────────────────────────────┘

STEP 1: CREATE NEW PROBLEM
├─ Run: python3 scripts/new-problem.py 1 "twoSum :: [Int] -> Int"
├─ Creates:
│  ├─ src/Solutions/P1.hs (empty solution)
│  ├─ test/Problems/P1.hs (tasty test template)
│  └─ Auto-updates:
│     ├─ leetcode.cabal (adds modules)
│     └─ test/Main.hs (adds imports)
│
│  AUTO-GENERATED FILES:
│  ┌────────────────────────────────────────┐
│  │ src/Solutions/P1.hs                    │
│  ├────────────────────────────────────────┤
│  │ module Solutions.P1 where              │
│  │                                         │
│  │ twoSum :: [Int] -> Int = undefined     │
│  └────────────────────────────────────────┘
│
│  ┌────────────────────────────────────────┐
│  │ test/Problems/P1.hs                    │
│  ├────────────────────────────────────────┤
│  │ import Test.Tasty                      │
│  │ import Test.Tasty.HUnit                │
│  │ import Solutions.P1                    │
│  │                                         │
│  │ tests :: TestTree                      │
│  │ tests = testGroup "P1" [...]           │
│  └────────────────────────────────────────┘

STEP 2: IMPLEMENT SOLUTION
├─ Edit: src/Solutions/P1.hs
├─ Write your algorithm
│
│  EXAMPLE:
│  ┌────────────────────────────────────────┐
│  │ twoSum :: [Int] -> Int -> [Int]        │
│  │ twoSum nums target =                   │
│  │   [ i | (i, x) <- zip [0..] nums      │
│  │       , (j, y) <- zip [0..] nums      │
│  │       , i < j                          │
│  │       , x + y == target                │
│  │       ] ++ [0, 0]                      │
│  └────────────────────────────────────────┘

STEP 3: WRITE TESTS (TASTY)
├─ Edit: test/Problems/P1.hs
├─ Add test cases using tasty assertions
│
│  EXAMPLE:
│  ┌────────────────────────────────────────┐
│  │ tests :: TestTree                      │
│  │ tests = testGroup "P1"                 │
│  │   [ testcase "ex1" $                   │
│  │       twoSum [2,7,11,15] 9             │
│  │         @?= [0,1]                      │
│  │   , testcase "ex2" $                   │
│  │       twoSum [3,2,4] 6                 │
│  │         @?= [1,2]                      │
│  │   ]                                     │
│  └────────────────────────────────────────┘

STEP 4: RUN TESTS
├─ Command: cabal test
│
│  OUTPUT:
│  ┌────────────────────────────────────────┐
│  │ LeetCode Problems                      │
│  │   P3691                                 │
│  │     example test 1: OK                 │
│  │   P1                                    │
│  │     ex1: OK                            │
│  │     ex2: OK                            │
│  │                                         │
│  │ All 3 tests passed (0.01s)             │
│  │ Test suite leetcode-test: PASS         │
│  └────────────────────────────────────────┘

STEP 5: REPEAT
└─ Go back to STEP 1 for next problem
```

## File Dependencies

```
test/Main.hs
    ↓
    ├─→ test/Problems/P1.hs
    │       ↓
    │       └─→ src/Solutions/P1.hs
    │
    ├─→ test/Problems/P3691.hs
    │       ↓
    │       └─→ src/Solutions/P3691.hs
    │
    └─→ (all problem modules auto-imported)


src/Solutions/*.hs
    ↓
    └─→ All can import src/Helpers.hs
```

## Cabal Configuration Updates

When you run the script, these files auto-update:

**Before:**
```cabal
library
  exposed-modules:
    MyLib
    Helpers
    Solutions.P3691
```

**After creating P1:**
```cabal
library
  exposed-modules:
    MyLib
    Helpers
    Solutions.P3691
    Solutions.P1          ← Added!
```

```cabal
test-suite leetcode-test
  other-modules:
    Problems.P3691
    Problems.P1           ← Added!
```

## Test Execution Flow

```
cabal test
    ↓
    [Compile all modules]
    ↓
    [Run test/Main.hs]
    ↓
    defaultMain $ testGroup "LeetCode Problems"
        [ Problems.P3691.tests
        , Problems.P1.tests
        ]
    ↓
    [Tasty runs all test cases]
    ↓
    [Display results]
    ├─ PASS: All tests passed ✅
    ├─ FAIL: Show which tests failed ❌
    └─ ERROR: Show compilation/runtime errors
```

## Tasty Test Execution

```
testGroup "P1"
├─ testCase "example 1"
│  └─ (solution [2,7] 9 @?= [0,1])
│     ├─ Run solution
│     ├─ Compare result
│     └─ Report: OK or FAIL
│
├─ testCase "example 2"
│  └─ (solution [3,2] 5 @?= [1,0])
│     └─ Report: OK or FAIL
│
└─ [Aggregate results]
   └─ Display in tree format with colors
```

## Common Tasty Patterns

```
PATTERN 1: Single Assertion
    testCase "name" $ solution input @?= expected

PATTERN 2: Multiple Assertions
    testCase "name" $ do
        solution input1 @?= expected1
        solution input2 @?= expected2

PATTERN 3: Named Helper
    test1 :: Assertion
    test1 = solution [1,2] @?= 3
    
    testcase "test 1" test1

PATTERN 4: Grouped Tests
    testGroup "group"
        [ testcase "test1" ...
        , testcase "test2" ...
        ]
```

## Problem Naming Convention

All problems follow the format `P<number>`:

```
Problem Number → P<number> → File Names
      ↓
      1 ──→ P1 ──→ src/Solutions/P1.hs
                  test/Problems/P1.hs

    123 ──→ P123 ──→ src/Solutions/P123.hs
                    test/Problems/P123.hs

   3691 ──→ P3691 ──→ src/Solutions/P3691.hs
                     test/Problems/P3691.hs
```

## Quick Reference: Script Usage

```bash
# Create problem (generic signature)
python3 scripts/new-problem.py 1 "solve :: Int -> Int"

# Create problem (complex type)
python3 scripts/new-problem.py 2 "add :: [Int] -> [Int] -> [Int]"

# Create problem (tree operations)
python3 scripts/new-problem.py 3 "traverse :: Tree Int -> [Int]"

# Script automatically:
# 1. Creates solution file with signature
# 2. Creates test file with tasty template
# 3. Updates leetcode.cabal
# 4. Updates test/Main.hs
# 5. All ready to code!
```

## Testing Assertion Operators

```haskell
@?=   -- Equality check, primary assertion
      Example: result @?= expected

~=?   -- Floating point equality
      Example: 3.14 ~=? 3.14159 (within tolerance)

assertBool  -- Boolean assertion
      Example: assertBool "should be positive" (x > 0)

assertEqual -- Labeled equality
      Example: assertEqual "label" result expected
```

## Project Statistics Commands

```bash
# Count problems solved
ls src/Solutions/ | wc -l

# List all problem numbers
ls src/Solutions/ | sed 's/P\|.hs//g' | sort -n

# See all tests
cabal test -- --list

# Run only P1 tests
cabal test -- -p "P1"

# Run tests matching pattern
cabal test -- -p "basic"
```

---

**Start your LeetCode journey!** 🚀

```bash
python3 scripts/new-problem.py 1 "yourSolution :: Type -> Result"
```
