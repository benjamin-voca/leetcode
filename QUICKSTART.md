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
   - `test/Problems/P1.hs` - Your test cases go here
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

3. **Write test cases** in `test/Problems/P<number>.hs`:
   ```haskell
   module Problems.P1 where

   import Solutions.P1

   tests :: IO ()
   tests = do
     putStrLn "P1: Running tests..."
     
     let result1 = twoSum [2, 7, 11, 15] 9
     putStrLn $ "Test 1: " ++ 
       (if result1 == [(0, 1)] then "PASS" else "FAIL")
     
     putStrLn "P1: All tests passed!"
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

## Testing Tips

- Write multiple test cases
- Test edge cases (empty input, single element, etc.)
- Use pattern matching for clean assertions
- Group related tests together

Example test pattern:
```haskell
tests :: IO ()
tests = do
  putStrLn "P123: Running tests..."
  
  -- Test group 1: Basic cases
  assertEq "basic 1" (solution [1,2,3]) 6
  assertEq "basic 2" (solution []) 0
  
  -- Test group 2: Edge cases
  assertEq "edge 1" (solution [1]) 1
  
  putStrLn "P123: All tests passed!"

assertEq :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEq name actual expected =
  if actual == expected
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (got " ++ show actual ++ ")"
```

## Building

```bash
# Build everything
cabal build

# Build and run executable
cabal run leetcode

# Run tests
cabal test

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

---

**Happy problem solving!** 🚀
