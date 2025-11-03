# LeetCode Haskell Project - Setup Complete! 🎉

Your LeetCode training project is now fully configured and ready to use. Here's what has been set up:

## What You Can Do Now

### 1. **Create New Problems Instantly**
```bash
python3 scripts/new-problem.py <problem-number> "<function-signature>"
```

Example:
```bash
python3 scripts/new-problem.py 1 "twoSum :: [Int] -> Int -> [Int]"
```

The script automatically:
- Creates `src/Solutions/P1.hs` with your function signature
- Creates `test/Problems/P1.hs` with a test template
- Updates `leetcode.cabal` to include both modules
- Updates `test/Main.hs` to import and run the new test

### 2. **Organize Solutions & Tests**
- **Solutions**: `src/Solutions/P<number>.hs`
- **Tests**: `test/Problems/P<number>.hs`
- **Helpers**: `src/Helpers.hs` (available to all solutions)

### 3. **Generic Helpers Library**
Add reusable utilities to `src/Helpers.hs`:
```haskell
-- Example helpers
isEven :: Int -> Bool
treeSum :: Tree Int -> Int
-- ... your utilities
```

Import in any solution:
```haskell
import Helpers

solution x = if isEven x then x * 2 else x
```

## Project Structure

```
leetcode/
├── src/
│   ├── Helpers.hs           # ← Generic helpers (lists, trees, etc.)
│   ├── MyLib.hs
│   └── Solutions/
│       ├── P1.hs            # Your solutions go here
│       ├── P3691.hs
│       └── ...
├── test/
│   ├── Main.hs              # Test runner (auto-updated)
│   └── Problems/
│       ├── P1.hs            # Your tests go here
│       ├── P3691.hs
│       └── ...
├── scripts/
│   └── new-problem.py       # ← Use this to create problems!
└── leetcode.cabal           # Auto-updated
```

## Quick Workflow

1. **Create problem**:
   ```bash
   python3 scripts/new-problem.py 2 "addTwoNumbers :: List -> List -> List"
   ```

2. **Implement solution** in `src/Solutions/P2.hs`:
   ```haskell
   module Solutions.P2 where
   
   import Helpers
   
   addTwoNumbers :: List -> List -> List
   addTwoNumbers a b = -- your implementation
   ```

3. **Write tests** in `test/Problems/P2.hs`:
   ```haskell
   module Problems.P2 where
   
   import Solutions.P2
   
   tests :: IO ()
   tests = do
     putStrLn "P2: Testing..."
     let result = addTwoNumbers [2,4,3] [5,6,4]
     putStrLn $ "Test 1: " ++ show result
   ```

4. **Run tests**:
   ```bash
   cabal test
   ```

## Available Commands

```bash
# Build everything
cabal build

# Run all tests
cabal test

# Run with verbose output
cabal test --verbose

# Clean build
cabal clean

# Check for warnings
cabal build 2>&1 | grep warning
```

## Adding Dependencies

Edit `leetcode.cabal`:
```cabal
library
    build-depends:
        base ^>=4.21.0.0,
        containers >= 0.6,    -- Add here
        vector >= 0.12
```

Run:
```bash
cabal update && cabal build
```

## Tips & Tricks

- ✅ Each problem gets isolated test and solution files
- ✅ All solutions are automatically imported by tests
- ✅ Helpers are available everywhere
- ✅ Script handles cabal updates automatically
- ✅ Tests run in order (P1, then P3691, etc.)

## File Examples

**Solution template** (`src/Solutions/P<num>.hs`):
```haskell
-- | Problem P123 Solution
-- Description: Your problem description here
-- Link: https://leetcode.com/problems/your-problem/
module Solutions.P123 where

import Helpers

solution :: Int -> Int
solution = undefined
```

**Test template** (`test/Problems/P<num>.hs`):
```haskell
-- | Tests for problem P123
module Problems.P123 where

import Solutions.P123

tests :: IO ()
tests = do
  putStrLn "P123: Running tests..."
  -- Test 1
  putStrLn "P123: All tests passed!"
```

---

**Ready to start?** 🚀

```bash
python3 scripts/new-problem.py 123 "yourFunction :: Type -> Type"
```

Have fun training! 💪
