#!/usr/bin/env python3
"""
Script to create a new LeetCode problem template.
Usage: python3 new-problem.py <problem-number> "<function-signature>"
Example: python3 new-problem.py 3691 "solution :: [Int] -> Int"
"""

import sys
import re
from pathlib import Path

def add_to_cabal_exposed_modules(content: str, problem_name: str) -> str:
    """Add a new Solutions module to exposed-modules in the library section."""
    if f"Solutions.{problem_name}" in content:
        return content
    
    lines = content.split('\n')
    in_exposed = False
    last_solution_idx = -1
    
    for i, line in enumerate(lines):
        if 'exposed-modules:' in line and 'library' in '\n'.join(lines[max(0, i-5):i]):
            in_exposed = True
        elif in_exposed and line.startswith('    ') and line.strip() and not line.strip().startswith('--'):
            if 'Solutions.' in line:
                last_solution_idx = i
            elif not line.strip().startswith('Solutions.') and last_solution_idx != -1:
                break
    
    if last_solution_idx != -1:
        if not lines[last_solution_idx].rstrip().endswith(','):
            lines[last_solution_idx] = lines[last_solution_idx].rstrip() + ','
        indent = len(lines[last_solution_idx]) - len(lines[last_solution_idx].lstrip())
        lines.insert(last_solution_idx + 1, ' ' * indent + f"Solutions.{problem_name}")
    else:
        for i, line in enumerate(lines):
            if 'Helpers' in line and 'exposed-modules' in '\n'.join(lines[max(0, i-5):i]):
                if not line.rstrip().endswith(','):
                    lines[i] = lines[i].rstrip() + ','
                indent = len(lines[i]) - len(lines[i].lstrip())
                lines.insert(i + 1, ' ' * indent + f"Solutions.{problem_name}")
                break
    
    return '\n'.join(lines)

def add_to_cabal_test_modules(content: str, problem_name: str) -> str:
    """Add a new Problems module to other-modules in the test-suite section."""
    if f"Problems.{problem_name}" in content:
        return content
    
    lines = content.split('\n')
    in_test_other = False
    last_problem_idx = -1
    
    for i, line in enumerate(lines):
        if 'other-modules:' in line and 'test-suite' in '\n'.join(lines[max(0, i-5):i]):
            in_test_other = True
        elif in_test_other and line.startswith('    ') and line.strip() and not line.strip().startswith('--'):
            if 'Problems.' in line:
                last_problem_idx = i
            elif not line.strip().startswith('Problems.') and last_problem_idx != -1:
                break
    
    if last_problem_idx != -1:
        indent = len(lines[last_problem_idx]) - len(lines[last_problem_idx].lstrip())
        lines.insert(last_problem_idx + 1, ' ' * indent + f"Problems.{problem_name}")
    else:
        for i, line in enumerate(lines):
            if 'other-modules:' in line and 'test-suite' in '\n'.join(lines[max(0, i-5):i]):
                indent = len(line) - len(line.lstrip()) + 4
                lines.insert(i + 1, ' ' * indent + f"Problems.{problem_name}")
                break
    
    return '\n'.join(lines)

def add_to_test_main(content: str, problem_name: str, problem_num: str) -> str:
    """Add import and test call to test/Main.hs."""
    if f"Problems.{problem_name}" in content:
        return content
    
    lines = content.split('\n')
    import_idx = -1
    
    # Find where to insert import (look for last "import qualified Problems." line)
    for i, line in enumerate(lines):
        if 'import qualified Problems.' in line or 'import Problems.' in line:
            import_idx = i
    
    if import_idx != -1:
        lines.insert(import_idx + 1, f"import qualified Problems.{problem_name}")
    
    # Find where to insert test call (look for the testGroup list)
    test_list_idx = -1
    for i, line in enumerate(lines):
        if 'testGroup' in line and '[' in line:
            test_list_idx = i
            break
    
    if test_list_idx == -1:
        # Find the closing bracket of the test group
        for i, line in enumerate(lines):
            if 'testGroup' in line:
                # Find the matching closing bracket
                for j in range(i, len(lines)):
                    if ']' in lines[j]:
                        test_list_idx = j - 1
                        break
                break
    
    if test_list_idx != -1:
        # Insert before the closing bracket
        lines.insert(test_list_idx + 1, f"    , Problems.{problem_name}.tests")
    
    return '\n'.join(lines)

def main():
    if len(sys.argv) < 3:
        print("Usage: python3 new-problem.py <problem-number> \"<function-signature>\"")
        print("Example: python3 new-problem.py 3691 \"solution :: [Int] -> Int\"")
        sys.exit(1)
    
    problem_num = sys.argv[1]
    fn_signature = sys.argv[2]
    
    if not problem_num.isdigit():
        print(f"Error: Problem number must be numeric (got: {problem_num})")
        sys.exit(1)
    
    problem_name = f"P{problem_num}"
    script_dir = Path(__file__).parent
    root_dir = script_dir.parent
    
    solution_file = root_dir / "src" / "Solutions" / f"{problem_name}.hs"
    test_file = root_dir / "test" / "Problems" / f"{problem_name}.hs"
    
    if solution_file.exists() or test_file.exists():
        print(f"Error: Problem {problem_name} already exists")
        if solution_file.exists():
            print(f"  {solution_file}")
        if test_file.exists():
            print(f"  {test_file}")
        sys.exit(1)
    
    print(f"Creating {solution_file}...")
    solution_content = f"""-- | Problem {problem_name} Solution
-- TODO: Add problem description and link
module Solutions.{problem_name} where

-- Solution implementation here
{fn_signature} = undefined
"""
    solution_file.write_text(solution_content)
    
    print(f"Creating {test_file}...")
    test_content = f"""-- | Test suite for problem {problem_name}
module Problems.{problem_name} where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)
import Solutions.{problem_name}

-- Define test cases here
tests :: TestTree
tests = testGroup "{problem_name}"
  [ testCase "example test 1" example1
  -- Add more test cases here
  ]

-- Example test
example1 :: Assertion
example1 = True @?= True
  -- Replace with your actual test:
  -- solution input @?= expected
"""
    test_file.write_text(test_content)
    
    print("Updating leetcode.cabal...")
    cabal_file = root_dir / "leetcode.cabal"
    cabal_content = cabal_file.read_text()
    
    cabal_content = add_to_cabal_exposed_modules(cabal_content, problem_name)
    cabal_content = add_to_cabal_test_modules(cabal_content, problem_name)
    cabal_file.write_text(cabal_content)
    
    print("Updating test/Main.hs...")
    main_test_file = root_dir / "test" / "Main.hs"
    main_content = main_test_file.read_text()
    main_content = add_to_test_main(main_content, problem_name, problem_num)
    main_test_file.write_text(main_content)
    
    print(f"✓ Created problem {problem_name}")
    print(f"  Solution: {solution_file}")
    print(f"  Tests: {test_file}")
    print()
    print("Next steps:")
    print(f"  1. Edit {solution_file} and implement your solution")
    print(f"  2. Edit {test_file} and add test cases")
    print("  3. Run: cabal test")

if __name__ == "__main__":
    main()
