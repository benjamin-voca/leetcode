#!/usr/bin/env fish
# Script to create a new LeetCode problem template
# Usage: ./scripts/new-problem.sh <problem-number> "<function-signature>"
# Example: ./scripts/new-problem.sh 3691 "solution :: [Int] -> Int"

if test (count $argv) -lt 2
    echo "Usage: $argv[0] <problem-number> \"<function-signature>\""
    echo "Example: $argv[0] 3691 \"solution :: [Int] -> Int\""
    exit 1
end

set problem_num $argv[1]
set fn_signature $argv[2]
set problem_name "P$problem_num"
set script_dir (cd (dirname (status -f)); pwd)
set root_dir (dirname $script_dir)

# Validate problem number
if not string match -qr '^\d+$' $problem_num
    echo "Error: Problem number must be numeric (got: $problem_num)"
    exit 1
end

set solution_file "$root_dir/src/Solutions/$problem_name.hs"
set test_file "$root_dir/test/Problems/$problem_name.hs"

# Check if problem already exists
if test -e $solution_file -o -e $test_file
    echo "Error: Problem $problem_name already exists"
    echo "  $solution_file"
    echo "  $test_file"
    exit 1
end

# Create solution file
echo "Creating $solution_file..."
cat > $solution_file << EOF
-- | Problem $problem_name Solution
-- TODO: Add problem description and link
module Solutions.$problem_name where

-- Solution implementation here
$fn_signature = undefined
EOF

# Create test file
echo "Creating $test_file..."
cat > $test_file << EOF
-- | Test suite for problem $problem_name
module Problems.$problem_name where

import Solutions.$problem_name

-- Define test cases here
tests :: IO ()
tests = do
  putStrLn "$problem_name: Running tests..."
  -- Example test:
  -- let result = solution testInput
  -- putStrLn $ "Test 1: " ++ (if result == expected then "PASS" else "FAIL")
  putStrLn "$problem_name: All tests passed!"
EOF

# Update cabal file to include the new modules
echo "Updating leetcode.cabal..."

# Read the current cabal file
set cabal_file "$root_dir/leetcode.cabal"

# Add to other-modules in library section if not already there
if not grep -q "Solutions.$problem_name" $cabal_file
    # Find the line with the last Solutions module or other-modules line
    set insert_line (grep -n "Solutions\." $cabal_file | tail -1 | cut -d: -f1)
    
    if test -z "$insert_line"
        set insert_line (grep -n "other-modules:" $cabal_file | head -1 | cut -d: -f1)
        if test -z "$insert_line"
            echo "Warning: Could not find insertion point in cabal file"
        else
            set insert_line (math $insert_line + 1)
        end
    else
        set insert_line (math $insert_line + 1)
    end
    
    if test -n "$insert_line"
        sed -i '' "${insert_line}i\\
\        Solutions.$problem_name
" $cabal_file
    end
end

# Add to other-modules in test-suite section if not already there
if not grep -q "Problems.$problem_name" $cabal_file
    set test_insert_line (grep -n "Problems\." $cabal_file | tail -1 | cut -d: -f1)
    
    if test -z "$test_insert_line"
        set test_insert_line (grep -n "other-modules:" $cabal_file | tail -1 | cut -d: -f1)
        if test -z "$test_insert_line"
            echo "Warning: Could not find test insertion point in cabal file"
        else
            set test_insert_line (math $test_insert_line + 1)
        end
    else
        set test_insert_line (math $test_insert_line + 1)
    end
    
    if test -n "$test_insert_line"
        sed -i '' "${test_insert_line}i\\
\        Problems.$problem_name
" $cabal_file
    end
end

# Update test/Main.hs to import the new test
echo "Updating test/Main.hs..."
set main_test_file "$root_dir/test/Main.hs"

if not grep -q "Problems.$problem_name" $main_test_file
    # Find the imports section and add the new import
    set import_line (grep -n "import Problems" $main_test_file | tail -1)
    
    if test -n "$import_line"
        set line_num (echo $import_line | cut -d: -f1)
        set line_num (math $line_num + 1)
        sed -i '' "${line_num}i\\
import Problems.$problem_name (tests as tests_$problem_num)
" $main_test_file
    else
        echo "Warning: Could not add import to test/Main.hs"
    end
    
    # Add test call in main function
    set main_line (grep -n "tests" $main_test_file | tail -1 | cut -d: -f1)
    if test -n "$main_line"
        set main_line (math $main_line + 1)
        sed -i '' "${main_line}i\\
\  tests_$problem_num
" $main_test_file
    end
end

echo "✓ Created problem $problem_name"
echo "  Solution: $solution_file"
echo "  Tests: $test_file"
echo ""
echo "Next steps:"
echo "  1. Edit $solution_file and implement your solution"
echo "  2. Edit $test_file and add test cases"
echo "  3. Run: cabal test"
