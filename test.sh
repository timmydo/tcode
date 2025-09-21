#!/bin/sh
# Test script for TLE (Timmy's Lisp Environment)

export CL_SOURCE_REGISTRY="$(pwd)//:"

# Parse command line arguments
VERBOSE=0
START_SERVER=0
SPECIFIC_TEST=""

while [ $# -gt 0 ]; do
    case "$1" in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        --start-server)
            START_SERVER=1
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS] [TEST_FILE]"
            echo ""
            echo "Run TLE (Timmy's Lisp Environment) tests"
            echo ""
            echo "OPTIONS:"
            echo "  -v, --verbose      Show all test output (verbose mode)"
            echo "  --start-server     Start TLE server before running tests"
            echo "  -h, --help         Show this help message"
            echo ""
            echo "ARGUMENTS:"
            echo "  TEST_FILE          Specific test file to run (e.g., test-dom.lisp)"
            echo "                     If not specified, all tests are run"
            echo ""
            echo "EXAMPLES:"
            echo "  $0                        # Run all tests (no server, quiet mode)"
            echo "  $0 -v                     # Run all tests (no server, verbose mode)"
            echo "  $0 --start-server         # Run all tests with TLE server"
            echo "  $0 test-dom.lisp          # Run only DOM tests (no server)"
            echo "  $0 --start-server -v test-dom.lisp   # Run DOM tests with server and verbose output"
            echo ""
            echo "Default mode is quiet - only failures are shown."
            echo "Use --verbose to see all test output including successes."
            echo "Use --start-server to start TLE server before running tests."
            exit 0
            ;;
        *)
            if [ -z "$SPECIFIC_TEST" ]; then
                SPECIFIC_TEST="$1"
            else
                echo "Error: Multiple test files specified"
                exit 1
            fi
            shift
            ;;
    esac
done

# Validate and normalize test file path if specified
if [ -n "$SPECIFIC_TEST" ]; then
    # Add tests/ prefix if not already present
    if [ ! -f "$SPECIFIC_TEST" ] && [ -f "tests/$SPECIFIC_TEST" ]; then
        SPECIFIC_TEST="tests/$SPECIFIC_TEST"
    fi
    
    # Verify the test file exists
    if [ ! -f "$SPECIFIC_TEST" ]; then
        echo "Error: Test file '$SPECIFIC_TEST' not found"
        echo "Available test files:"
        ls tests/test*.lisp tests/test_*.sh 2>/dev/null || echo "No test files found"
        exit 1
    fi
fi

if [ -n "$SPECIFIC_TEST" ]; then
    if [ $VERBOSE -eq 1 ]; then
        echo "Running specific test: $SPECIFIC_TEST (verbose mode)..."
    else
        echo "Running specific test: $SPECIFIC_TEST (quiet mode - only failures shown)..."
    fi
else
    if [ $VERBOSE -eq 1 ]; then
        echo "Starting TLE tests (verbose mode)..."
    else
        echo "Starting TLE tests (quiet mode - only failures shown)..."
    fi
fi

# Start TLE in background if requested
TLE_PID=""
if [ $START_SERVER -eq 1 ]; then
    if [ $VERBOSE -eq 1 ]; then
        echo "Starting TLE server..."
    fi
    ./start.sh &
    TLE_PID=$!

    # Wait for server to start
    if [ $VERBOSE -eq 1 ]; then
        echo "Waiting for server to start..."
    fi
    sleep 5
fi

# Run all tests in tests/ directory
TESTS_PASSED=0
TESTS_FAILED=0

# Run shell script tests
if [ -n "$SPECIFIC_TEST" ]; then
    # Run specific test only
    if [ "${SPECIFIC_TEST##*.}" = "sh" ]; then
        test_files="$SPECIFIC_TEST"
    else
        test_files=""
    fi
else
    # Run all shell script tests
    test_files="tests/test_*.sh"
fi

for test_file in $test_files; do
    if [ -f "$test_file" ]; then
        if [ $VERBOSE -eq 1 ]; then
            echo "Running $test_file..."
            if "$test_file"; then
                TESTS_PASSED=$((TESTS_PASSED + 1))
            else
                TESTS_FAILED=$((TESTS_FAILED + 1))
            fi
            echo ""
        else
            # Quiet mode - capture output and only show on failure
            if output=$("$test_file" 2>&1); then
                TESTS_PASSED=$((TESTS_PASSED + 1))
            else
                TESTS_FAILED=$((TESTS_FAILED + 1))
                echo "FAILED: $test_file"
                echo "$output"
                echo ""
            fi
        fi
    fi
done

# Run Lisp test files
if [ -n "$SPECIFIC_TEST" ]; then
    # Run specific test only
    if [ "${SPECIFIC_TEST##*.}" = "lisp" ]; then
        lisp_test_files="$SPECIFIC_TEST"
    else
        lisp_test_files=""
    fi
else
    # Run all Lisp tests
    lisp_test_files="tests/test*.lisp"
fi

for test_file in $lisp_test_files; do
    if [ -f "$test_file" ]; then
        # Extract the test function name from the file (assumes run-all-*-tests pattern)
        test_function=$(grep -o "defun run-all-[^(]*" "$test_file" | head -1 | sed 's/defun //')
        if [ -n "$test_function" ]; then
            if [ $VERBOSE -eq 1 ]; then
                echo "Running Lisp tests from $test_file..."
                if sbcl --noinform --no-userinit --no-sysinit --non-interactive \
                        --eval "(require \"asdf\")" \
                        --eval "(sb-int:set-floating-point-modes :traps nil)" \
                        --eval "(asdf:load-system :tcode)" \
                        --eval "(load \"$test_file\")" \
                        --eval "(if (tcode::$test_function) (sb-ext:exit :code 0) (sb-ext:exit :code 1))"; then
                    TESTS_PASSED=$((TESTS_PASSED + 1))
                    echo "✓ Lisp tests in $test_file passed"
                else
                    TESTS_FAILED=$((TESTS_FAILED + 1))
                    echo "✗ Lisp tests in $test_file failed"
                fi
                echo ""
            else
                # Quiet mode - capture output and only show on failure
                if output=$(sbcl --noinform --no-userinit --no-sysinit --non-interactive \
                        --eval "(require \"asdf\")" \
                        --eval "(sb-int:set-floating-point-modes :traps nil)" \
                        --eval "(asdf:load-system :tcode)" \
                        --eval "(load \"$test_file\")" \
                        --eval "(if (tcode::$test_function) (sb-ext:exit :code 0) (sb-ext:exit :code 1))" 2>&1); then
                    TESTS_PASSED=$((TESTS_PASSED + 1))
                else
                    TESTS_FAILED=$((TESTS_FAILED + 1))
                    echo "FAILED: $test_file"
                    echo "$output"
                    echo ""
                fi
            fi
        else
            echo "⚠ No test function found in $test_file (expected run-all-*-tests pattern)"
        fi
    fi
done

# Clean up - kill TLE server if it was started
if [ -n "$TLE_PID" ]; then
    if [ $VERBOSE -eq 1 ]; then
        echo "Stopping TLE server..."
    fi
    kill $TLE_PID 2>/dev/null
    wait $TLE_PID 2>/dev/null
fi

echo "Tests completed."
echo "Passed: $TESTS_PASSED, Failed: $TESTS_FAILED"

if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
else
    exit 0
fi
