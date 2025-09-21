# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tcode is a terminal-based CLI for coding

## Development Commands

### Building and Running
- **Quick start**: `./start.sh` - convenience script to start
- **Entry point**: `src/main.lisp` contains the main function

### Testing
- When running tests, unless told otherwise, specify the relevant test file. Do not run all the tests. You should not need to start TLE to run the tests.
- **Run specific test**: `./test.sh test-dom.lisp` - runs only the specified test file
- **Run specific test (verbose)**: `./test.sh -v test-dom.lisp` - runs specific test with full output
- **Run all tests**: `./test.sh` - runs all tests (quiet mode, only failures shown). Only do when requested specifically.
- **Help**: `./test.sh -h` - shows usage information and examples
- **Test files location**: Tests are located in the `tests/` directory
  - Lisp test files: `test*.lisp` (must contain `run-all-*-tests` function)

### Utilities
- **Parenthesis checker**: `util/paren-checker.lisp` - checks for mismatched parentheses in Common Lisp source files
  - **Usage**: `util/paren-checker.lisp filename.lisp`

### Dependencies
- **jsown**: JSON parsing library
- **bordeaux-threads**: Threading library for concurrent operations
- **cffi**: Common Foreign Function Interface
- Uses ASDF to load dependencies from vendor/
- Do not use quicklisp

## Architecture

TBD
