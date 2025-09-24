# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tcode is a terminal-based CLI for coding

## Development Commands

### Building and Running
- **IMPORTANT**: Do NOT run `./start.sh` as it takes over the Claude Code PTY and cannot be killed easily
- **Entry point**: `src/main.lisp` contains the main function
- **Test mode**: `./compile.sh` - compiles and loads the system without starting PTY operations, allowing you to see any SBCL compilation messages. Useful for checking compilation errors.
- start.sh/test.sh shows how to load the crypto libraries properly. You can't run lisp files individually.

### Testing
- When running tests, unless told otherwise, specify the relevant test file. Do not run all the tests. You should not need to start TLE to run the tests.
- **Run specific test**: `./test.sh test-rmdir.lisp` - runs only the specified test file
- **Run specific test (verbose)**: `./test.sh -v test-rmdir.lisp` - runs specific test with full output
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

tcode is a terminal-based CLI application written in Common Lisp that provides an interactive REPL environment with curses-like terminal control and PTY management.

### Core Components

#### Entry Point (`src/main.lisp`)
- **Main function**: Entry point that initializes the terminal interface and starts the REPL loop
- **Terminal management**: Sets up raw mode for direct character input and curses-like screen control
- **REPL context**: Manages user input, command history, cursor positioning, and scrollable content display
- **Screen layout**: Fixed prompt at bottom with scrollable history area above, plus status line

#### PTY Interface (`src/pty.lisp`)
- **Low-level terminal control**: CFFI bindings for POSIX PTY operations, terminal attributes, and process management
- **Raw mode support**: Enables character-by-character input without line buffering
- **Terminal I/O**: Cursor movement, screen clearing, color management, and escape sequence handling
- **Window size detection**: Gets terminal dimensions for proper layout

#### Command Processing (`src/eval.lisp`)
- **Command evaluation**: Safely evaluates Lisp expressions with error handling
- **Special commands**: `/add`, `/rmdir` for directory context management, `/clear` for history, `/config` for configuration
- **History management**: Maintains command history with results in the REPL context

#### Configuration System (`src/config.lisp`)
- **Config file management**: Creates and loads `~/.tcode/config.lisp` with user customizations
- **Backend setup**: Configures API connections (currently OpenRouter for AI integration)
- **Template system**: Provides default configuration template with examples

#### Backend Integration (`src/backend.lisp`)
- **Abstract backend interface**: Base class for different backend connections
- **OpenRouter implementation**: Concrete implementation for OpenRouter API integration
- **Extensible design**: Allows for additional backend types

#### Package Definition (`src/tcode-package.lisp`)
- **Namespace management**: Defines the `:tcode` package with exported symbols
- **Public API**: Exposes main entry points and backend connection classes

### Key Features

- **Interactive REPL**: Full Common Lisp evaluation with persistent history
- **Terminal UI**: Curses-like interface with fixed prompt and scrollable content
- **Keyboard navigation**: Arrow keys, page up/down, home/end, and standard editing shortcuts
- **Directory context**: Track multiple directory contexts for file operations
- **Configurable backends**: Pluggable backend system for AI integration
- **Error handling**: Robust error handling throughout the stack
