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
- **jsown**: JSON parsing library for backend API communication
- **bordeaux-threads**: Threading library for concurrent operations
- **cffi**: Common Foreign Function Interface
- **drakma**: HTTP client library for backend API requests
- **flexi-streams**: Flexible stream handling
- **webview**: Native webview window creation
- **usocket**: Socket programming library for web server
- **babel**: Character encoding/decoding library
- Uses ASDF to load dependencies from vendor/
- Do not use quicklisp

## Architecture

tcode is a webview-based CLI application written in Common Lisp that provides an interactive REPL environment through a web browser interface with backend AI integration.

### Core Components

#### Entry Point (`src/main.lisp`)
- **Main function**: Entry point that initializes logging, loads configuration, and starts the webview application
- **Web server startup**: Creates and launches a web server in a background thread
- **Webview integration**: Uses webview library to create a native window displaying the web interface
- **Port management**: Automatically finds available ports or uses TCODE_PORT environment variable
- **Graceful exit**: Handles application shutdown when webview closes

#### Web User Interface (`src/web-ui.lisp`)
- **HTTP server**: Custom web server using usocket for handling client connections
- **HTML template**: Terminal-styled web interface with command input and scrollable history
- **Real-time updates**: Server-Sent Events (SSE) for streaming command results to browser
- **DOM reconciliation**: Client-side JavaScript for efficient UI updates without full page reloads
- **REST endpoints**: `/command` for command submission, `/history` for history retrieval, `/quit` for application exit

#### Command Processing (`src/eval.lisp`)
- **Command evaluation**: Processes user commands and special tcode commands
- **Special commands**: `/add`, `/rmdir` for directory context management, `/clear` for history, `/config` for configuration, `/lorem` for test data
- **History management**: Thread-safe history management with mutex locks
- **Backend integration**: Routes non-special commands to configured AI backend

#### Data Structures (`src/struct.lisp`)
- **History items**: Stores command, result, and usage data for each interaction
- **REPL context**: Manages application state including history, directories, and threading context
- **Thread safety**: Mutex support for concurrent access to shared state

#### Backend Integration (`src/backend.lisp`)
- **Abstract backend interface**: Base class for different AI backend connections
- **OpenRouter implementation**: HTTP streaming client for OpenRouter API with conversation context
- **Streaming responses**: Real-time response processing with incremental UI updates
- **Usage tracking**: Captures and displays token usage and cost information
- **Background processing**: Non-blocking command execution using threads

#### Configuration System (`src/config.lisp`)
- **Config file management**: Creates and loads `~/.tcode/config.lisp` with user customizations
- **Backend setup**: Configures API connections (currently OpenRouter for AI integration)
- **Template system**: Provides default configuration template with examples
- **Directory management**: Handles ~/.tcode directory creation and file initialization

#### Logging System (`src/logs.lisp`)
- **Structured logging**: Timestamped log entries with different severity levels
- **File and console output**: Logs to both stdout and timestamped log files
- **Configurable logging**: Can be disabled via TCODE_NO_LOGS environment variable
- **Session tracking**: Logs application lifecycle and command execution

#### Threading Utilities (`src/locks.lisp`)
- **Lock management**: Wrapper functions for creating and managing mutex locks
- **Thread utilities**: Helper functions for thread creation and management with logging
- **Consistent naming**: Centralized thread naming and lifecycle management

#### DOM Generation (`src/dom.lisp`)
- **Virtual DOM**: Lisp-based DOM generation for HTML content
- **JSON serialization**: Converts DOM structures to JSON for client-side rendering
- **HTML escaping**: Safe text rendering with proper character escaping
- **Helper functions**: Convenient functions for creating common HTML elements

#### Package Definition (`src/tcode-package.lisp`)
- **Namespace management**: Defines the `:tcode` package with exported symbols
- **Public API**: Exposes main entry points, backend classes, and utility functions
- **Dependency management**: Local nicknames for external libraries

### Key Features

- **Webview interface**: Native application window with web-based UI
- **Real-time streaming**: Live command results via Server-Sent Events
- **AI integration**: Pluggable backend system for different AI providers
- **Conversation context**: Maintains full conversation history for AI interactions
- **Directory context**: Track multiple directory contexts for file operations
- **Thread-safe operations**: Concurrent command processing with proper synchronization
- **Usage analytics**: Token usage and cost tracking for AI interactions
- **Comprehensive logging**: Detailed logging system for debugging and monitoring
