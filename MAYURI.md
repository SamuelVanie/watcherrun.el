# Project Overview

WatcherRun.el is an Emacs package that lets you run system commands or Emacs Lisp expressions automatically when files change. It uses Emacs’ native file-notify APIs (no external daemons) and integrates with Dired and an Emacs menu for a smooth workflow.

Why it exists:
- Rapid feedback: recompile, test, or reload code as soon as files change.
- Native Emacs experience: no dependencies like entr/entr-runner; works cross-platform within Emacs.
- Multiple watchers: manage several concurrent tasks, including separate compilation buffers.

# Getting Started (Local Development)

## Prerequisites
- Emacs 27+ (required for file-notify and modern APIs).
- OS: Linux, macOS, or Windows (uses Emacs-native backends: inotify/fsevents/kqueue/w32).
- Recommended: A shell in PATH for system commands you plan to run (e.g., make, npm, python).
- No external Emacs packages required; aims to use built-in libraries.


## Running Tests
Framework: ERT

### Running All Tests
```sh
emacs -Q --batch -l ert -l watcherrun-support.el -l watcherrun-utils.el -l test/test-support.el -l test/test-utils.el -f ert-run-tests-batch-and-exit
```

### Running Tests for Specific Components
```sh
# For support components only
emacs -Q --batch -l ert -l watcherrun-support.el -l test/test-support.el -f ert-run-tests-batch-and-exit

# For utility functions only
emacs -Q --batch -l ert -l watcherrun-support.el -l watcherrun-utils.el -l test/test-utils.el -f ert-run-tests-batch-and-exit
```

### Writing Tests
When implementing new features, follow these testing guidelines:

1. Create a test file named `test-<component>.el` in the `test/` directory
2. Include setup/teardown functions for creating temporary test resources
3. Use `ert-deftest` to define tests with descriptive names (`watcherrun-test-<function-name>`)
4. Test both happy paths and error cases
5. For file operations, create and clean up temporary files
6. Run tests everytime you implement changes into the files related to functionalities of package

# Codebase Structure & Key Areas

## High-Level Components
- User Interface Layer: Menu bar, Dired integration, interactive commands for adding/listing/modifying watchers.
- Core Engine: Watcher manager, file-notify wrapper, command executor (routing).
- Execution Layer: System command runner, Lisp expression evaluator, compilation buffer manager.
- Support Components: Error handling, session storage (in-memory), utilities.

## Command Executor Functions (CORE-002)
The following functions are implemented in watcherrun-core.el:
- `watcherrun-determine-command-type`: Intelligently detects if command is system or lisp
- `watcherrun-validate-command`: Validates commands based on type
- `watcherrun-execute-command`: Main dispatcher that handles execution with statistics tracking
- `watcherrun--execute-system-command`: Executes system commands in dedicated buffers
- `watcherrun--execute-lisp-command`: Safely executes Lisp expressions

Testing framework: All functions have comprehensive tests in test/test-core-executor.el

See .mayuri/ for design details:
- architecture_overview.md
- component_core_engine.md
- component_execution_layer.md
- component_support_components.md
- component_user_interface.md

## Core Directories & Entry Points

### Current Directory Structure
```
~/projects/watcherrun.el/
├── .mayuri/           # Design documents and architecture details
├── test/              # Test directory
│   ├── test-support.el # Tests for support components
│   └── test-utils.el   # Tests for utility functions
├── LICENSE            # Project license
├── MAYURI.md          # This development guide
├── project_spec.org   # Project specifications
├── watcherrun-support.el # Support components implementation
├── watcherrun-utils.el   # Utility functions implementation
└── watcherrun.el      # Main package file
```

### Planned Files (align with your preferred naming once implemented):
- watcherrun.el: Package entry; provides user-facing commands and sets up the menu.
- watcherrun-core.el: Watcher registry, add/remove/modify/list, file-notify setup and callbacks.
- watcherrun-exec.el: Command dispatch; system vs Lisp evaluations; placeholder expansion.
- watcherrun-compilation.el: Compilation buffer naming/management for concurrent builds.
- watcherrun-support.el: Error handler, session storage, utilities.
- watcherrun-utils.el: Path validation, command parsing, ID generation, file type detection.
- ui/watcherrun-dired.el: Dired integration, interactive prompts.
- test/*.el: ERT tests per component.

Key starting points to read first:
1) Core manager API: add/remove/modify/list watcher functions.
2) File-notify event handler and event filtering/debouncing.
3) Execution routing and placeholder expansion.
4) UI commands in Dired and the menu setup.

## Architectural Patterns & Conventions
- Modular monolith: clear logical separation by component; all within one package namespace.
- Namespacing: watcherrun- or watcherrun-- prefix for all functions/vars (public or private).
- Error-centric design: central error buffer “*WatcherRun Errors*” and structured logs.
- Session-only data: no persistence by default (avoid file corruption, keep it simple).
- Async-first: system commands via start-process or equivalent; compilation via compile with automatic buffer rename.
- Emacs-native tools: file-notify.el, easy-menu.el, dired.el, compile.el.


# Implementation Status
- ✅ Menu bar interface with keyboard shortcuts (C-c w [l|a|d|m|e])
- ✅ All menu commands with proper error handling and user feedback
- ✅ Dynamic menu state management based on watcher count
- ✅ Support components with 6 passing tests
- ✅ Utility functions with 7 passing tests
- ✅ System command execution layer with 9 passing tests
- ✅ Process management and async execution implemented
- ✅ Compilation buffer auto-renaming working
- ✅ Process cleanup and resource management
- ✅ Package activation/deactivation with `watcherrun-mode`
- ✅ Integration tests with 6 passing tests
- ✅ All components properly connected and working together
- ✅ Comprehensive documentation suite (README, API, Examples, Troubleshooting)
- ✅ Improved docstrings and inline documentation
- ✅ Documentation validation tests passing
- ✅ MELPA-ready package structure
- ✅ All existing tests still passing with documentation updates

# Important Notes & Tips
- File-notify backends vary by OS. Some events differ or are noisy; event filtering/debouncing helps.
- TRAMP/remote files: file-notify may be limited; consider local-only watchers in v1.
- Compilation buffers: Emacs by default uses "*compilation*". This project auto-renames buffers per watcher to allow multiple parallel compilations.
- System commands may require environment setup (PATH). If Emacs doesn’t inherit your shell environment, configure exec-path-from-shell on macOS.
- Safety for Lisp execution: validation blocks dangerous functions; prefer explicit, minimal expressions.

# Development Tips

## Workflow Optimization
- Start by checking MAYURI.md for updated project structure and testing instructions
- Before adding new features, run existing tests to verify nothing is broken
- Key utility functions are already implemented in watcherrun-utils.el:
  - `watcherrun-validate-path`: Path validation with informative errors
  - `watcherrun-normalize-path`: Cross-platform path normalization
  - `watcherrun-parse-command`: Command parsing with type detection
  - `watcherrun-expand-placeholders`: Template variable expansion
  - `watcherrun-generate-unique-id`: Watcher ID generation
  - `watcherrun-detect-file-type`: File type detection by extension
  - `watcherrun-suggest-command`: Smart command suggestions

## Dired Integration
- Implemented in watcherrun-ui.el with keybinding 'W' in Dired mode
- Functions: `watcherrun-dired-add-watcher`, `watcherrun--prompt-command-type`, `watcherrun--prompt-recursion-for-directories`
- Tests cover marking files, command prompts, and error handling scenarios

## Token & Tool Call Optimization
- Use list-directory on project root once to understand structure instead of multiple specific path checks
- When testing, modify existing test files rather than creating redundant new ones
- Refer to existing implementations for coding style and patterns
- The project uses a consistent naming scheme: `watcherrun-<component>-<function>`
- Store common references in variables rather than repeatedly looking them up

## Interactive Command Features (UI-003)
- ✅ Implemented `watcherrun-add-watcher-interactive` with tab completion using `read-file-name`
- ✅ Uses same prompt functions as Dired integration (`watcherrun--prompt-command-type`, `watcherrun--prompt-recursion-for-directories`)
- ✅ Path validation with `watcherrun-validate-path` and comprehensive error handling
- ✅ 4 new test cases cover file/directory creation, invalid paths, and completion features
- ✅ All 11 UI tests pass, existing tests still pass

---
Continuous improvement
This guide is living documentation. If you find gaps or friction points, please update MAYURI.md
