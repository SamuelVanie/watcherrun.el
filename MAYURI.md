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
Typical batch command pattern (adjust file paths when tests exist):
```sh
emacs -Q --batch -l ert -l test/test-watcherrun.el -f ert-run-tests-batch-and-exit
```
If tests are split:
```sh
emacs -Q --batch -l ert -l test/test-core.el -l test/test-exec.el -f ert-run-tests-batch-and-exit
```

# Codebase Structure & Key Areas

## High-Level Components
- User Interface Layer: Menu bar, Dired integration, interactive commands for adding/listing/modifying watchers.
- Core Engine: Watcher manager, file-notify wrapper, command executor (routing).
- Execution Layer: System command runner, Lisp expression evaluator, compilation buffer manager.
- Support Components: Error handling, session storage (in-memory), utilities.

See .mayuri/ for design details:
- architecture_overview.md
- component_core_engine.md
- component_execution_layer.md
- component_support_components.md
- component_user_interface.md

## Core Directories & Entry Points
Planned/expected files (align with your preferred naming once implemented):
- watcherrun.el: Package entry; provides user-facing commands and sets up the menu.
- watcherrun-core.el: Watcher registry, add/remove/modify/list, file-notify setup and callbacks.
- watcherrun-exec.el: Command dispatch; system vs Lisp evaluations; placeholder expansion.
- watcherrun-compilation.el: Compilation buffer naming/management for concurrent builds.
- watcherrun-support.el: Error handler, session storage, utilities.
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


# Important Notes & Tips
- File-notify backends vary by OS. Some events differ or are noisy; event filtering/debouncing helps.
- TRAMP/remote files: file-notify may be limited; consider local-only watchers in v1.
- Compilation buffers: Emacs by default uses "*compilation*". This project auto-renames buffers per watcher to allow multiple parallel compilations.
- System commands may require environment setup (PATH). If Emacs doesn’t inherit your shell environment, configure exec-path-from-shell on macOS.
- Safety for Lisp execution: validation blocks dangerous functions; prefer explicit, minimal expressions.

---
Continuous improvement
This guide is living documentation. If you find gaps or friction points, please update MAYURI.md
