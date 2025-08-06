# Task ID: CORE-002

**Title:** Implement Command Executor and Routing System

## Description
Create the command executor that serves as the dispatcher between file change events and the execution layer. This component determines command types, validates commands, handles placeholder substitution, and routes execution to appropriate handlers.

**Step-by-step breakdown:**
1. Implement command type detection (system vs lisp)
2. Create placeholder substitution system
3. Add command validation for both system and lisp commands
4. Implement the main execution routing function
5. Add execution tracking and statistics
6. Integrate with error handling system

## Inputs
- Command executor specification from `component_core_engine.md`
- Placeholder system requirements from execution layer docs
- Command validation requirements
- Integration points with execution layer

## Expected Output
- `watcherrun-determine-command-type` function
- `watcherrun-expand-placeholders` function  
- `watcherrun-validate-command` function
- `watcherrun-execute-command` main dispatcher
- Command validation utilities
- Execution tracking and statistics

## Estimated Time
3 hours

## Component
['core_engine']

## Level
intermediate

## Best Practices / Notes
- Use pattern matching (`pcase`) for command type detection
- Placeholder replacement should be safe and predictable
- Validate system commands by checking if executable exists in PATH
- For Lisp expressions, use `read` to validate syntax before execution
- Keep execution routing logic simple and clear
- Update watcher statistics after each execution
- Always wrap execution in `condition-case` for error handling

**Command Type Detection Logic:**
```elisp
(defun watcherrun-determine-command-type (command)
  "Intelligently determine if command is system or lisp."
  (cond
   ((string-match-p "^(" command) 'lisp)     ; Starts with parenthesis
   ((string-match-p "compile\\|make\\|npm" command) 'system)
   (t 'system)))  ; Default to system command
```

**Placeholder Substitution:**
- `{{file}}`: Full path of the changed file
- `{{filename}}`: Just the filename without path
- `{{dir}}`: Directory containing the file  
- `{{ext}}`: File extension
- `{{basename}}`: Filename without extension

**Validation Requirements:**
- **System Commands:** Check executable exists, validate syntax, warn about dangerous commands
- **Lisp Expressions:** Parse with `read`, validate function existence, sandbox dangerous operations

**Main Execution Flow:**
1. Update watcher statistics (last-executed, execution-count)
2. Substitute placeholders in command
3. Validate command based on type
4. Route to appropriate executor (system/lisp)
5. Handle any execution errors
6. Update watcher status

**Dependencies:** Requires CORE-001 (file notification), SUPPORT-002 (session storage), SUPPORT-003 (utilities), and SUPPORT-004 (error handling).
