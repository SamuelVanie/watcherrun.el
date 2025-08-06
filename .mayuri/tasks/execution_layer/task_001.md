# Task ID: EXEC-001

**Title:** Implement System Command Runner with Async Process Management

## Description
Create the system command runner that executes system commands asynchronously using Emacs' process management. This component must keep Emacs responsive while running potentially long-running commands and provide proper output handling.

**Step-by-step breakdown:**
1. Implement `watcherrun-execute-system-command` for async execution
2. Create process buffer management system
3. Set up process sentinels for completion/error handling
4. Add output buffer formatting and management
5. Implement process cleanup on watcher deletion
6. Add support for different command types (regular vs compilation)

## Inputs
- System command runner specification from `component_execution_layer.md`
- Async process requirements
- Buffer management conventions
- Integration with compilation buffer manager

## Expected Output
- `watcherrun-execute-system-command` function
- Process buffer creation and management
- Process sentinel functions for monitoring
- Output formatting and display logic
- Process cleanup utilities

## Estimated Time
4 hours

## Component
['execution_layer']

## Level
intermediate

## Best Practices / Notes
- Use `start-process` for true asynchronous execution - never `shell-command`
- Create dedicated output buffers for each watcher to avoid conflicts
- Implement process sentinels to handle completion and errors properly
- Use `process-live-p` to check process status before operations
- Clean up processes when watchers are deleted to prevent resource leaks
- Handle both successful execution and error cases gracefully

**Buffer Naming Convention:**
- Regular commands: `*watcherrun-output-<watcher-id>*`
- Error output: `*watcherrun-errors*`
- Compilation commands will be handled separately

**Process Management:**
```elisp
(defun watcherrun-execute-system-command (command file-path watcher-id)
  "Execute system command asynchronously with proper error handling."
  (let* ((expanded-command (watcherrun-expand-placeholders command file-path))
         (buffer-name (format "*watcherrun-output-%s*" watcher-id))
         (process-name (format "watcherrun-%s" watcher-id)))
    
    ;; Create or reuse output buffer
    ;; Start async process
    ;; Set up process sentinel
    ;; Return process object
    ))
```

**Process Sentinel:**
- Handle process completion (success/failure)
- Update watcher statistics
- Log errors to error handling system
- Clean up resources as needed

**Output Buffer Features:**
- Automatic scrolling to show latest output
- Timestamps for each execution
- Color coding for different types of output
- Option to clear buffer on new execution

**Error Handling:**
- Capture process exit codes
- Handle process signals (SIGTERM, SIGKILL, etc.)
- Report process errors to error handler
- Update watcher status appropriately

**Dependencies:** Requires CORE-002 (command executor), SUPPORT-002 (session storage), SUPPORT-003 (utilities), and SUPPORT-004 (error handling).
