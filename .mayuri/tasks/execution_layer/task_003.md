# Task ID: EXEC-003

**Title:** Implement Compilation Buffer Manager for Multiple Concurrent Compilations

## Description
Create the compilation buffer manager that solves the core problem mentioned in the project spec: managing multiple compilation processes by automatically renaming their buffers. This allows multiple watchers to run compilation commands simultaneously without conflicts.

**Step-by-step breakdown:**
1. Implement compilation command detection
2. Create buffer rename hook system  
3. Implement `watcherrun-execute-compilation-command`
4. Add buffer tracking and association management
5. Create cleanup system for compilation buffers
6. Ensure compatibility with Emacs compilation mode features

## Inputs
- Compilation buffer manager spec from `component_execution_layer.md`
- The core problem: "manage multiple compilation buffers by renaming them"
- Emacs compilation mode integration requirements
- Buffer lifecycle management needs

## Expected Output
- `watcherrun-is-compilation-command-p` detection function
- `watcherrun-execute-compilation-command` function
- Buffer rename hook system
- Compilation buffer tracking and cleanup
- Integration with existing compilation mode features

## Estimated Time
3 hours

## Component
['execution_layer']

## Level
intermediate

## Best Practices / Notes
- This solves the specific problem mentioned in the project requirements
- Preserve all compilation mode features (error navigation, keybindings)
- Use hooks to rename buffers immediately after creation
- Track buffer associations for proper cleanup
- Support compilation mode's `next-error` and `previous-error` functions
- Handle case where compilation fails to start

**Compilation Detection:**
```elisp
(defun watcherrun-is-compilation-command-p (command)
  "Determine if command should use compilation mode."
  (or (string-match-p "\\b\\(make\\|compile\\|build\\|test\\)\\b" command)
      (string-match-p "\\b\\(npm\\|yarn\\|cargo\\|go\\)\\s-+\\(run\\|build\\|test\\)" command)
      (string-prefix-p "compile" command)))
```

**Buffer Naming Strategy:**
1. **Detect Compilation**: Check if command will create a `*compilation*` buffer
2. **Generate Unique Name**: Create name like `*compilation-watcher-001*`  
3. **Monitor Buffer Creation**: Watch for new `*compilation*` buffer
4. **Immediate Rename**: Rename buffer as soon as it's created
5. **Track Association**: Link renamed buffer to watcher for cleanup

**Implementation Approach:**
```elisp
(defun watcherrun-execute-compilation-command (command watcher-id)
  "Execute compilation command with automatic buffer renaming."
  (let ((target-buffer-name (format "*compilation-watcher-%s*" watcher-id)))
    
    ;; Set up buffer rename hook
    (add-hook 'compilation-start-hook 
              `(lambda (proc)
                 (when (string= (buffer-name) "*compilation*")
                   (rename-buffer ,target-buffer-name t)
                   (watcherrun-track-compilation-buffer ,target-buffer-name ,watcher-id))))
    
    ;; Execute the compilation
    (compile command)
    
    ;; Clean up hook after brief delay
    (run-with-timer 1.0 nil 
                    (lambda ()
                      (remove-hook 'compilation-start-hook 'watcherrun-buffer-rename-hook)))))
```

**Buffer Lifecycle Management:**
- **Creation**: Automatically rename compilation buffers
- **Tracking**: Maintain association between watchers and their buffers
- **Cleanup**: Remove buffers when watchers are deleted
- **Reuse**: Allow same watcher to create multiple compilation sessions

**Integration Requirements:**
- Must work with `compile` function
- Preserve `compilation-mode` features
- Support `next-error`, `previous-error` navigation
- Maintain compilation buffer history
- Work with external compilation tools

**Dependencies:** Requires EXEC-001 (system command runner), CORE-003 (watcher manager), and SUPPORT-002 (session storage).
