# Task ID: UI-003

**Title:** Implement Interactive Commands for Command-Line Watcher Creation

## Description
Create interactive commands that allow users to create watchers by typing file/directory paths directly, without using Dired. This provides a command-line interface for power users and situations where Dired is not convenient.

**Step-by-step breakdown:**
1. Implement `watcherrun-add-watcher-interactive` main function
2. Add tab completion for file path input
3. Create path validation and error handling
4. Integrate the same prompt system as Dired (command type, recursion)
5. Add command history for recently entered paths
6. Provide helpful error messages and recovery options

## Inputs
- Interactive commands specification from `component_user_interface.md`
- File path completion requirements
- Same user workflow as Dired integration
- Command history and usability requirements

## Expected Output
- `watcherrun-add-watcher-interactive` function
- File path completion system
- Path validation and error handling
- Command history management
- Integration with existing prompt functions from Dired task

## Estimated Time
2 hours

## Component
['user_interface']

## Level
junior

## Best Practices / Notes
- Use `read-file-name` for file path input - it provides tab completion automatically
- Reuse the prompt functions from Dired integration (command type, recursion)
- Provide immediate feedback if entered path doesn't exist
- Maintain history of recently entered paths for quick re-use
- Handle both absolute and relative paths correctly
- Follow Emacs interactive command conventions

**Primary Function:**
```elisp
(defun watcherrun-add-watcher-interactive ()
  "Add watcher by prompting for file/directory path."
  (interactive)
  (let ((path (read-file-name "Watch file or directory: " 
                              nil nil t)))
    
    ;; Validate path exists and is accessible
    (condition-case error
        (progn
          (watcherrun-validate-path path)
          
          ;; Get command type and recursion settings
          (let ((command-type (watcherrun--prompt-command-type))
                (recursive (when (file-directory-p path)
                            (watcherrun--prompt-recursion-for-directories (list path)))))
            
            ;; Get command string
            (let ((command (read-string "Enter command to execute: ")))
              
              ;; Create the watcher
              (watcherrun-add-watcher (list path) command command-type recursive))))
      
      (error
       (message "Error: %s" (error-message-string error))))))
```

**Features:**
- **Tab Completion**: Full file path completion using Emacs' built-in completion
- **Path Validation**: Immediate feedback if entered path doesn't exist
- **Same Options**: Identical command type and recursion prompts as Dired mode
- **History**: Maintains history of recently entered paths for quick re-use

**Path Validation Flow:**
1. User enters path with tab completion
2. Validate path exists using `watcherrun-validate-path`
3. If path is invalid, show error and re-prompt
4. If path is valid, continue to command setup

**Integration with Existing Code:**
- Reuse `watcherrun--prompt-command-type` from Dired task
- Reuse `watcherrun--prompt-recursion-for-directories` from Dired task
- Use same `watcherrun-add-watcher` API from watcher manager
- Follow same error handling patterns

**User Experience:**
- Clear prompts that match Dired interface
- Immediate validation feedback
- Option to cancel at any step
- Helpful error messages that guide users to solutions
- Support for both experienced users (who know paths) and beginners (who need completion)

**Error Scenarios:**
- Path doesn't exist → helpful error message, re-prompt
- Path not readable → permission error with suggestions
- Invalid command → validation error with examples
- Watcher already exists → conflict resolution options

**History Management:**
- `read-file-name` automatically provides history
- Consider adding custom history for command strings
- Allow users to recall recent watcher configurations

**Dependencies:** Requires UI-002 (Dired integration) for shared prompt functions, CORE-003 (watcher manager), and SUPPORT-003 (utilities).
