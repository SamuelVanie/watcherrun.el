# Task ID: UI-002

**Title:** Implement Dired Integration for File Selection

## Description
Create Dired integration that allows users to mark files and directories in Dired and create watchers directly from them. This provides a visual, intuitive way to select multiple files/directories for watching without typing paths manually.

**Step-by-step breakdown:**
1. Implement `watcherrun-dired-add-watcher` function
2. Create user prompt system for command type and recursion settings
3. Add input validation and path processing for marked files
4. Integrate with the watcher manager API
5. Add keybinding for the Dired integration
6. Create user experience enhancements (feedback, error handling)

## Inputs
- Dired integration specification from `component_user_interface.md`
- Dired API documentation (`dired-get-marked-files`)
- User workflow requirements (mark files, prompt for settings, create watchers)
- Integration points with watcher manager

## Expected Output
- `watcherrun-dired-add-watcher` function
- User prompt functions for command type and recursion
- Dired keybinding configuration
- Input validation and error handling
- Integration with watcher manager API

## Estimated Time
3 hours

## Component
['user_interface']

## Level
junior

## Best Practices / Notes
- Use `dired-get-marked-files` to get selected files/directories
- Provide clear, single-character prompts for speed: "(s)ystem or (l)isp?"
- Show visual feedback about which files/directories will be watched
- Handle both files and directories appropriately
- Validate all paths before creating watchers
- Provide helpful error messages for common issues

**Workflow:**
1. **File Selection**: User marks files/directories using `m` in Dired
2. **Watcher Creation**: User invokes `watcherrun-dired-add-watcher` (bound to a key)
3. **Command Type Choice**: System prompts: "Execute (s)ystem command or (l)isp expression?"
4. **Recursion Setting**: For directories: "Watch (i)mmediate files only or (r)ecursive?"
5. **Command Input**: User enters the command/expression to execute

**Implementation:**
```elisp
(defun watcherrun-dired-add-watcher ()
  "Add watcher for marked files in Dired."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (null marked-files)
        (message "No files marked. Use 'm' to mark files first.")
      
      ;; Get command type
      (let ((command-type (watcherrun--prompt-command-type))
            (recursive (watcherrun--prompt-recursion-for-directories marked-files)))
        
        ;; Get command string
        (let ((command (read-string "Enter command to execute: ")))
          
          ;; Validate and create watchers
          (watcherrun--create-watchers-for-paths marked-files command command-type recursive))))))
```

**User Prompt Functions:**
```elisp
(defun watcherrun--prompt-command-type ()
  "Prompt user for command type with single character input."
  (let ((choice (read-char-choice 
                 "Execute (s)ystem command or (l)isp expression? " 
                 '(?s ?l))))
    (pcase choice
      (?s 'system)
      (?l 'lisp))))

(defun watcherrun--prompt-recursion-for-directories (paths)
  "Prompt for recursion setting if directories are included."
  (when (seq-some #'file-directory-p paths)
    (let ((choice (read-char-choice
                   "Watch directories (i)mmediately or (r)ecursively? "
                   '(?i ?r))))
      (pcase choice
        (?i nil)
        (?r t)))))
```

**User Experience Enhancements:**
- Clear prompts with single-character choices for speed
- Automatic detection of file vs directory to show relevant options
- Visual feedback showing which files/directories will be watched
- Graceful error handling with helpful messages
- Option to cancel at any step in the process

**Keybinding:**
- Bind to a logical key in Dired mode (e.g., `W` for "Watch")
- Document the keybinding in help text
- Follow Dired keybinding conventions

**Error Handling:**
- Check if files/directories still exist
- Validate permissions (readable, etc.)
- Handle case where no files are marked
- Provide clear error messages for each failure type

**Dependencies:** Requires CORE-003 (watcher manager), SUPPORT-003 (utilities), and UI-001 (menu interface).
