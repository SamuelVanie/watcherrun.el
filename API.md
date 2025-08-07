# API Documentation

## Public API Functions

### Core Management Functions

#### `watcherrun-add-watcher`
```elisp
(watcherrun-add-watcher PATHS COMMAND COMMAND-TYPE RECURSIVE)
```

Add a new file watcher.

**Parameters:**
- `PATHS` (list): List of file or directory paths to watch
- `COMMAND` (string): Command string to execute when files change
- `COMMAND-TYPE` (symbol): Either `'system` or `'lisp`
- `RECURSIVE` (boolean): Whether to watch directories recursively

**Returns:** Watcher ID string (e.g., "watcher-abc123")

**Example:**
```elisp
(watcherrun-add-watcher '("src/") "npm run build" 'system t)
```

#### `watcherrun-remove-watcher`
```elisp
(watcherrun-remove-watcher WATCHER-ID)
```

Remove an existing watcher by ID.

**Parameters:**
- `WATCHER-ID` (string): The ID of the watcher to remove

**Returns:** t if successful, nil if watcher not found

#### `watcherrun-list-watchers`
```elisp
(watcherrun-list-watchers)
```

Return list of all active watchers with their details.

**Returns:** List of watcher objects with keys: `:id`, `:paths`, `:command`, `:command-type`, `:recursive`

#### `watcherrun-modify-watcher`
```elisp
(watcherrun-modify-watcher WATCHER-ID PATHS COMMAND COMMAND-TYPE RECURSIVE)
```

Modify an existing watcher's configuration.

**Parameters:** Same as `watcherrun-add-watcher` plus:
- `WATCHER-ID` (string): ID of the watcher to modify

**Returns:** t if successful, nil if watcher not found

### Interactive Commands

#### `watcherrun-mode`
```elisp
(watcherrun-mode &optional ARG)
```

Toggle WatcherRun mode globally. When enabled, activates keybindings and menu.

**Parameters:**
- `ARG` (optional): Positive number enables, negative disables, nil toggles

#### `watcherrun-add-watcher-interactive`
```elisp
(watcherrun-add-watcher-interactive)
```

Interactively add a watcher with file completion and prompts.

Prompts for:
- File or directory path (with completion)
- Command to run
- Command type (system or lisp)
- Recursive watching for directories

#### `watcherrun-list-watchers-interactive`
```elisp
(watcherrun-list-watchers-interactive)
```

Display all watchers in a formatted buffer.

#### `watcherrun-delete-watcher-interactive`
```elisp
(watcherrun-delete-watcher-interactive)
```

Interactively select and delete a watcher.

#### `watcherrun-modify-watcher-interactive`
```elisp
(watcherrun-modify-watcher-interactive)
```

Interactively select and modify a watcher's configuration.

#### `watcherrun-show-errors`
```elisp
(watcherrun-show-errors)
```

Display the WatcherRun error buffer.

### Utility Functions

#### `watcherrun-expand-placeholders`
```elisp
(watcherrun-expand-placeholders COMMAND FILE-PATH)
```

Expand template variables in command strings.

**Parameters:**
- `COMMAND` (string): Command string with template variables
- `FILE-PATH` (string): Path to the file that triggered the event

**Template Variables:**
- `{{file}}` - Full path to the changed file
- `{{basename}}` - Filename without extension  
- `{{dirname}}` - Directory containing the file
- `{{extension}}` - File extension (without dot)

**Example:**
```elisp
(watcherrun-expand-placeholders "pandoc {{file}} -o {{basename}}.html" "/docs/readme.md")
;; Returns: "pandoc /docs/readme.md -o readme.html"
```

#### `watcherrun-validate-path`
```elisp
(watcherrun-validate-path PATH)
```

Validate that a path exists and is accessible.

**Parameters:**
- `PATH` (string): File or directory path to validate

**Returns:** t if valid, signals error if invalid

#### `watcherrun-determine-command-type`
```elisp
(watcherrun-determine-command-type COMMAND)
```

Intelligently detect if a command is system or lisp.

**Parameters:**
- `COMMAND` (string): Command string to analyze

**Returns:** `'system` or `'lisp`

**Detection Rules:**
- Commands starting with `(` are detected as lisp
- Commands containing `{{` templates are detected as system  
- Common shell commands are detected as system
- Everything else defaults to system

## Extension Points

### Custom Command Types

To add support for new command types, extend the command type detection:

```elisp
(defun my-watcherrun-detect-custom-type (command)
  "Detect custom command types."
  (cond
   ((string-match-p "^docker" command) 'docker)
   ((string-match-p "^make" command) 'makefile)
   (t nil)))

;; Hook into the detection system
(add-hook 'watcherrun-command-type-detection-hook 'my-watcherrun-detect-custom-type)
```

### Custom Template Variables

Add new template variables by extending the placeholder expansion:

```elisp
(defun my-watcherrun-custom-placeholders (command file-path)
  "Add custom template variables."
  (let* ((git-branch (shell-command-to-string "git branch --show-current"))
         (timestamp (format-time-string "%Y%m%d-%H%M%S")))
    (setq command (string-replace "{{git-branch}}" (string-trim git-branch) command))
    (setq command (string-replace "{{timestamp}}" timestamp command))
    command))

;; Hook into the expansion system
(add-hook 'watcherrun-placeholder-expansion-hook 'my-watcherrun-custom-placeholders)
```

### Custom File Event Filtering

Filter which file events trigger command execution:

```elisp
(defun my-watcherrun-filter-events (event)
  "Custom event filtering logic."
  (let ((file (nth 2 event))
        (action (nth 1 event)))
    (and
     ;; Only process 'changed events, not 'created or 'deleted
     (eq action 'changed)
     ;; Ignore temporary files
     (not (string-match-p "\\.tmp$\\|\\.swp$\\|~$" file))
     ;; Ignore hidden files
     (not (string-match-p "/\\." file)))))

;; Replace the default filter
(setq watcherrun-should-process-event-function 'my-watcherrun-filter-events)
```

### Custom Error Handling

Hook into the error handling system:

```elisp
(defun my-watcherrun-error-handler (error-type message watcher-id)
  "Custom error handling."
  (when (eq error-type 'command-failed)
    ;; Send notification for failed commands
    (notifications-notify :title "WatcherRun Error"
                         :body message
                         :urgency 'critical)))

(add-hook 'watcherrun-error-hook 'my-watcherrun-error-handler)
```

## Data Structures

### Watcher Object

Internal watcher objects contain these fields:

```elisp
(:id "watcher-abc123"
 :paths ("src/" "test/")
 :command "npm run build"
 :command-type system
 :recursive t
 :file-notify-descriptors (descriptor1 descriptor2)
 :stats (:executions 5 :last-run "2024-01-15 10:30:00" :errors 0))
```

### Error Object

Error objects in the error buffer:

```elisp
(:timestamp "2024-01-15 10:30:00"
 :level error
 :type command-failed
 :message "Command 'npm run build' failed with exit code 1"
 :watcher-id "watcher-abc123"
 :context (:file "/src/app.js" :command "npm run build"))
```

## Configuration Variables

### User Options

```elisp
(defcustom watcherrun-debug nil
  "Enable debug output to *Messages* buffer."
  :type 'boolean
  :group 'watcherrun)

(defcustom watcherrun-error-buffer-name "*WatcherRun Errors*"
  "Name of the error buffer."
  :type 'string
  :group 'watcherrun)

(defcustom watcherrun-compilation-buffer-prefix "*compilation-watcher-"
  "Prefix for compilation buffer names."
  :type 'string
  :group 'watcherrun)
```

### Internal Variables

```elisp
watcherrun--watchers
;; Hash table storing all active watchers

watcherrun--next-id
;; Counter for generating unique watcher IDs

watcherrun--mode-enabled
;; Boolean indicating if watcherrun-mode is active
```

## Hooks

```elisp
watcherrun-mode-hook
;; Run when watcherrun-mode is enabled/disabled

watcherrun-watcher-added-hook
;; Run when a new watcher is added

watcherrun-watcher-removed-hook
;; Run when a watcher is removed

watcherrun-command-executed-hook
;; Run after a command is executed

watcherrun-error-hook
;; Run when an error occurs
```

## Best Practices for Extensions

1. **Prefix all custom functions** with your package name to avoid conflicts
2. **Use hooks instead of advice** when possible for better compatibility
3. **Check if WatcherRun is active** before running extensions
4. **Handle errors gracefully** and don't break the main functionality
5. **Document your extensions** for other users

## Example: Complete Custom Extension

```elisp
;;; watcherrun-docker.el --- Docker integration for WatcherRun

(defun watcherrun-docker-build-command (dockerfile-path)
  "Create a Docker build command for DOCKERFILE-PATH."
  (format "docker build -t %s -f {{file}} %s"
          (file-name-base dockerfile-path)
          (file-name-directory dockerfile-path)))

(defun watcherrun-docker-setup-watcher ()
  "Interactively set up a Docker build watcher."
  (interactive)
  (let ((dockerfile (read-file-name "Dockerfile: ")))
    (watcherrun-add-watcher
     (list dockerfile)
     (watcherrun-docker-build-command dockerfile)
     'system
     nil)))

(define-key watcherrun-mode-map (kbd "C-c w D") 'watcherrun-docker-setup-watcher)

(provide 'watcherrun-docker)
```
