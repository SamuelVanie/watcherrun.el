# Task ID: CORE-001

**Title:** Implement File Notification Handler

## Description
Create the file notification handler that wraps Emacs' native `file-notify` API. This component will monitor file changes and trigger command execution. It needs to handle both individual files and directories (with optional recursion) while filtering out irrelevant events.

**Step-by-step breakdown:**
1. Implement `watcherrun-setup-file-watcher` to create file notifications
2. Create the main event handler `watcherrun-handle-change-event`
3. Add event filtering logic to ignore temp files and debounce rapid changes
4. Implement cleanup functions for removing file watchers
5. Add error handling for deleted files/directories
6. Create watcher restart capability when directories are recreated

## Inputs
- File notification requirements from `component_core_engine.md`
- Emacs `file-notify.el` API documentation
- Event filtering specifications
- Cross-platform compatibility requirements

## Expected Output
- `watcherrun-setup-file-watcher` function
- `watcherrun-handle-change-event` callback function
- `watcherrun-cleanup-file-watcher` function
- Event filtering utilities
- Error handling for file system changes

## Estimated Time
4 hours

## Component
['core_engine']

## Level
intermediate

## Best Practices / Notes
- Use `file-notify-add-watch` with appropriate flags for each platform
- Handle different event types: 'created, 'deleted, 'changed, 'renamed
- Implement debouncing to avoid multiple triggers for rapid file changes (100ms window)
- Filter out temporary files (ending in ~, .tmp, .swp, etc.)
- Filter out hidden files (starting with .) unless explicitly requested
- Store file descriptors in watcher structure for cleanup
- Use `condition-case` to handle file system errors gracefully

**Key functions to implement:**
```elisp
(defun watcherrun-setup-file-watcher (paths recursive callback)
  "Set up file watching for given paths."
  ;; Use file-notify-add-watch for each path
  ;; Handle both files and directories
  ;; Return file descriptor for cleanup
  )

(defun watcherrun-handle-change-event (event)
  "Handle file change events from file-notify."
  ;; Event structure: (descriptor action file [file1])
  ;; Filter events and call appropriate handlers
  )
```

**Event filtering logic:**
- Only respond to: 'changed, 'created, 'renamed
- Ignore files ending in: ~, .tmp, .swp, .bak
- Ignore hidden files unless explicitly watching them
- Debounce: ignore events within 100ms of previous event for same file

**Error scenarios to handle:**
- Watched file gets deleted
- Watched directory gets deleted  
- Permission changes
- File system unmounting
- Network drives disconnecting

**Dependencies:** This task requires completion of SUPPORT-001 (basic package structure) and SUPPORT-002 (session storage).
