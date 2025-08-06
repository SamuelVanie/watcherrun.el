# Task ID: CORE-003

**Title:** Implement Watcher Manager - Core Coordination System

## Description
Create the central watcher manager that coordinates all watcher operations. This is the primary API that the user interface layer will interact with. It manages the watcher registry, handles add/remove/modify operations, and integrates with the file notification system.

**Step-by-step breakdown:**
1. Implement `watcherrun-add-watcher` function with full validation
2. Create `watcherrun-remove-watcher` with cleanup
3. Implement `watcherrun-modify-watcher` for updating commands
4. Add `watcherrun-list-watchers` for UI display
5. Create watcher lookup and registry management functions
6. Add conflict detection (preventing duplicate watchers for same path)

## Inputs
- Watcher manager specification from `component_core_engine.md`
- Watcher data structure from session storage
- Integration requirements with file notification and UI layers
- Registry management and conflict detection requirements

## Expected Output
- `watcherrun-add-watcher` (main API function)
- `watcherrun-remove-watcher` with cleanup
- `watcherrun-modify-watcher` function
- `watcherrun-list-watchers` for display
- `watcherrun-find-watcher-by-id` and similar lookup functions
- Conflict detection and resolution

## Estimated Time
4 hours

## Component
['core_engine']

## Level
intermediate

## Best Practices / Notes
- This is the main API - all user interface components go through these functions
- Always validate inputs before creating watchers
- Generate unique IDs using the utility functions
- Maintain registry consistency - if something fails, rollback changes
- Provide clear error messages for common issues (path doesn't exist, duplicate watcher, etc.)
- Update menu state after watcher operations
- Clean up all resources when removing watchers

**Key API Functions:**
```elisp
(defun watcherrun-add-watcher (paths command command-type recursive)
  "Add new watcher with validation and registration."
  ;; 1. Validate all inputs
  ;; 2. Check for conflicts  
  ;; 3. Generate unique ID
  ;; 4. Create watcher structure
  ;; 5. Set up file notification
  ;; 6. Store in registry
  ;; 7. Return watcher ID
  )

(defun watcherrun-remove-watcher (watcher-id)
  "Remove watcher and clean up all resources."
  ;; 1. Find watcher by ID
  ;; 2. Stop file notification
  ;; 3. Clean up buffers
  ;; 4. Remove from registry
  ;; 5. Update UI state
  )
```

**Registry Management:**
- Use hash table for O(1) watcher lookup by ID
- Maintain separate index by file path for conflict detection
- Automatically generate unique IDs using timestamp + counter
- Provide cleanup functions for session end

**Conflict Detection:**
- Check if path is already being watched
- Allow multiple watchers per path only if commands are different
- Warn user about potential conflicts
- Provide option to update existing watcher instead

**Integration Points:**
- **File Notification**: Set up/tear down file watching
- **Session Storage**: Store/retrieve watcher data
- **Error Handler**: Report validation and operation errors
- **UI Layer**: Provide consistent API for all interfaces

**Dependencies:** Requires CORE-001 (file notification), CORE-002 (command executor), and all SUPPORT tasks.
