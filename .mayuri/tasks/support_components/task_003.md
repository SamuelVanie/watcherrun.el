# Task ID: SUPPORT-003

**Title:** Implement Utility Functions for Path and Command Handling

## Description
Create essential utility functions for path validation, command parsing, ID generation, and file type detection. These functions will be used throughout the WatcherRun system and need to be robust and well-tested.

**Step-by-step breakdown:**
1. Implement path validation and normalization functions
2. Create command parsing and validation utilities
3. Build unique ID generation system
4. Add file type detection and smart command suggestions
5. Create cleanup helper functions
6. Add comprehensive error handling for all utilities

## Inputs
- Utility function specifications from `component_support_components.md`
- Cross-platform compatibility requirements
- Placeholder expansion system requirements from `component_execution_layer.md`

## Expected Output
- `watcherrun-validate-path` function with comprehensive checks
- `watcherrun-normalize-path` for cross-platform path handling
- `watcherrun-parse-command` for command analysis
- `watcherrun-generate-unique-id` for watcher ID creation
- `watcherrun-detect-file-type` and `watcherrun-suggest-command`
- `watcherrun-expand-placeholders` for command substitution

## Estimated Time
4 hours

## Component
['support_components']

## Level
intermediate

## Best Practices / Notes
- Use `file-exists-p`, `file-readable-p` for path validation - they're cross-platform
- Handle Windows vs Unix path differences with `file-name-as-directory`
- Use `expand-file-name` to resolve `~/` and relative paths
- Regular expressions should be carefully tested for edge cases (`test/` folder)
- Provide helpful error messages that guide users to solutions
- Use `pcase` for pattern matching - it's modern and efficient

**Key validation checks:**
- Path must be a string
- Path must exist (`file-exists-p`)
- Path must be readable (`file-readable-p`)
- For directories, check if they're actually directories

**Placeholder system:**
- `{{file}}`: Full path of changed file
- `{{filename}}`: Just the filename without path  
- `{{dir}}`: Directory containing the file
- `{{ext}}`: File extension
- `{{basename}}`: Filename without extension

**Smart suggestions by file type:**
- `.js` → `npm run build`
- `.py` → `python {{file}}`
- `.el` → `(load-file "{{file}}")`
- `.md` → `pandoc {{file}} -o {{basename}}.html`

**Example implementation:**
```elisp
(defun watcherrun-validate-path (path)
  "Validate that path exists and is accessible."
  (unless (stringp path)
    (error "Path must be a string"))
  (unless (file-exists-p path)
    (error "Path does not exist: %s" path))
  (unless (file-readable-p path)
    (error "Path is not readable: %s" path))
  path)
```
