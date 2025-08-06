# Task ID: UI-001

**Title:** Implement Menu Bar Interface using easy-menu

## Description
Create the traditional Emacs menu bar interface for WatcherRun using `easy-menu.el`. This will be the primary way users interact with the watcher management system. The menu should be intuitive, provide keyboard shortcuts, and dynamically enable/disable items based on current state.

**Step-by-step breakdown:**
1. Define the main WatcherRun menu structure using `easy-menu-define`
2. Implement menu command functions that call watcher manager APIs
3. Add dynamic enabling/disabling logic for menu items
4. Set up keyboard shortcuts for power users
5. Create menu state update functions
6. Add context sensitivity (menu items change based on current state)

## Inputs
- Menu interface specification from `component_user_interface.md`
- Emacs `easy-menu.el` documentation
- Watcher manager API from core engine
- Standard Emacs menu conventions

## Expected Output
- WatcherRun menu definition with all required items
- Menu command functions (list, add, delete, modify watchers)
- Dynamic menu state management
- Keyboard shortcuts configuration
- Integration with watcher manager API

## Estimated Time
3 hours

## Component
['user_interface']

## Level
junior

## Best Practices / Notes
- Use `easy-menu-define` - it's the standard way to create Emacs menus
- Follow Emacs menu conventions: separators, mnemonics, keyboard shortcuts
- Menu items should be self-explanatory and use consistent terminology
- Dynamic enabling: "Delete Watcher" only available when watchers exist
- Use standard Emacs keybinding conventions (Ctrl+c followed by package prefix)
- Provide clear feedback when operations succeed or fail

**Menu Structure:**
```
WatcherRun
├── List All Watchers        [Ctrl+c w l]
├── Add New Watcher         [Ctrl+c w a]
├── ──────────────────────
├── Delete Watcher          [Ctrl+c w d]
├── Modify Watcher Command  [Ctrl+c w m]
├── ──────────────────────
└── Show Error Buffer       [Ctrl+c w e]
```

**Menu Definition Example:**
```elisp
(easy-menu-define watcherrun-menu nil
  "WatcherRun menu for managing file watchers."
  '("WatcherRun"
    ["List All Watchers" watcherrun-list-watchers-menu
     :keys "C-c w l"
     :help "Show all active watchers"]
    ["Add New Watcher" watcherrun-add-watcher-interactive
     :keys "C-c w a"
     :help "Add a new file watcher"]
    "---"
    ["Delete Watcher" watcherrun-delete-watcher-menu
     :keys "C-c w d"
     :enable (> (hash-table-count watcherrun-watchers) 0)
     :help "Delete an existing watcher"]
    ["Modify Watcher" watcherrun-modify-watcher-menu
     :keys "C-c w m"
     :enable (> (hash-table-count watcherrun-watchers) 0)
     :help "Modify watcher command"]
    "---"
    ["Show Error Buffer" watcherrun-show-error-buffer
     :keys "C-c w e"
     :help "Show error log buffer"]))
```

**Menu Command Functions:**
- `watcherrun-list-watchers-menu`: Display watchers in a buffer
- `watcherrun-add-watcher-interactive`: Start interactive watcher creation
- `watcherrun-delete-watcher-menu`: Prompt for watcher to delete
- `watcherrun-modify-watcher-menu`: Prompt for watcher to modify
- `watcherrun-show-error-buffer`: Open error buffer

**Dynamic State Management:**
- Update menu state after watcher operations
- Enable/disable items based on watcher count
- Show/hide items based on context
- Provide feedback for long-running operations

**Dependencies:** Requires CORE-003 (watcher manager) and SUPPORT-004 (error handling).
