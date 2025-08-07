# WatcherRun.el

Watch files and directories and run commands when they change.

## Features

- **Native Emacs Integration**: Uses Emacs' built-in file-notify APIs with no external dependencies
- **Flexible Command Execution**: Run system commands or Emacs Lisp expressions
- **Multiple Watchers**: Manage several concurrent file watchers with dedicated compilation buffers
- **Smart Menu Integration**: Menu bar interface with keyboard shortcuts (`C-c w [l|a|d|m|e]`)
- **Dired Integration**: Add watchers directly from Dired with `W` key
- **Cross-Platform**: Works on Linux (inotify), macOS (fsevents), and Windows (w32notify)
- **Template Variables**: Use `{{file}}`, `{{basename}}`, `{{dirname}}` in commands
- **Error Handling**: Dedicated error buffer with structured logging
- **Session Storage**: No configuration files - clean and simple

## Installation

### From MELPA (when available)

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-install 'watcherrun)
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/username/watcherrun.el.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/watcherrun.el")
   (require 'watcherrun)
   ```

### Using use-package

```elisp
(use-package watcherrun
  :ensure t
  :demand t
  :config
  (watcherrun-mode 1))
```

## Quick Start

1. **Enable WatcherRun**: `M-x watcherrun-mode`
2. **Add a watcher**: `C-c w a` or use the WatcherRun menu
3. **Edit your file** and watch the command execute automatically

## Usage Examples

### Web Development
Watch source files and rebuild automatically:
```
Path: src/
Command: npm run build
Type: System
Recursive: Yes
```

### Python Testing
Run tests when test files change:
```
Path: test_*.py
Command: python {{file}}
Type: System
Recursive: No
```

### Documentation Generation
Convert Markdown to HTML:
```
Path: docs/
Command: pandoc {{file}} -o {{dirname}}/{{basename}}.html
Type: System
Recursive: Yes
```

### Emacs Lisp Development
Reload Lisp files automatically:
```
Path: *.el
Command: (load-file "{{file}}")
Type: Lisp
Recursive: No
```

## Template Variables

Use these variables in your commands:

- `{{file}}` - Full path to the changed file
- `{{basename}}` - Filename without extension
- `{{dirname}}` - Directory containing the file
- `{{extension}}` - File extension (without dot)

## Keyboard Shortcuts

When `watcherrun-mode` is enabled:

- `C-c w l` - List all watchers
- `C-c w a` - Add new watcher
- `C-c w d` - Delete watcher
- `C-c w m` - Modify watcher
- `C-c w e` - Show error buffer

In Dired mode:
- `W` - Add watcher for marked files/directories

## Menu Interface

Access WatcherRun through the menu bar:
- **WatcherRun → List Watchers** - View all active watchers
- **WatcherRun → Add Watcher** - Create new watcher
- **WatcherRun → Delete Watcher** - Remove existing watcher
- **WatcherRun → Modify Watcher** - Edit watcher settings
- **WatcherRun → Show Errors** - View error buffer

## Configuration Examples

### Web Development Setup
```elisp
(defun my-web-dev-watchers ()
  "Set up watchers for web development."
  (interactive)
  (watcherrun-add-watcher '("src/js/") "npm run build:js" 'system t)
  (watcherrun-add-watcher '("src/css/") "npm run build:css" 'system t)
  (watcherrun-add-watcher '("test/") "npm test" 'system t))
```

### Emacs Package Development
```elisp
(defun my-elisp-dev-watchers ()
  "Set up watchers for Emacs Lisp development."
  (interactive)
  (watcherrun-add-watcher '("*.el") "(byte-compile-file \"{{file}}\")" 'lisp nil)
  (watcherrun-add-watcher '("test/") "emacs -batch -l ert -l {{file}} -f ert-run-tests-batch-and-exit" 'system t))
```

### Documentation Workflow
```elisp
(defun my-docs-watchers ()
  "Set up documentation watchers."
  (interactive)
  (watcherrun-add-watcher '("docs/") "make docs" 'system t)
  (watcherrun-add-watcher '("README.md") "pandoc {{file}} -o README.html" 'system nil))
```

## API Documentation

### Public Functions

#### Core Functions

```elisp
(watcherrun-add-watcher PATHS COMMAND COMMAND-TYPE RECURSIVE)
```
Add a new file watcher.
- `PATHS`: List of file or directory paths to watch
- `COMMAND`: Command string to execute when files change
- `COMMAND-TYPE`: Either `'system` or `'lisp`
- `RECURSIVE`: Boolean for recursive directory watching
- Returns: Watcher ID string

```elisp
(watcherrun-remove-watcher WATCHER-ID)
```
Remove an existing watcher by ID.

```elisp
(watcherrun-list-watchers)
```
Return list of all active watchers with their details.

```elisp
(watcherrun-modify-watcher WATCHER-ID PATHS COMMAND COMMAND-TYPE RECURSIVE)
```
Modify an existing watcher's configuration.

#### Interactive Commands

```elisp
(watcherrun-mode &optional ARG)
```
Toggle WatcherRun mode globally. When enabled, activates keybindings and menu.

```elisp
(watcherrun-add-watcher-interactive)
```
Interactively add a watcher with file completion and prompts.

```elisp
(watcherrun-list-watchers-interactive)
```
Display all watchers in a formatted buffer.

### Extension Points

To extend WatcherRun functionality, you can:

1. **Custom Command Types**: Extend `watcherrun-determine-command-type` for new command patterns
2. **Template Variables**: Add new variables to `watcherrun-expand-placeholders`
3. **File Filters**: Customize `watcherrun-should-process-event` for event filtering
4. **Error Handling**: Hook into `watcherrun-handle-error` for custom error processing

## Troubleshooting

### Common Issues

#### Commands Not Found
**Problem**: "command not found" errors when running system commands.

**Solution**: Ensure your `exec-path` includes the command's directory:
```elisp
(add-to-list 'exec-path "/usr/local/bin")
```

On macOS, consider using `exec-path-from-shell`:
```elisp
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
```

#### Too Many File Events
**Problem**: Commands run too frequently on large directories.

**Solution**: Use more specific paths or file patterns instead of watching entire large directories.

#### Compilation Buffer Conflicts
**Problem**: Multiple builds overwriting the same compilation buffer.

**Solution**: WatcherRun automatically renames compilation buffers per watcher. Each watcher gets its own buffer like `*compilation-watcher-abc123*`.

#### Permission Errors
**Problem**: Cannot watch certain directories.

**Solution**: Ensure Emacs has read permissions for the target directories. Some system directories may be restricted.

#### Performance Issues
**Problem**: Emacs becomes slow when watching large directories.

**Solution**: 
- Use more specific file patterns
- Avoid watching temporary or build directories
- Consider recursive=nil for large directory trees

### Platform-Specific Notes

#### Linux (inotify)
- Works well for most use cases
- May hit inotify limits with very large directory trees
- Check `/proc/sys/fs/inotify/max_user_watches` if needed

#### macOS (fsevents)
- Generally reliable
- May have slight delays compared to other platforms
- Works well with symlinks

#### Windows (w32notify)
- Basic functionality works
- May have more limitations with certain file operations
- Test thoroughly in your Windows environment

### Debug Mode

Enable debug output to troubleshoot issues:
```elisp
(setq watcherrun-debug t)
```

Check the `*WatcherRun Errors*` buffer for detailed error information.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass:
   ```bash
   emacs -Q --batch -l ert -l watcherrun-support.el -l watcherrun-utils.el -l test/test-support.el -l test/test-utils.el -f ert-run-tests-batch-and-exit
   ```
5. Submit a pull request

## License

GPL-3.0 or later. See [LICENSE](LICENSE) for details.

## Requirements

- Emacs 27.1 or later
- Native file-notify support (built into modern Emacs)
- No external package dependencies

---

*WatcherRun.el - Making file watching native to Emacs*
