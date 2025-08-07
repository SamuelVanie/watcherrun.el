# Troubleshooting Guide

## Common Issues and Solutions

### Installation and Setup

#### Package Not Found
**Problem:** `package-install` can't find `watcherrun`.

**Solution:** 
1. Ensure MELPA is in your package archives:
   ```elisp
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
   ```
2. Refresh package list: `M-x package-refresh-contents`
3. For manual installation, verify the load path is correct

#### Menu Not Showing
**Problem:** WatcherRun menu doesn't appear in menu bar.

**Solution:**
1. Enable `watcherrun-mode`: `M-x watcherrun-mode`
2. Check if menu bar is enabled: `M-x menu-bar-mode`
3. Restart Emacs if menu still doesn't appear

### Command Execution Issues

#### Command Not Found
**Problem:** "command not found" errors when running system commands.

**Symptoms:**
- Error buffer shows: `Command 'npm' not found`
- Commands work in terminal but not in WatcherRun

**Solution:**
1. **Check exec-path:**
   ```elisp
   (message "exec-path: %s" exec-path)
   ```

2. **Add missing directories:**
   ```elisp
   (add-to-list 'exec-path "/usr/local/bin")
   (add-to-list 'exec-path "/opt/homebrew/bin")  ; macOS Homebrew
   ```

3. **macOS users - use exec-path-from-shell:**
   ```elisp
   (use-package exec-path-from-shell
     :ensure t
     :config
     (exec-path-from-shell-initialize))
   ```

4. **Windows users - check PATH:**
   ```elisp
   (setenv "PATH" (concat "C:\\path\\to\\your\\tools;" (getenv "PATH")))
   ```

#### Permission Denied
**Problem:** Commands fail with permission errors.

**Solution:**
1. Check file permissions: `ls -la /path/to/file`
2. Make script executable: `chmod +x script.sh`
3. For Emacs files, ensure read permissions
4. On Windows, run Emacs as administrator if needed

#### Commands Run Too Slowly
**Problem:** There's a delay between file changes and command execution.

**Solution:**
1. **Check if file watching is working:**
   ```elisp
   (setq watcherrun-debug t)
   ```
   Look for events in `*Messages*` buffer

2. **Reduce file-notify latency** (Linux):
   ```bash
   echo 50 | sudo tee /proc/sys/fs/inotify/max_queued_events
   ```

3. **Use more specific paths** instead of watching large directories

### File Watching Issues

#### Too Many File Events
**Problem:** Commands run constantly or multiple times for single file changes.

**Symptoms:**
- Commands execute repeatedly
- High CPU usage
- Error buffer shows many rapid executions

**Solution:**
1. **Check for editor backup files:**
   ```elisp
   ;; Ignore common backup patterns
   (defun my-watcherrun-filter (event)
     (let ((file (nth 2 event)))
       (not (string-match-p "\\.tmp$\\|\\.swp$\\|~$\\|#.*#$" file))))
   
   (setq watcherrun-should-process-event-function 'my-watcherrun-filter)
   ```

2. **Use more specific file patterns:**
   - Instead of watching `src/`, watch `src/*.js`
   - Use non-recursive watching when possible

3. **Add debouncing delay** (advanced):
   ```elisp
   (defvar my-watcherrun-debounce-timer nil)
   
   (defun my-watcherrun-debounced-execute (original-func &rest args)
     (when my-watcherrun-debounce-timer
       (cancel-timer my-watcherrun-debounce-timer))
     (setq my-watcherrun-debounce-timer
           (run-with-timer 0.5 nil original-func args)))
   
   (advice-add 'watcherrun-execute-command :around 'my-watcherrun-debounced-execute)
   ```

#### File Changes Not Detected
**Problem:** Commands don't run when files change.

**Solution:**
1. **Verify file-notify is working:**
   ```elisp
   (file-notify-supported-p)  ; Should return non-nil
   ```

2. **Check watcher is active:**
   ```elisp
   (watcherrun-list-watchers)
   ```

3. **Test with a simple file:**
   - Create a test file: `touch test.txt`
   - Add watcher: `echo "File changed" > /tmp/watcherrun-test.log`
   - Edit file and check if log is created

4. **Platform-specific issues:**
   - **Linux:** Check inotify limits: `cat /proc/sys/fs/inotify/max_user_watches`
   - **macOS:** Verify file system supports fsevents
   - **Windows:** Ensure file system supports change notifications

#### Watching Network/Remote Files
**Problem:** File watching doesn't work for network drives or TRAMP files.

**Solution:**
1. **Use local copies** for development
2. **Set up sync scripts** instead of direct watching
3. **For TRAMP files**, use local editing with upload commands:
   ```elisp
   (watcherrun-add-watcher 
    '("local-file.py")
    "scp {{file}} user@server:/remote/path/"
    'system nil)
   ```

### Compilation Buffer Issues

#### Multiple Builds Conflict
**Problem:** Multiple watchers overwrite the same compilation buffer.

**Solution:**
This should be automatically handled by WatcherRun's buffer renaming. Each watcher gets its own buffer like `*compilation-watcher-abc123*`.

If you still see conflicts:
1. **Check for manual compilation commands:**
   ```elisp
   ;; Don't use this in watchers:
   (compile "make")  ; Uses default *compilation* buffer
   
   ;; Use this instead:
   "make"  ; WatcherRun handles buffer naming
   ```

2. **Verify buffer names:**
   ```elisp
   (setq watcherrun-debug t)
   ```
   Check `*Messages*` for buffer creation logs

#### Compilation Buffers Accumulate
**Problem:** Too many compilation buffers remain open.

**Solution:**
1. **Auto-close successful compilations:**
   ```elisp
   (defun my-watcherrun-close-successful-compilation ()
     (when (and (eq major-mode 'compilation-mode)
                (string-match-p "compilation-watcher-" (buffer-name))
                (eq (buffer-local-value 'compilation-exit-message-function (current-buffer)) 'successful))
       (quit-window)))
   
   (add-hook 'compilation-finish-functions 'my-watcherrun-close-successful-compilation)
   ```

2. **Limit buffer count:**
   ```elisp
   (defun my-watcherrun-cleanup-old-buffers ()
     (let ((compilation-buffers 
            (seq-filter (lambda (buf)
                         (string-match-p "compilation-watcher-" (buffer-name buf)))
                       (buffer-list))))
       (when (> (length compilation-buffers) 5)
         (dolist (buf (nthcdr 5 compilation-buffers))
           (kill-buffer buf)))))
   
   (add-hook 'watcherrun-command-executed-hook 'my-watcherrun-cleanup-old-buffers)
   ```

### Performance Issues

#### High CPU Usage
**Problem:** Emacs uses high CPU when WatcherRun is active.

**Solution:**
1. **Check what's being watched:**
   ```elisp
   (watcherrun-list-watchers)
   ```

2. **Avoid watching large directories:**
   - Don't watch `/`, `/home`, or other huge trees
   - Exclude `node_modules/`, `.git/`, build directories

3. **Use specific file patterns:**
   ```elisp
   ;; Instead of this:
   (watcherrun-add-watcher '("/large/project/") "make" 'system t)
   
   ;; Use this:
   (watcherrun-add-watcher '("/large/project/src/*.c") "make" 'system nil)
   ```

4. **Monitor file-notify events:**
   ```elisp
   (setq file-notify-debug t)
   ```

#### Memory Usage Grows
**Problem:** Emacs memory usage increases over time.

**Solution:**
1. **Check for event accumulation:**
   ```elisp
   ;; Check error buffer size
   (with-current-buffer "*WatcherRun Errors*"
     (message "Error buffer size: %d" (buffer-size)))
   ```

2. **Limit error buffer size:**
   ```elisp
   (defcustom watcherrun-max-error-buffer-size 100000
     "Maximum size of error buffer in characters."
     :type 'integer)
   
   (defun watcherrun-trim-error-buffer ()
     (with-current-buffer (get-buffer-create "*WatcherRun Errors*")
       (when (> (buffer-size) watcherrun-max-error-buffer-size)
         (goto-char (point-max))
         (forward-line (- (/ watcherrun-max-error-buffer-size 2)))
         (delete-region (point-min) (point)))))
   ```

### Platform-Specific Issues

#### Linux (inotify)

**inotify limit reached:**
```bash
# Check current limit
cat /proc/sys/fs/inotify/max_user_watches

# Increase limit temporarily
echo 524288 | sudo tee /proc/sys/fs/inotify/max_user_watches

# Increase permanently
echo 'fs.inotify.max_user_watches=524288' | sudo tee -a /etc/sysctl.conf
```

**File system doesn't support inotify:**
- NFS, some network file systems don't support inotify
- Use local copies or alternative sync methods

#### macOS (fsevents)

**File watching stops working:**
```elisp
;; Restart file watching
(watcherrun-mode -1)
(watcherrun-mode 1)
```

**Spotlight indexing conflicts:**
- Add development directories to Spotlight privacy list
- System Preferences → Spotlight → Privacy

#### Windows (w32notify)

**Long path issues:**
```elisp
;; Use shorter paths or enable long path support
(setq w32-enable-long-file-names t)
```

**Antivirus interference:**
- Add Emacs and project directories to antivirus exclusions
- Some antivirus software blocks file watching

### Debug Mode

Enable comprehensive debugging:

```elisp
(setq watcherrun-debug t
      file-notify-debug t)

;; Check debug output
(with-current-buffer "*Messages*"
  (goto-char (point-max))
  (search-backward "watcherrun"))
```

### Getting Help

1. **Check error buffer:** `C-c w e` or `M-x watcherrun-show-errors`
2. **Enable debug mode** and check `*Messages*` buffer
3. **Test with minimal setup:**
   ```elisp
   emacs -Q
   (add-to-list 'load-path "/path/to/watcherrun")
   (require 'watcherrun)
   (watcherrun-mode 1)
   ```
4. **Create minimal reproduction case**
5. **Check GitHub issues** or create a new one

### Emergency Recovery

If WatcherRun causes problems:

```elisp
;; Disable immediately
(watcherrun-mode -1)

;; Clear all watchers
(setq watcherrun--watchers (make-hash-table :test 'equal))

;; Stop all file watching
(dolist (descriptor file-notify-descriptors)
  (file-notify-rm-watch descriptor))

;; Reset state
(setq watcherrun--next-id 1)
```
