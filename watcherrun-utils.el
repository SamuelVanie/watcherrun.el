;;; watcherrun-utils.el --- Utility functions for WatcherRun -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors

;;; Commentary:
;; This file contains utility functions for path validation, command parsing,
;; ID generation, file type detection, and placeholder expansion for WatcherRun.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Path validation and normalization

(defun watcherrun-validate-path (path)
  "Validate that PATH exists and is accessible.
Returns PATH if valid, otherwise signals an error."
  (unless (stringp path)
    (error "Path must be a string"))
  (unless (file-exists-p path)
    (error "Path does not exist: %s" path))
  (unless (file-readable-p path)
    (error "Path is not readable: %s" path))
  (when (file-directory-p path)
    (unless (file-accessible-directory-p path)
      (error "Directory is not accessible: %s" path)))
  path)

(defun watcherrun-normalize-path (path)
  "Normalize PATH for consistent handling across platforms.
Expands tildes, converts to absolute path, and ensures proper directory separators."
  (let ((expanded (expand-file-name path)))
    ;; Convert backslashes to forward slashes on Windows
    (if (eq system-type 'windows-nt)
        (replace-regexp-in-string "\\\\" "/" expanded)
      expanded)))

;;; Command parsing and validation

(defun watcherrun-parse-command (command)
  "Parse COMMAND string and extract components.
Returns a plist with command information."
  (let ((trimmed (string-trim command)))
    (list
     :raw-command trimmed
     :executable (car (split-string trimmed))
     :arguments (cdr (split-string trimmed))
     :has-placeholders (string-match-p "{{.*}}" trimmed)
     :estimated-type (if (string-prefix-p "(" trimmed) 'lisp 'system))))

(defun watcherrun-expand-placeholders (command file-path)
  "Replace placeholders in COMMAND with actual values from FILE-PATH.
Supported placeholders:
- {{file}}: Full path of changed file
- {{filename}}: Just the filename without path
- {{dir}}: Directory containing the file
- {{ext}}: File extension
- {{basename}}: Filename without extension"
  (let* ((file-name (file-name-nondirectory file-path))
         (dir-name (file-name-directory file-path))
         (extension (file-name-extension file-path))
         (basename (file-name-sans-extension file-name)))
    
    (replace-regexp-in-string
     "{{\\([^}]+\\)}}"
     (lambda (match)
       (pcase (match-string 1 match)
         ("file" file-path)
         ("filename" file-name)
         ("dir" dir-name)
         ("ext" (or extension ""))
         ("basename" basename)
         (_ match)))  ; Return unchanged if unknown placeholder
     command)))

;;; ID generation

(defun watcherrun-generate-unique-id ()
  "Generate unique ID for new watcher.
IDs are in the format 'watcher-NNN' where NNN is a sequence number."
  (let ((base-id (format "watcher-%03d" (hash-table-count watcherrun-watchers)))
        (counter 1))
    ;; If the ID already exists, increment counter until we find an available ID
    (while (gethash base-id watcherrun-watchers)
      (setq base-id (format "watcher-%03d" counter))
      (setq counter (1+ counter)))
    base-id))

;;; File type detection and smart command suggestions

(defun watcherrun-detect-file-type (file-path)
  "Detect file type and language based on FILE-PATH extension.
Returns a symbol representing the detected file type."
  (let ((ext (file-name-extension file-path)))
    (when ext
      (pcase (downcase ext)
        ;; JavaScript/TypeScript
        ((or "js" "jsx" "ts" "tsx") 'javascript)
        ;; Python
        ("py" 'python)
        ;; Emacs Lisp
        ("el" 'elisp)
        ;; Markdown
        ((or "md" "markdown") 'markdown)
        ;; C/C++
        ((or "c" "cpp" "h" "hpp") 'c-cpp)
        ;; HTML/CSS
        ((or "html" "htm" "css") 'web)
        ;; Java
        ("java" 'java)
        ;; Shell scripts
        ((or "sh" "bash" "zsh") 'shell)
        ;; Ruby
        ("rb" 'ruby)
        ;; Go
        ("go" 'golang)
        ;; Rust
        ("rs" 'rust)
        ;; Other types return nil
        (_ nil)))))

(defun watcherrun-suggest-command (file-path)
  "Suggest appropriate command based on FILE-PATH type.
Returns a string with a suitable command for the file type."
  (let ((file-type (watcherrun-detect-file-type file-path)))
    (pcase file-type
      ('javascript "npm run build")
      ('python "python {{file}}")
      ('elisp "(load-file \"{{file}}\")")
      ('markdown "pandoc {{file}} -o {{basename}}.html")
      ('c-cpp "make")
      ('web "npm run start")
      ('java "javac {{file}}")
      ('shell "sh {{file}}")
      ('ruby "ruby {{file}}")
      ('golang "go run {{file}}")
      ('rust "cargo build")
      ;; Default for unknown file types
      (_ "echo 'File changed: {{file}}'"))))

;;; Cleanup helper functions

(defun watcherrun-cleanup-file-watchers ()
  "Clean up any stale file notification watchers.
Removes any file-notify descriptors for non-existent files."
  (maphash (lambda (id watcher)
            (let ((paths (watcherrun-watcher-paths watcher))
                  (descriptor (watcherrun-watcher-file-descriptor watcher)))
              ;; If descriptor exists but path doesn't, remove the watcher
              (when (and descriptor
                         (cl-some (lambda (path) (not (file-exists-p path))) paths))
                (ignore-errors
                  (file-notify-rm-watch descriptor))
                (setf (watcherrun-watcher-file-descriptor watcher) nil)
                (setf (watcherrun-watcher-status watcher) 'error))))
          watcherrun-watchers))

(provide 'watcherrun-utils)
;;; watcherrun-utils.el ends here
