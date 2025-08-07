;;; test-utils.el --- Tests for WatcherRun utility functions -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors

;;; Commentary:
;; ERT tests for WatcherRun utility functions, focusing on:
;; - Path validation and normalization
;; - Command parsing and placeholder expansion
;; - File type detection and command suggestions
;; - ID generation

;;; Code:

(require 'ert)
(require 'watcherrun-utils)
(require 'watcherrun-support)

;; Temporary file for path testing
(defvar watcherrun-test-temp-file nil)
(defvar watcherrun-test-temp-dir nil)

;; Setup and teardown for tests
(defun watcherrun-test-setup ()
  "Create temporary file and directory for testing."
  (setq watcherrun-test-temp-file (make-temp-file "watcherrun-test"))
  (setq watcherrun-test-temp-dir (make-temp-file "watcherrun-test-dir" t)))

(defun watcherrun-test-teardown ()
  "Clean up temporary files and directories."
  (when (and watcherrun-test-temp-file (file-exists-p watcherrun-test-temp-file))
    (delete-file watcherrun-test-temp-file))
  (when (and watcherrun-test-temp-dir (file-exists-p watcherrun-test-temp-dir))
    (delete-directory watcherrun-test-temp-dir t)))

;;; Path validation tests

(ert-deftest watcherrun-test-validate-path ()
  "Test path validation function."
  (watcherrun-test-setup)
  (unwind-protect
      (progn
        ;; Valid file path
        (should (string= (watcherrun-validate-path watcherrun-test-temp-file)
                         watcherrun-test-temp-file))
        
        ;; Valid directory path
        (should (string= (watcherrun-validate-path watcherrun-test-temp-dir)
                         watcherrun-test-temp-dir))
        
        ;; Invalid path types
        (should-error (watcherrun-validate-path nil) :type 'error)
        (should-error (watcherrun-validate-path 123) :type 'error)
        
        ;; Non-existent paths
        (should-error (watcherrun-validate-path "/path/does/not/exist") :type 'error)
        
        ;; Make temp file non-readable and test
        (when (file-exists-p watcherrun-test-temp-file)
          (set-file-modes watcherrun-test-temp-file #o000)
          (should-error (watcherrun-validate-path watcherrun-test-temp-file) :type 'error)
          ;; Reset permissions for cleanup
          (set-file-modes watcherrun-test-temp-file #o644)))
    (watcherrun-test-teardown)))

(ert-deftest watcherrun-test-normalize-path ()
  "Test path normalization function."
  ;; Test tilde expansion
  (let ((home (getenv "HOME")))
    (when home
      (should (string= (watcherrun-normalize-path "~/test.txt")
                       (concat home "/test.txt")))))
  
  ;; Test relative path expansion
  (let ((default-directory "/tmp/"))
    (should (string= (watcherrun-normalize-path "./test.txt")
                     "/tmp/test.txt")))
  
  ;; Test Windows-style paths on all platforms for consistency
  (let ((test-path "C:\\Users\\test\\file.txt"))
    (if (eq system-type 'windows-nt)
        (should (string-match-p "/" (watcherrun-normalize-path test-path)))
      ;; On non-Windows, it should preserve the path structure but normalize separators
      (should (string= (watcherrun-normalize-path test-path)
                       (expand-file-name test-path))))))

;;; Command parsing tests

(ert-deftest watcherrun-test-parse-command ()
  "Test command parsing function."
  ;; Test system command
  (let ((cmd-info (watcherrun-parse-command "npm run build")))
    (should (string= (plist-get cmd-info :raw-command) "npm run build"))
    (should (string= (plist-get cmd-info :executable) "npm"))
    (should (equal (plist-get cmd-info :arguments) '("run" "build")))
    (should-not (plist-get cmd-info :has-placeholders))
    (should (eq (plist-get cmd-info :estimated-type) 'system)))
  
  ;; Test Lisp command
  (let ((cmd-info (watcherrun-parse-command "(message \"File changed: %s\" file-var)")))
    (should (string= (plist-get cmd-info :executable) "(message"))
    (should (eq (plist-get cmd-info :estimated-type) 'lisp)))
  
  ;; Test command with placeholders
  (let ((cmd-info (watcherrun-parse-command "echo {{file}} changed")))
    (should (plist-get cmd-info :has-placeholders))
    (should (eq (plist-get cmd-info :estimated-type) 'system))))

(ert-deftest watcherrun-test-expand-placeholders ()
  "Test placeholder expansion in commands."
  (let ((file-path "/home/user/projects/test/file.txt"))
    
    ;; Test file placeholder
    (should (string= (watcherrun-expand-placeholders "echo {{file}}" file-path)
                     "echo /home/user/projects/test/file.txt"))
    
    ;; Test filename placeholder
    (should (string= (watcherrun-expand-placeholders "echo {{filename}}" file-path)
                     "echo file.txt"))
    
    ;; Test directory placeholder
    (should (string= (watcherrun-expand-placeholders "cd {{dir}}" file-path)
                     "cd /home/user/projects/test/"))
    
    ;; Test extension placeholder
    (should (string= (watcherrun-expand-placeholders "Extension: {{ext}}" file-path)
                     "Extension: txt"))
    
    ;; Test basename placeholder
    (should (string= (watcherrun-expand-placeholders "Base: {{basename}}" file-path)
                     "Base: file"))
    
    ;; Test multiple placeholders
    (should (string= (watcherrun-expand-placeholders 
                      "{{basename}}.{{ext}} in {{dir}}" file-path)
                     "file.txt in /home/user/projects/test/"))
    
    ;; Test unknown placeholder
    (should (string= (watcherrun-expand-placeholders "{{unknown}}" file-path)
                     "{{unknown}}"))))

;;; ID generation tests

(ert-deftest watcherrun-test-generate-unique-id ()
  "Test unique ID generation."
  ;; Initialize with empty hash table
  (watcherrun-initialize-session)
  
  ;; First ID should be watcher-000
  (let ((id (watcherrun-generate-unique-id)))
    (should (string= id "watcher-000")))
  
  ;; Add a watcher to the hash table
  (puthash "watcher-000" (make-watcherrun-watcher :id "watcher-000") watcherrun-watchers)
  
  ;; Next ID should be watcher-001
  (let ((id (watcherrun-generate-unique-id)))
    (should (string= id "watcher-001")))
  
  ;; Add non-sequential ID to test gap filling
  (puthash "watcher-005" (make-watcherrun-watcher :id "watcher-005") watcherrun-watchers)
  
  ;; Should still return watcher-001 or next available since the IDs are dynamic
  (let ((id (watcherrun-generate-unique-id)))
    (should (or (string= id "watcher-001") (string= id "watcher-002")))))

;;; File type detection tests

(ert-deftest watcherrun-test-detect-file-type ()
  "Test file type detection based on extension."
  ;; Test JavaScript files
  (should (eq (watcherrun-detect-file-type "app.js") 'javascript))
  (should (eq (watcherrun-detect-file-type "component.jsx") 'javascript))
  (should (eq (watcherrun-detect-file-type "service.ts") 'javascript))
  
  ;; Test Python files
  (should (eq (watcherrun-detect-file-type "script.py") 'python))
  
  ;; Test Emacs Lisp files
  (should (eq (watcherrun-detect-file-type "package.el") 'elisp))
  
  ;; Test Markdown files
  (should (eq (watcherrun-detect-file-type "README.md") 'markdown))
  
  ;; Test C/C++ files
  (should (eq (watcherrun-detect-file-type "main.c") 'c-cpp))
  (should (eq (watcherrun-detect-file-type "class.hpp") 'c-cpp))
  
  ;; Test unknown extension
  (should-not (watcherrun-detect-file-type "data.xyz"))
  
  ;; Test no extension
  (should-not (watcherrun-detect-file-type "README")))

(ert-deftest watcherrun-test-suggest-command ()
  "Test command suggestion based on file type."
  ;; Test suggestions for different file types
  (should (string= (watcherrun-suggest-command "app.js")
                   "npm run build"))
  
  (should (string= (watcherrun-suggest-command "script.py")
                   "python {{file}}"))
  
  (should (string= (watcherrun-suggest-command "package.el")
                   "(load-file \"{{file}}\")"))
  
  (should (string= (watcherrun-suggest-command "README.md")
                   "pandoc {{file}} -o {{basename}}.html"))
  
  ;; Test default suggestion for unknown file type
  (should (string-match-p "echo 'File changed: {{file}}'" 
                          (watcherrun-suggest-command "data.xyz"))))

(provide 'test-utils)
;;; test-utils.el ends here
