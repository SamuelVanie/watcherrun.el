;;; test-compilation.el --- Tests for WatcherRun compilation buffer manager

;; Author: WatcherRun Development Team
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (ert "1.0"))

;;; Commentary:
;; This file contains tests for the compilation buffer manager component,
;; which handles multiple concurrent compilation commands by automatically
;; renaming their buffers and tracking them by watcher ID.

;;; Code:

(require 'ert)
(require 'watcherrun-support)
(require 'watcherrun-utils)
(require 'watcherrun-exec)

;; Test utilities
(defun watcherrun-test-create-temp-file (content)
  "Create a temporary file with CONTENT for testing."
  (let ((file (make-temp-file "watcherrun-test-" nil ".txt")))
    (with-temp-file file
      (insert content))
    file))

(defun watcherrun-test-wait-for-buffer (buffer-name &optional timeout)
  "Wait for buffer BUFFER-NAME to exist with optional TIMEOUT seconds."
  (let ((timeout (or timeout 2))
        (start-time (current-time)))
    (while (and (not (get-buffer buffer-name))
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sleep-for 0.1))
    (get-buffer buffer-name)))

(defun watcherrun-test-wait-for-compilation-finish (&optional timeout)
  "Wait for current compilation to finish with optional TIMEOUT seconds."
  (let ((timeout (or timeout 5))
        (start-time (current-time)))
    (while (and (get-buffer-process (compilation-find-buffer))
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sleep-for 0.1))))

;; Setup and teardown for each test
(defun watcherrun-test-compilation-setup ()
  "Set up clean state for compilation tests."
  (setq watcherrun--compilation-buffers (make-hash-table :test 'equal))
  (setq watcherrun--active-processes (make-hash-table :test 'equal))
  ;; Kill any existing compilation buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\*compilation" (buffer-name buffer))
      (kill-buffer buffer))))

(defun watcherrun-test-compilation-teardown ()
  "Clean up after compilation tests."
  ;; Kill compilation buffers forcefully without prompting
  (let ((kill-buffer-query-functions nil))
    (dolist (buffer (buffer-list))
      (when (string-match-p "\\*compilation" (buffer-name buffer))
        (kill-buffer buffer))))
  ;; Clear tracking tables
  (setq watcherrun--compilation-buffers (make-hash-table :test 'equal))
  (setq watcherrun--active-processes (make-hash-table :test 'equal)))

;; Tests for compilation detection
(ert-deftest watcherrun-test-compilation-detection-make ()
  "Test detection of make commands."
  (should (watcherrun-is-compilation-command-p "make"))
  (should (watcherrun-is-compilation-command-p "make all"))
  (should (watcherrun-is-compilation-command-p "make clean install"))
  (should (watcherrun-is-compilation-command-p "make -j4 test")))

(ert-deftest watcherrun-test-compilation-detection-nodejs ()
  "Test detection of Node.js build commands."
  (should (watcherrun-is-compilation-command-p "npm run build"))
  (should (watcherrun-is-compilation-command-p "npm test"))
  (should (watcherrun-is-compilation-command-p "yarn build"))
  (should (watcherrun-is-compilation-command-p "yarn test")))

(ert-deftest watcherrun-test-compilation-detection-rust ()
  "Test detection of Rust cargo commands."
  (should (watcherrun-is-compilation-command-p "cargo build"))
  (should (watcherrun-is-compilation-command-p "cargo test"))
  (should (watcherrun-is-compilation-command-p "cargo run")))

(ert-deftest watcherrun-test-compilation-detection-go ()
  "Test detection of Go build commands."
  (should (watcherrun-is-compilation-command-p "go build main.go"))
  (should (watcherrun-is-compilation-command-p "go test ./..."))
  (should (watcherrun-is-compilation-command-p "go run main.go")))

(ert-deftest watcherrun-test-compilation-detection-negative ()
  "Test that non-compilation commands are not detected."
  (should-not (watcherrun-is-compilation-command-p "cat file.txt"))
  (should-not (watcherrun-is-compilation-command-p "ls -la"))
  (should-not (watcherrun-is-compilation-command-p "python script.py"))
  (should-not (watcherrun-is-compilation-command-p "echo 'hello'"))
  (should-not (watcherrun-is-compilation-command-p "grep pattern file.txt")))

;; Tests for buffer management
(ert-deftest watcherrun-test-compilation-buffer-naming ()
  "Test that compilation buffers are renamed correctly."
  (watcherrun-test-compilation-setup)
  (unwind-protect
      (let ((watcher-id "test-compilation-001"))
        ;; Execute a simple compilation command
        (watcherrun-execute-compilation-command "echo 'test compilation'" watcher-id)
        
        ;; Wait for compilation to start and buffer to be created
        (let ((buffer (watcherrun-test-wait-for-buffer 
                      (format "*compilation-watcher-%s*" watcher-id))))
          (should buffer)
          (should (string= (buffer-name buffer) 
                          (format "*compilation-watcher-%s*" watcher-id)))
          
          ;; Check that buffer is tracked
          (should (gethash watcher-id watcherrun--compilation-buffers))
          (should (eq buffer (gethash watcher-id watcherrun--compilation-buffers)))))
    (watcherrun-test-compilation-teardown)))

(ert-deftest watcherrun-test-multiple-compilation-buffers ()
  "Test that multiple compilation commands create separate buffers."
  (watcherrun-test-compilation-setup)
  (unwind-protect
      (let ((watcher-1 "test-comp-1")
            (watcher-2 "test-comp-2"))
        
        ;; Start first compilation
        (watcherrun-execute-compilation-command "echo 'compilation 1'" watcher-1)
        (let ((buffer-1 (watcherrun-test-wait-for-buffer 
                        (format "*compilation-watcher-%s*" watcher-1))))
          (should buffer-1)
          
          ;; Start second compilation  
          (watcherrun-execute-compilation-command "echo 'compilation 2'" watcher-2)
          (let ((buffer-2 (watcherrun-test-wait-for-buffer 
                          (format "*compilation-watcher-%s*" watcher-2))))
            (should buffer-2)
            
            ;; Both buffers should exist and be different
            (should (not (eq buffer-1 buffer-2)))
            (should (string= (buffer-name buffer-1) 
                            (format "*compilation-watcher-%s*" watcher-1)))
            (should (string= (buffer-name buffer-2) 
                            (format "*compilation-watcher-%s*" watcher-2)))
            
            ;; Both should be tracked
            (should (gethash watcher-1 watcherrun--compilation-buffers))
            (should (gethash watcher-2 watcherrun--compilation-buffers)))))
    (watcherrun-test-compilation-teardown)))

(ert-deftest watcherrun-test-compilation-buffer-cleanup ()
  "Test that compilation buffers are cleaned up properly."
  (watcherrun-test-compilation-setup)
  (unwind-protect
      (let ((watcher-id "test-cleanup"))
        ;; Execute compilation command
        (watcherrun-execute-compilation-command "echo 'test'" watcher-id)
        
        ;; Wait for buffer creation
        (let ((buffer (watcherrun-test-wait-for-buffer 
                      (format "*compilation-watcher-%s*" watcher-id))))
          (should buffer)
          (should (gethash watcher-id watcherrun--compilation-buffers))
          
          ;; Wait for compilation to finish before cleanup
          (watcherrun-test-wait-for-compilation-finish)
          
          ;; Clean up the compilation buffer
          (let ((kill-buffer-query-functions nil))
            (watcherrun-cleanup-compilation-buffer watcher-id))
          
          ;; Buffer should be killed and removed from tracking
          (should-not (buffer-live-p buffer))
          (should-not (gethash watcher-id watcherrun--compilation-buffers))))
    (watcherrun-test-compilation-teardown)))

(ert-deftest watcherrun-test-compilation-buffer-reuse ()
  "Test that new compilation kills old buffer for same watcher."
  (watcherrun-test-compilation-setup)
  (unwind-protect
      (let ((watcher-id "test-reuse"))
        ;; First compilation
        (watcherrun-execute-compilation-command "echo 'first'" watcher-id)
        (let ((first-buffer (watcherrun-test-wait-for-buffer 
                            (format "*compilation-watcher-%s*" watcher-id))))
          (should first-buffer)
          
          ;; Wait for compilation to finish
          (watcherrun-test-wait-for-compilation-finish)
          
          ;; Second compilation should replace the first
          (watcherrun-execute-compilation-command "echo 'second'" watcher-id)
          (let ((second-buffer (watcherrun-test-wait-for-buffer 
                               (format "*compilation-watcher-%s*" watcher-id))))
            (should second-buffer)
            
            ;; First buffer should be killed, second should exist
            (should-not (buffer-live-p first-buffer))
            (should (buffer-live-p second-buffer))
            (should (eq second-buffer (gethash watcher-id watcherrun--compilation-buffers))))))
    (watcherrun-test-compilation-teardown)))

(ert-deftest watcherrun-test-compilation-mode-features ()
  "Test that compilation mode features are preserved."
  (watcherrun-test-compilation-setup)
  (unwind-protect
      (let ((watcher-id "test-mode"))
        ;; Execute compilation that should trigger compilation-mode
        (watcherrun-execute-compilation-command "echo 'Error: test error'" watcher-id)
        
        ;; Wait for buffer creation
        (let ((buffer (watcherrun-test-wait-for-buffer 
                      (format "*compilation-watcher-%s*" watcher-id))))
          (should buffer)
          
          ;; Check that it's in compilation-mode
          (with-current-buffer buffer
            (should (derived-mode-p 'compilation-mode))
            
            ;; Check that compilation-mode variables are set
            (should (boundp 'compilation-error-regexp-alist))
            (should (boundp 'compilation-mode-map)))))
    (watcherrun-test-compilation-teardown)))

(ert-deftest watcherrun-test-compilation-error-handling ()
  "Test error handling in compilation command execution."
  (watcherrun-test-compilation-setup)
  (unwind-protect
      (let ((watcher-id "test-error"))
        ;; Try to execute an invalid command
        (watcherrun-execute-compilation-command "nonexistent-command-xyz" watcher-id)
        
        ;; Should handle the error gracefully
        ;; The function should not throw an error, but may log one
        (should t)) ; Test passes if no error is thrown
    (watcherrun-test-compilation-teardown)))

(provide 'test-compilation)
;;; test-compilation.el ends here
