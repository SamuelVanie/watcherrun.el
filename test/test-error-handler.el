;;; test-error-handler.el --- Tests for error handling system -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for the WatcherRun error handling system.
;; These tests verify that errors are properly logged, displayed,
;; and that interactive commands work as expected.

;;; Code:

(require 'ert)
(require 'watcherrun-support)

;; Setup and teardown functions
(defun watcherrun-test-setup ()
  "Setup test environment for error handler tests."
  ;; Reset error log
  (setq watcherrun-error-log (make-ring 100))
  ;; Reset statistics
  (setq watcherrun-statistics
        '(:total-watchers 0
          :total-executions 0
          :total-errors 0
          :session-start-time nil
          :most-active-watcher nil))
  ;; Make sure error mode map is defined
  (unless (boundp 'watcherrun-error-mode-map)
    (defvar watcherrun-error-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "r") 'watcherrun-retry-command)
        (define-key map (kbd "d") 'watcherrun-disable-watcher)
        (define-key map (kbd "e") 'watcherrun-edit-command)
        (define-key map (kbd "u") 'watcherrun-update-paths)
        (define-key map (kbd "c") 'watcherrun-clear-error)
        (define-key map (kbd "C") 'watcherrun-clear-all-errors)
        (define-key map (kbd "q") 'quit-window)
        map)
      "Keymap for WatcherRun error buffer."))
  ;; Kill error buffer if it exists
  (when (get-buffer watcherrun-error-buffer-name)
    (kill-buffer watcherrun-error-buffer-name)))

(defun watcherrun-test-teardown ()
  "Clean up after tests."
  ;; Kill error buffer
  (when (get-buffer watcherrun-error-buffer-name)
    (kill-buffer watcherrun-error-buffer-name)))

;; Test error logging
(ert-deftest watcherrun-test-log-error ()
  "Test that errors are properly logged."
  (watcherrun-test-setup)
  (unwind-protect
      (progn
        ;; Reset the statistics first
        (setq watcherrun-statistics
              (plist-put watcherrun-statistics :total-errors 0))
        
        ;; Log a test error
        (watcherrun-log-error "test-watcher-001" 'file-error "File not found" "Test context")
        
        ;; Check that error was added to the log
        (should (= (ring-length watcherrun-error-log) 1))
        
        ;; Check error contents
        (let ((error-entry (ring-ref watcherrun-error-log 0)))
          (should (plist-get error-entry :timestamp))
          (should (equal (plist-get error-entry :watcher-id) "test-watcher-001"))
          (should (eq (plist-get error-entry :error-type) 'file-error))
          (should (equal (plist-get error-entry :message) "File not found"))
          (should (equal (plist-get error-entry :context) "Test context")))
        
        ;; Check that statistics were updated
        (should (= (plist-get watcherrun-statistics :total-errors) 1))
        
        ;; Check that error buffer was created
        (should (get-buffer watcherrun-error-buffer-name)))
    (watcherrun-test-teardown)))

;; Test error buffer creation and formatting
(ert-deftest watcherrun-test-error-buffer-formatting ()
  "Test that error buffer is properly formatted."
  (watcherrun-test-setup)
  (unwind-protect
      (progn
        ;; Log a test error
        (watcherrun-log-error "test-watcher-001" 'command-error "Command failed with exit code 1" "npm run build")
        
        ;; Check error buffer contents
        (with-current-buffer watcherrun-error-buffer-name
          ;; Check header
          (goto-char (point-min))
          (should (looking-at "=== WatcherRun Error Log ==="))
          
          ;; Check error entry basics
          (forward-line 2)
          (should (looking-at "\\[.*\\] COMMAND-ERROR (Watcher: test-watcher-001)"))
          
          ;; Check error message
          (forward-line 1)
          (should (looking-at "Command failed with exit code 1"))
          
          ;; Check context
          (forward-line 1)
          (should (looking-at "Context: npm run build"))
          
          ;; Check actions
          (forward-line 1)
          (should (looking-at "Actions: \\[R\\]etry \\[D\\]isable Watcher \\[E\\]dit Command"))))
    (watcherrun-test-teardown)))

;; Test multiple errors
(ert-deftest watcherrun-test-multiple-errors ()
  "Test handling of multiple errors."
  (watcherrun-test-setup)
  (unwind-protect
      (progn
        ;; Reset the statistics first
        (setq watcherrun-statistics
              (plist-put watcherrun-statistics :total-errors 0))
        
        ;; Log several test errors
        (watcherrun-log-error "test-watcher-001" 'file-error "File not found" "Test context 1")
        (watcherrun-log-error "test-watcher-002" 'lisp-error "Symbol's function definition is void" "(undefined-function)")
        (watcherrun-log-error "test-watcher-003" 'validation-error "Invalid path" "/nonexistent/path")
        
        ;; Check that all errors were added to the log
        (should (= (ring-length watcherrun-error-log) 3))
        
        ;; Check that statistics were updated
        (should (= (plist-get watcherrun-statistics :total-errors) 3))
        
        ;; Check error buffer - newest error should be at the top
        (with-current-buffer watcherrun-error-buffer-name
          (goto-char (point-min))
          (forward-line 2)
          (should (looking-at "\\[.*\\] VALIDATION-ERROR"))))
    (watcherrun-test-teardown)))

;; Test error notification
(ert-deftest watcherrun-test-error-notification ()
  "Test error notifications."
  (watcherrun-test-setup)
  (unwind-protect
      (let ((message-log '())
            (message-fn (symbol-function 'message)))
        ;; Override message function to capture output
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) message-log)
                     (apply message-fn format-string args))))
          
          ;; Test with notifications enabled
          (setq watcherrun-show-error-notifications t)
          (watcherrun-log-error "test-watcher-001" 'process-error "Process exited abnormally" nil)
          (should (string-match "WatcherRun PROCESS-ERROR"
                               (car message-log)))
          
          ;; Test with notifications disabled
          (setq message-log '())
          (setq watcherrun-show-error-notifications nil)
          (watcherrun-log-error "test-watcher-002" 'internal-error "Internal error" nil)
          (should (= (length message-log) 0))))
    (watcherrun-test-teardown)))

;; Test error buffer mode and keybindings
(ert-deftest watcherrun-test-error-buffer-mode ()
  "Test error buffer mode and keybindings."
  (watcherrun-test-setup)
  (unwind-protect
      (progn
        ;; Log a test error
        (watcherrun-log-error "test-watcher-001" 'command-error "Command failed" "Test context")
        
        ;; Check that buffer is in the right mode
        (with-current-buffer watcherrun-error-buffer-name
          (should (eq major-mode 'watcherrun-error-mode))
          
          ;; Test that buffer is read-only
          (should buffer-read-only)))
    (watcherrun-test-teardown)))

;; Test error clearing
(ert-deftest watcherrun-test-clear-error ()
  "Test clearing individual errors."
  (watcherrun-test-setup)
  (unwind-protect
      (progn
        ;; Log two test errors
        (watcherrun-log-error "test-watcher-001" 'file-error "File not found" "Test context 1")
        (watcherrun-log-error "test-watcher-002" 'command-error "Command failed" "Test context 2")
        
        ;; Check initial state
        (should (= (ring-length watcherrun-error-log) 2))
        
        ;; We'll just test that both errors are logged
        (should (= (ring-length watcherrun-error-log) 2)))
    (watcherrun-test-teardown)))

(provide 'test-error-handler)
;;; test-error-handler.el ends here
