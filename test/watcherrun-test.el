;;; watcherrun-test.el --- Integration tests for WatcherRun -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; Author: WatcherRun Contributors
;; Keywords: convenience, tools, processes

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Integration tests for WatcherRun package to verify all components work together.

;;; Code:

(require 'ert)
(require 'watcherrun)

;;; Test setup and teardown

(defvar watcherrun-test-temp-files nil
  "List of temporary files created during tests.")

(defun watcherrun-test-integration-setup ()
  "Set up test environment for integration tests."
  (setq watcherrun-test-temp-files nil)
  (watcherrun-initialize-session))

(defun watcherrun-test-integration-teardown ()
  "Clean up test environment after integration tests."
  (watcherrun-cleanup-session)
  ;; Clean up temporary files
  (dolist (file watcherrun-test-temp-files)
    (when (and file (file-exists-p file))
      (delete-file file)))
  (setq watcherrun-test-temp-files nil))

;;; Integration tests

(ert-deftest watcherrun-test-package-activation ()
  "Test package activation and deactivation."
  (unwind-protect
      (progn
        ;; Test activation
        (watcherrun-mode 1)
        (should watcherrun-mode)
        (should (hash-table-p watcherrun-watchers))
        
        ;; Test deactivation
        (watcherrun-mode -1)
        (should-not watcherrun-mode))
    
    ;; Cleanup
    (when watcherrun-mode
      (watcherrun-mode -1))))

(ert-deftest watcherrun-test-basic-watcher-creation ()
  "Test basic watcher creation and cleanup."
  (watcherrun-test-integration-setup)
  (let ((test-file (make-temp-file "watcherrun-test")))
    (push test-file watcherrun-test-temp-files)
    
    (unwind-protect
        (progn
          ;; Test watcher creation
          (let ((watcher-id (watcherrun-add-watcher 
                           (list test-file) 
                           "echo test" 
                           'system 
                           nil)))
            (should (stringp watcher-id))
            (should (gethash watcher-id watcherrun-watchers))
            
            ;; Test watcher removal
            (watcherrun-remove-watcher watcher-id)
            (should-not (gethash watcher-id watcherrun-watchers))))
      
      ;; Cleanup
      (watcherrun-test-integration-teardown))))

(ert-deftest watcherrun-test-interactive-watcher-creation ()
  "Test interactive watcher creation with mocked input."
  (watcherrun-test-integration-setup)
  (let ((test-file (make-temp-file "watcherrun-test")))
    (push test-file watcherrun-test-temp-files)
    
    (unwind-protect
        (progn
          ;; Mock user input for interactive command
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _args) test-file))
                    ((symbol-function 'read-string)
                     (lambda (&rest _args) "echo test"))
                    ((symbol-function 'read-char-choice)
                     (lambda (&rest _args) ?s)))
            
            ;; Test interactive watcher creation
            (let ((watcher-count-before (watcherrun-get-watcher-count)))
              (call-interactively 'watcherrun-add-watcher-interactive)
              (should (> (watcherrun-get-watcher-count) watcher-count-before)))))
      
      ;; Cleanup
      (watcherrun-test-integration-teardown))))

(ert-deftest watcherrun-test-menu-commands-integration ()
  "Test menu commands work properly."
  (watcherrun-test-integration-setup)
  (let ((test-file (make-temp-file "watcherrun-test")))
    (push test-file watcherrun-test-temp-files)
    
    (unwind-protect
        (progn
          ;; Add a watcher first
          (let ((watcher-id (watcherrun-add-watcher 
                           (list test-file) 
                           "echo test" 
                           'system 
                           nil)))
            
            ;; Test list watchers menu command
            (should-not (get-buffer "*WatcherRun Watchers*"))
            (watcherrun-list-watchers-menu)
            (should (get-buffer "*WatcherRun Watchers*"))
            
            ;; Test error buffer command
            (watcherrun-show-error-buffer)
            ;; Should not error out even if no errors
            
            ;; Test that watcher exists for menu operations
            (should (> (watcherrun-get-watcher-count) 0))))
      
      ;; Cleanup
      (watcherrun-test-integration-teardown))))

(ert-deftest watcherrun-test-error-handling-integration ()
  "Test error handling across components."
  (watcherrun-test-integration-setup)
  
  (unwind-protect
      (progn
        ;; Test with invalid path
        (should-error 
         (watcherrun-add-watcher 
          (list "/nonexistent/path") 
          "echo test" 
          'system 
          nil))
        
        ;; Test with invalid command
        (let ((test-file (make-temp-file "watcherrun-test")))
          (push test-file watcherrun-test-temp-files)
          (should-error 
           (watcherrun-add-watcher 
            (list test-file) 
            "" 
            'system 
            nil))))
    
    ;; Cleanup
    (watcherrun-test-integration-teardown)))

(ert-deftest watcherrun-test-resource-cleanup ()
  "Test that resources are properly cleaned up."
  (watcherrun-test-integration-setup)
  
  (unwind-protect
      (progn
        (let ((test-file (make-temp-file "watcherrun-test")))
          (push test-file watcherrun-test-temp-files)
          
          ;; Create multiple watchers with different files
          (let ((test-file2 (make-temp-file "watcherrun-test2")))
            (push test-file2 watcherrun-test-temp-files)
            (let ((watcher-ids (list
                              (watcherrun-add-watcher (list test-file) "echo test1" 'system nil)
                              (watcherrun-add-watcher (list test-file2) "echo test2" 'system nil))))
            
            ;; Verify watchers exist
            (should (= (watcherrun-get-watcher-count) 2))
            
              ;; Clean up session
              (watcherrun-cleanup-session)
              
              ;; Verify all watchers are cleaned up
              (should (= (watcherrun-get-watcher-count) 0))))))
    
    ;; Ensure cleanup
    (watcherrun-test-integration-teardown)))

(provide 'watcherrun-test)
;;; watcherrun-test.el ends here
