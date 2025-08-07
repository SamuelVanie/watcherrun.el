;;; test-core-executor.el --- Tests for command executor functions -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors

;;; Commentary:
;; Test command type detection, validation, and execution routing for WatcherRun.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'watcherrun-support)
(require 'watcherrun-utils)
(require 'watcherrun-core)

;; Test data and mock watcher structure
(defvar watcherrun-test-watcher nil
  "Mock watcher for testing.")

(defun watcherrun-test-setup ()
  "Set up test environment."
  (setq watcherrun-test-watcher
        (make-watcherrun-watcher
         :id "test-watcher-001"
         :paths '("/tmp/test.txt")
         :command "echo 'test'"
         :command-type 'system
         :recursive nil
         :file-descriptor nil
         :last-executed nil
         :execution-count 0
         :status 'active)))

(defun watcherrun-test-teardown ()
  "Clean up test environment."
  (setq watcherrun-test-watcher nil))

;; Command type detection tests
(ert-deftest watcherrun-test-determine-command-type-lisp ()
  "Test Lisp command detection."
  (should (eq 'lisp (watcherrun-determine-command-type "(message \"hello\")")))
  (should (eq 'lisp (watcherrun-determine-command-type "(+ 1 2)")))
  (should (eq 'lisp (watcherrun-determine-command-type "(load-file \"test.el\")"))))

(ert-deftest watcherrun-test-determine-command-type-system ()
  "Test system command detection."
  (should (eq 'system (watcherrun-determine-command-type "make")))
  (should (eq 'system (watcherrun-determine-command-type "npm run build")))
  (should (eq 'system (watcherrun-determine-command-type "compile test.c")))
  (should (eq 'system (watcherrun-determine-command-type "echo hello")))
  (should (eq 'system (watcherrun-determine-command-type "ls -la"))))

;; Command validation tests
(ert-deftest watcherrun-test-validate-system-command-valid ()
  "Test validation of valid system commands."
  (should (watcherrun-validate-system-command "echo hello"))
  (should (watcherrun-validate-system-command "pwd"))
  (should (watcherrun-validate-system-command "cd /tmp")))

(ert-deftest watcherrun-test-validate-lisp-command-valid ()
  "Test validation of valid Lisp commands."
  (should (watcherrun-validate-lisp-command "(message \"hello\")"))
  (should (watcherrun-validate-lisp-command "(+ 1 2)"))
  (should (watcherrun-validate-lisp-command "(load-file \"test.el\")")))

(ert-deftest watcherrun-test-validate-lisp-command-invalid ()
  "Test validation of invalid Lisp commands."
  (should-error (watcherrun-validate-lisp-command "(message \"unclosed"))
  (should-error (watcherrun-validate-lisp-command "(missing parens"))
  (should-error (watcherrun-validate-lisp-command "(((")))

(ert-deftest watcherrun-test-validate-command-dispatcher ()
  "Test command validation dispatcher."
  (should (watcherrun-validate-command "echo hello" 'system))
  (should (watcherrun-validate-command "(message \"hello\")" 'lisp))
  (should-error (watcherrun-validate-command "test" 'unknown)))

;; Accessor functions are already provided by cl-defstruct in watcherrun-support.el

;; Test execution tracking
(ert-deftest watcherrun-test-execute-command-statistics ()
  "Test that execution updates statistics."
  (watcherrun-test-setup)
  (let ((original-count (watcherrun-watcher-execution-count watcherrun-test-watcher))
        (test-file "/tmp/test.txt"))
    
    ;; Mock the execution functions to avoid actual execution
    (cl-letf (((symbol-function 'watcherrun--execute-system-command)
               (lambda (command watcher) 
                 (message "Mock executing: %s" command)))
              ((symbol-function 'async-shell-command)
               (lambda (command buffer-name)
                 (message "Mock shell command: %s in %s" command buffer-name))))
      
      (watcherrun-execute-command watcherrun-test-watcher test-file)
      
      ;; Check that statistics were updated
      (should (watcherrun-watcher-last-executed watcherrun-test-watcher))
      (should (= (1+ original-count) 
                 (watcherrun-watcher-execution-count watcherrun-test-watcher)))))
  
  (watcherrun-test-teardown))

;; Test error handling
(ert-deftest watcherrun-test-execute-command-error-handling ()
  "Test error handling during command execution."
  (watcherrun-test-setup)
  (let ((error-watcher (copy-watcherrun-watcher watcherrun-test-watcher)))
    (setf (watcherrun-watcher-command error-watcher) "(invalid-function)")
    (setf (watcherrun-watcher-command-type error-watcher) 'lisp)
    
    ;; Mock error reporting to capture the error
    (let ((error-reported nil))
      (cl-letf (((symbol-function 'watcherrun-report-error)
                 (lambda (message) 
                   (setq error-reported message))))
        
        (watcherrun-execute-command error-watcher "/tmp/test.txt")
        
        ;; Check that error was reported and status updated
        (should error-reported)
        (should (eq 'error (watcherrun-watcher-status error-watcher))))))
  
  (watcherrun-test-teardown))

;; Test placeholder expansion integration
(ert-deftest watcherrun-test-execute-command-placeholder-expansion ()
  "Test that placeholders are expanded during execution."
  (watcherrun-test-setup)
  (let ((placeholder-watcher (copy-watcherrun-watcher watcherrun-test-watcher))
        (expanded-command nil))
    (setf (watcherrun-watcher-command placeholder-watcher) "echo {{file}}")
    
    ;; Mock execution to capture expanded command
    (cl-letf (((symbol-function 'watcherrun--execute-system-command)
               (lambda (command watcher) 
                 (setq expanded-command command))))
      
      (watcherrun-execute-command placeholder-watcher "/tmp/test.txt")
      
      ;; Check that placeholder was expanded
      (should (string= "echo /tmp/test.txt" expanded-command))))
  
  (watcherrun-test-teardown))

(provide 'test-core-executor)
;;; test-core-executor.el ends here
