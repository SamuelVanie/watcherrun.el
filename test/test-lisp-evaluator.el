;;; test-lisp-evaluator.el --- Tests for Lisp expression evaluator -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;;; Commentary:
;; Tests for the Lisp expression evaluator component

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the execution layer
(load-file (expand-file-name "../watcherrun-exec.el" 
                           (file-name-directory (or load-file-name buffer-file-name))))

;; Test data
(defvar test-file-path "/tmp/test-file.txt")
(defvar test-watcher-id "test-watcher-001")

;; Test expression validation
(ert-deftest watcherrun-test-validate-safe-expressions ()
  "Test validation of safe Lisp expressions."
  ;; Safe simple expressions
  (should (watcherrun-validate-lisp-expression '(+ 1 2)))
  (should (watcherrun-validate-lisp-expression '(message "Hello")))
  (should (watcherrun-validate-lisp-expression '(length '(1 2 3))))
  
  ;; Safe atoms
  (should (watcherrun-validate-lisp-expression 'file-var))
  (should (watcherrun-validate-lisp-expression 42))
  (should (watcherrun-validate-lisp-expression "string"))
  
  ;; Safe nested expressions
  (should (watcherrun-validate-lisp-expression '(when (string-suffix-p ".el" file-var)
                                                   (message "Elisp file: %s" file-var)))))

(ert-deftest watcherrun-test-validate-forbidden-expressions ()
  "Test validation correctly rejects forbidden expressions."
  ;; Direct forbidden functions
  (should-not (watcherrun-validate-lisp-expression '(delete-file "/tmp/test")))
  (should-not (watcherrun-validate-lisp-expression '(shell-command "rm -rf /")))
  (should-not (watcherrun-validate-lisp-expression '(kill-emacs)))
  
  ;; Forbidden functions in nested expressions
  (should-not (watcherrun-validate-lisp-expression '(when t (delete-file "/tmp/test"))))
  (should-not (watcherrun-validate-lisp-expression '(progn
                                                       (message "Hello")
                                                       (shell-command "echo hack"))))
  
  ;; Forbidden function as symbol
  (should-not (watcherrun-validate-lisp-expression 'delete-file)))

(ert-deftest watcherrun-test-lisp-evaluation-basic ()
  "Test basic Lisp expression evaluation."
  ;; Test simple arithmetic
  (should (= 3 (watcherrun-evaluate-lisp-expression "(+ 1 2)" test-file-path test-watcher-id)))
  
  ;; Test string operations
  (should (equal "HELLO" (watcherrun-evaluate-lisp-expression "(upcase \"hello\")" 
                                                              test-file-path test-watcher-id)))
  
  ;; Test list operations
  (should (= 3 (watcherrun-evaluate-lisp-expression "(length '(a b c))" 
                                                    test-file-path test-watcher-id))))

(ert-deftest watcherrun-test-lisp-evaluation-context-variables ()
  "Test that context variables are available during evaluation."
  ;; Test file-var is available
  (should (equal test-file-path 
                (watcherrun-evaluate-lisp-expression "file-var" test-file-path test-watcher-id)))
  
  ;; Test watcher-id-var is available
  (should (equal test-watcher-id
                (watcherrun-evaluate-lisp-expression "watcher-id-var" test-file-path test-watcher-id)))
  
  ;; Test change-time is available (should be a time value)
  (should (watcherrun-evaluate-lisp-expression "(time-to-seconds change-time)" 
                                               test-file-path test-watcher-id))
  
  ;; Test using file-var in expressions
  (should (equal "test-file.txt"
                (watcherrun-evaluate-lisp-expression "(file-name-nondirectory file-var)" 
                                                     test-file-path test-watcher-id))))

(ert-deftest watcherrun-test-lisp-evaluation-forbidden-expressions ()
  "Test that forbidden expressions return nil and log errors."
  ;; Redirect error messages to capture them
  (let ((error-messages '()))
    (cl-letf (((symbol-function 'watcherrun-report-error)
               (lambda (msg) (push msg error-messages))))
      
      ;; Test forbidden function
      (should-not (watcherrun-evaluate-lisp-expression "(delete-file \"/tmp/test\")" 
                                                       test-file-path test-watcher-id))
      (should error-messages)
      
      ;; Test nested forbidden function
      (setq error-messages '())
      (should-not (watcherrun-evaluate-lisp-expression "(when t (shell-command \"echo test\"))" 
                                                       test-file-path test-watcher-id))
      (should error-messages))))

(ert-deftest watcherrun-test-lisp-evaluation-parse-errors ()
  "Test handling of malformed expressions."
  (let ((error-messages '()))
    (cl-letf (((symbol-function 'watcherrun-report-error)
               (lambda (msg) (push msg error-messages))))
      
      ;; Test malformed expression
      (should-not (watcherrun-evaluate-lisp-expression "(+ 1 2" test-file-path test-watcher-id))
      (should error-messages)
      
      ;; Test invalid syntax
      (setq error-messages '())
      (should-not (watcherrun-evaluate-lisp-expression ")" test-file-path test-watcher-id))
      (should error-messages))))

(ert-deftest watcherrun-test-lisp-evaluation-runtime-errors ()
  "Test handling of runtime evaluation errors."
  (let ((error-messages '()))
    (cl-letf (((symbol-function 'watcherrun-report-error)
               (lambda (msg) (push msg error-messages))))
      
      ;; Test division by zero
      (should-not (watcherrun-evaluate-lisp-expression "(/ 1 0)" test-file-path test-watcher-id))
      (should error-messages)
      
      ;; Test undefined function
      (setq error-messages '())
      (should-not (watcherrun-evaluate-lisp-expression "(undefined-function-xyz)" 
                                                       test-file-path test-watcher-id))
      (should error-messages))))

(ert-deftest watcherrun-test-lisp-test-expression ()
  "Test the test-expression function for validation without evaluation."
  ;; Test safe expression
  (let ((result (watcherrun-test-lisp-expression "(+ 1 2)")))
    (should (car result))
    (should (string-match-p "safe" (cdr result))))
  
  ;; Test forbidden expression
  (let ((result (watcherrun-test-lisp-expression "(delete-file \"/tmp/test\")")))
    (should-not (car result))
    (should (string-match-p "forbidden" (cdr result))))
  
  ;; Test malformed expression
  (let ((result (watcherrun-test-lisp-expression "(+ 1 2")))
    (should-not (car result))
    (should (string-match-p "Parse error" (cdr result)))))

(ert-deftest watcherrun-test-lisp-common-use-cases ()
  "Test common use cases for Lisp expressions."
  ;; Test compilation command
  (let ((result (watcherrun-evaluate-lisp-expression 
                "(when (string-suffix-p \".c\" file-var) \"gcc -o test test.c\")"
                "/path/to/test.c" test-watcher-id)))
    (should (equal "gcc -o test test.c" result)))
  
  ;; Test file type detection
  (let ((result (watcherrun-evaluate-lisp-expression 
                "(cond ((string-suffix-p \".el\" file-var) \"elisp\")
                      ((string-suffix-p \".py\" file-var) \"python\")
                      (t \"unknown\"))"
                "/path/to/script.py" test-watcher-id)))
    (should (equal "python" result)))
  
  ;; Test message formatting
  (let ((result (watcherrun-evaluate-lisp-expression 
                "(format \"File %s changed\" (file-name-nondirectory file-var))"
                "/path/to/document.txt" test-watcher-id)))
    (should (equal "File document.txt changed" result))))

(provide 'test-lisp-evaluator)

;;; test-lisp-evaluator.el ends here
