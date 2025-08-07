;;; test-ui.el --- Tests for WatcherRun UI components -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;;; Commentary:
;; Tests for the WatcherRun UI menu system.

;;; Code:

(require 'ert)
(require 'watcherrun-ui)
(require 'watcherrun-support)
(require 'watcherrun-core)

;; Test setup and teardown
(defvar watcherrun-test-temp-files nil
  "List of temporary files created during tests.")

(defun watcherrun-test-setup ()
  "Set up test environment."
  (setq watcherrun-test-temp-files nil)
  (clrhash watcherrun-watchers))

(defun watcherrun-test-teardown ()
  "Clean up test environment."
  (clrhash watcherrun-watchers)
  (dolist (file watcherrun-test-temp-files)
    (when (file-exists-p file)
      (delete-file file)))
  (setq watcherrun-test-temp-files nil))

(defun watcherrun-test-create-temp-file ()
  "Create a temporary file for testing."
  (let ((temp-file (make-temp-file "watcherrun-test-")))
    (push temp-file watcherrun-test-temp-files)
    temp-file))

;; Tests for menu command functions

(ert-deftest watcherrun-test-list-watchers-menu-empty ()
  "Test listing watchers when none exist."
  (watcherrun-test-setup)
  (let ((messages nil))
    (cl-letf (((symbol-function 'message) 
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (watcherrun-list-watchers-menu)
      (should (member "No active watchers" messages))))
  (watcherrun-test-teardown))

(ert-deftest watcherrun-test-list-watchers-menu-with-watchers ()
  "Test listing watchers when some exist."
  (watcherrun-test-setup)
  (let ((temp-file (watcherrun-test-create-temp-file)))
    ;; Add a test watcher
    (watcherrun-add-watcher (list temp-file) "echo test" 'system nil)
    
    ;; Test that with watchers present, we don't get the empty message
    (let ((messages nil))
      (cl-letf (((symbol-function 'with-output-to-temp-buffer)
                 (lambda (buffer-name &rest body)
                   (should (string= buffer-name "*WatcherRun Watchers*"))
                   t))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (watcherrun-list-watchers-menu)
        (should-not (member "No active watchers" messages)))))
  (watcherrun-test-teardown))

(ert-deftest watcherrun-test-add-watcher-interactive-mock ()
  "Test interactive watcher addition with mocked input."
  (watcherrun-test-setup)
  (let ((temp-file (watcherrun-test-create-temp-file))
        (messages nil))
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (&rest _) temp-file))
              ((symbol-function 'y-or-n-p)
               (lambda (_) nil))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?s))
              ((symbol-function 'read-string)
               (lambda (&rest _) "echo test"))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (watcherrun-add-watcher-interactive)
      (should (> (watcherrun-get-watcher-count) 0))
      (should (cl-some (lambda (msg) (string-match-p "Added watcher" msg)) messages))))
  (watcherrun-test-teardown))

(ert-deftest watcherrun-test-delete-watcher-menu-empty ()
  "Test deleting watchers when none exist."
  (watcherrun-test-setup)
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (watcherrun-delete-watcher-menu)
      (should (member "No watchers to delete" messages))))
  (watcherrun-test-teardown))

(ert-deftest watcherrun-test-modify-watcher-menu-empty ()
  "Test modifying watchers when none exist."
  (watcherrun-test-setup)
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (watcherrun-modify-watcher-menu)
      (should (member "No watchers to modify" messages))))
  (watcherrun-test-teardown))

(ert-deftest watcherrun-test-show-error-buffer-no-buffer ()
  "Test showing error buffer when it doesn't exist."
  (watcherrun-test-setup)
  (let ((messages nil))
    (cl-letf (((symbol-function 'get-buffer)
               (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (watcherrun-show-error-buffer)
      (should (member "No error buffer found" messages))))
  (watcherrun-test-teardown))

(ert-deftest watcherrun-test-show-error-buffer-exists ()
  "Test showing error buffer when it exists."
  (watcherrun-test-setup)
  (let ((displayed-buffer nil))
    (cl-letf (((symbol-function 'get-buffer)
               (lambda (name) 
                 (when (string= name "*WatcherRun Errors*")
                   'dummy-buffer)))
              ((symbol-function 'display-buffer)
               (lambda (buffer)
                 (setq displayed-buffer buffer))))
      (watcherrun-show-error-buffer)
      (should (eq displayed-buffer 'dummy-buffer))))
  (watcherrun-test-teardown))

;; Test menu structure
(ert-deftest watcherrun-test-menu-defined ()
  "Test that the WatcherRun menu is properly defined."
  (should (boundp 'watcherrun-menu))
  (should watcherrun-menu))

(ert-deftest watcherrun-test-keymap-defined ()
  "Test that the WatcherRun keymap is properly defined."
  (should (boundp 'watcherrun-mode-map))
  (should watcherrun-mode-map))

(provide 'test-ui)
;;; test-ui.el ends here
