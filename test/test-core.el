;;; test-core.el --- Tests for watcherrun-core -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;;; Commentary:
;; Tests for the core engine of watcherrun, focused on file notification handling.

;;; Code:

(require 'ert)
(require 'ert)
(require 'watcherrun-support)
(require 'watcherrun-utils)

;; Mock file-notify functionality for testing
(defvar file-notify-descriptors nil
  "Mock storage for file notification descriptors.")

(defun file-notify-add-watch (_path _events _callback)
  "Mock implementation of file-notify-add-watch."
  (let ((descriptor (gensym "file-notify-")))
    (push descriptor file-notify-descriptors)
    descriptor))

(defun file-notify-rm-watch (descriptor)
  "Mock implementation of file-notify-rm-watch."
  (setq file-notify-descriptors 
        (delq descriptor file-notify-descriptors))
  nil)

(defun file-notify-valid-p (descriptor)
  "Mock implementation of file-notify-valid-p."
  (and (symbolp descriptor)
       (memq descriptor file-notify-descriptors)))

(require 'watcherrun-core)

;; Test setup and teardown
(defvar watcherrun-test-temp-dir nil
  "Temporary directory for testing file operations.")

(defun watcherrun-test-setup ()
  "Set up temporary test environment."
  (setq watcherrun-test-temp-dir (make-temp-file "watcherrun-test-" t))
  (let ((test-file (expand-file-name "test-file.txt" watcherrun-test-temp-dir)))
    (with-temp-file test-file
      (insert "Test content"))))

(defun watcherrun-test-teardown ()
  "Clean up temporary test environment."
  (when (and watcherrun-test-temp-dir 
             (file-exists-p watcherrun-test-temp-dir))
    (delete-directory watcherrun-test-temp-dir t)))

;; Tests for file pattern filtering
(ert-deftest watcherrun-test-should-ignore-file-p ()
  "Test file pattern filtering."
  (should (watcherrun--should-ignore-file-p "file.tmp"))
  (should (watcherrun--should-ignore-file-p "file~"))
  (should (watcherrun--should-ignore-file-p "file.bak"))
  (should (watcherrun--should-ignore-file-p "file.swp"))
  (should (watcherrun--should-ignore-file-p ".hidden"))
  (let ((watcherrun-ignore-hidden-files nil))
    (should-not (watcherrun--should-ignore-file-p ".notignored"))))

;; Tests for event type filtering
(ert-deftest watcherrun-test-should-process-event-p ()
  "Test event type filtering."
  (should (watcherrun--should-process-event-p 'changed))
  (should (watcherrun--should-process-event-p 'created))
  (should (watcherrun--should-process-event-p 'renamed))
  (should-not (watcherrun--should-process-event-p 'deleted))
  (should-not (watcherrun--should-process-event-p 'attribute-changed)))

;; Tests for debouncing
(ert-deftest watcherrun-test-debounce-event-p ()
  "Test event debouncing."
  (let ((file-path "/test/path.txt")
        (watcherrun-debounce-time 0.1))
    ;; First event should not be debounced
    (should-not (watcherrun--debounce-event-p file-path))
    ;; Immediate second event should be debounced
    (should (watcherrun--debounce-event-p file-path))
    ;; Wait for debounce time
    (sleep-for 0.2)
    ;; Event after waiting should not be debounced
    (should-not (watcherrun--debounce-event-p file-path))))

;; Test file watcher setup and cleanup (requires file system access)
(ert-deftest watcherrun-test-file-watcher-lifecycle ()
  :expected-result :passed
  "Test file watcher setup and cleanup."
  (watcherrun-test-setup)
  (unwind-protect
      (let* ((test-path (expand-file-name "test-file.txt" watcherrun-test-temp-dir))
             (callback (lambda (_event) nil))
             (descriptors (watcherrun-setup-file-watcher (list test-path) nil callback)))
        ;; Check that we got descriptors
        (should descriptors)
        (should (> (length descriptors) 0)))
    (watcherrun-test-teardown)))

(provide 'test-core)
;;; test-core.el ends here
