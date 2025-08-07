;;; test-core-manager.el --- Tests for watcherrun-core watcher manager -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; Author: WatcherRun Contributors
;; Keywords: convenience, tools, processes

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Tests for the watcher manager functionality in watcherrun-core.el.
;; Tests cover add/remove/modify/list operations and conflict detection.

;;; Code:

(require 'ert)
(require 'watcherrun-support)
(require 'watcherrun-utils)
(require 'watcherrun-core)

;; Test data and helpers
(defvar test-watcherrun-temp-dir nil
  "Temporary directory for tests.")

(defvar test-watcherrun-temp-file nil
  "Temporary file for tests.")

(defun test-watcherrun-setup ()
  "Set up test environment."
  ;; Create temporary directory and file
  (setq test-watcherrun-temp-dir (make-temp-file "watcherrun-test-" t))
  (setq test-watcherrun-temp-file (make-temp-file "watcherrun-test-file-" nil ".txt"))
  
  ;; Clean up any existing watchers
  (watcherrun-cleanup-all-watchers))

(defun test-watcherrun-teardown ()
  "Clean up test environment."
  ;; Clean up watchers
  (watcherrun-cleanup-all-watchers)
  
  ;; Remove temporary files
  (when (and test-watcherrun-temp-dir (file-exists-p test-watcherrun-temp-dir))
    (delete-directory test-watcherrun-temp-dir t))
  (when (and test-watcherrun-temp-file (file-exists-p test-watcherrun-temp-file))
    (delete-file test-watcherrun-temp-file)))

;; Test watcher ID generation
(ert-deftest watcherrun-test-generate-watcher-id ()
  "Test unique watcher ID generation."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((id1 (watcherrun--generate-watcher-id))
            (id2 (watcherrun--generate-watcher-id)))
        ;; IDs should be strings
        (should (stringp id1))
        (should (stringp id2))
        ;; IDs should be different
        (should-not (string= id1 id2))
        ;; IDs should follow expected format
        (should (string-match-p "^watcher-[0-9]+-[0-9]+$" id1)))
    (test-watcherrun-teardown)))

;; Test watcher lookup functions
(ert-deftest watcherrun-test-find-watcher-by-id ()
  "Test finding watchers by ID."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file)
                         "echo test" 'system nil)))
        ;; Should find existing watcher
        (should (watcherrun-find-watcher-by-id watcher-id))
        ;; Should return nil for non-existent watcher
        (should-not (watcherrun-find-watcher-by-id "non-existent-id")))
    (test-watcherrun-teardown)))

;; Test add watcher functionality
(ert-deftest watcherrun-test-add-watcher-basic ()
  "Test basic add watcher functionality."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file)
                         "echo test" 'system nil)))
        ;; Should return a watcher ID
        (should (stringp watcher-id))
        ;; Watcher should exist in registry
        (should (watcherrun-find-watcher-by-id watcher-id))
        ;; Watcher count should be 1
        (should (= 1 (watcherrun-get-watcher-count))))
    (test-watcherrun-teardown)))

(ert-deftest watcherrun-test-add-watcher-validation ()
  "Test input validation for add watcher."
  (test-watcherrun-setup)
  (unwind-protect
      (progn
        ;; Empty paths should error
        (should-error (watcherrun-add-watcher nil "echo test" 'system nil))
        (should-error (watcherrun-add-watcher '() "echo test" 'system nil))
        
        ;; Non-string command should error
        (should-error (watcherrun-add-watcher 
                       (list test-watcherrun-temp-file) 123 'system nil))
        
        ;; Invalid command type should error
        (should-error (watcherrun-add-watcher 
                       (list test-watcherrun-temp-file) "echo test" 'invalid nil))
        
        ;; Non-existent path should error
        (should-error (watcherrun-add-watcher 
                       '("/non/existent/path") "echo test" 'system nil)))
    (test-watcherrun-teardown)))

(ert-deftest watcherrun-test-add-watcher-conflicts ()
  "Test conflict detection when adding watchers."
  (test-watcherrun-setup)
  (unwind-protect
      (progn
        ;; Add first watcher
        (watcherrun-add-watcher (list test-watcherrun-temp-file)
                                "echo test1" 'system nil)
        
        ;; Adding second watcher for same path should error
        (should-error (watcherrun-add-watcher (list test-watcherrun-temp-file)
                                              "echo test2" 'system nil)))
    (test-watcherrun-teardown)))

;; Test remove watcher functionality
(ert-deftest watcherrun-test-remove-watcher ()
  "Test remove watcher functionality."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file)
                         "echo test" 'system nil)))
        ;; Watcher should exist
        (should (watcherrun-find-watcher-by-id watcher-id))
        (should (= 1 (watcherrun-get-watcher-count)))
        
        ;; Remove watcher
        (should (watcherrun-remove-watcher watcher-id))
        
        ;; Watcher should no longer exist
        (should-not (watcherrun-find-watcher-by-id watcher-id))
        (should (= 0 (watcherrun-get-watcher-count)))
        
        ;; Removing non-existent watcher should error
        (should-error (watcherrun-remove-watcher "non-existent-id")))
    (test-watcherrun-teardown)))

;; Test modify watcher functionality
(ert-deftest watcherrun-test-modify-watcher ()
  "Test modify watcher functionality."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file)
                         "echo test" 'system nil)))
        ;; Modify command
        (should (watcherrun-modify-watcher watcher-id "echo modified"))
        
        ;; Check that command was modified
        (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
          (should (string= "echo modified" (watcherrun-watcher-command watcher))))
        
        ;; Modify command type
        (should (watcherrun-modify-watcher watcher-id "(message \"test\")" 'lisp))
        
        ;; Check that command and type were modified
        (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
          (should (string= "(message \"test\")" (watcherrun-watcher-command watcher)))
          (should (eq 'lisp (watcherrun-watcher-command-type watcher))))
        
        ;; Modifying non-existent watcher should error
        (should-error (watcherrun-modify-watcher "non-existent-id" "echo test")))
    (test-watcherrun-teardown)))

;; Test list watchers functionality
(ert-deftest watcherrun-test-list-watchers ()
  "Test list watchers functionality."
  (test-watcherrun-setup)
  (unwind-protect
      (progn
        ;; Initially should be empty
        (should (null (watcherrun-list-watchers)))
        
        ;; Add watchers
        (let ((id1 (watcherrun-add-watcher (list test-watcherrun-temp-file)
                                           "echo test1" 'system nil))
              (id2 (watcherrun-add-watcher (list test-watcherrun-temp-dir)
                                           "echo test2" 'system nil)))
          
          ;; Should return list of formatted strings
          (let ((watcher-list (watcherrun-list-watchers)))
            (should (= 2 (length watcher-list)))
            (should (cl-every #'stringp watcher-list))
            ;; Each entry should contain watcher ID and command
            (should (cl-some (lambda (entry) (string-match-p id1 entry)) watcher-list))
            (should (cl-some (lambda (entry) (string-match-p id2 entry)) watcher-list)))))
    (test-watcherrun-teardown)))

;; Test pause/resume functionality
(ert-deftest watcherrun-test-pause-resume-watcher ()
  "Test pause and resume watcher functionality."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file)
                         "echo test" 'system nil)))
        
        ;; Watcher should be active initially
        (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
          (should (eq 'active (watcherrun-watcher-status watcher))))
        
        ;; Pause watcher
        (should (watcherrun-pause-watcher watcher-id))
        
        ;; Watcher should be paused
        (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
          (should (eq 'paused (watcherrun-watcher-status watcher)))
          (should-not (watcherrun-watcher-file-descriptor watcher)))
        
        ;; Resume watcher
        (should (watcherrun-resume-watcher watcher-id))
        
        ;; Watcher should be active again
        (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
          (should (eq 'active (watcherrun-watcher-status watcher)))
          (should (watcherrun-watcher-file-descriptor watcher)))
        
        ;; Pausing non-existent watcher should error
        (should-error (watcherrun-pause-watcher "non-existent-id"))
        
        ;; Resuming non-existent watcher should error
        (should-error (watcherrun-resume-watcher "non-existent-id"))
        
        ;; Resuming active watcher should error
        (should-error (watcherrun-resume-watcher watcher-id)))
    (test-watcherrun-teardown)))

;; Test cleanup functionality
(ert-deftest watcherrun-test-cleanup-all-watchers ()
  "Test cleanup all watchers functionality."
  (test-watcherrun-setup)
  (unwind-protect
      (progn
        ;; Add multiple watchers
        (watcherrun-add-watcher (list test-watcherrun-temp-file)
                                "echo test1" 'system nil)
        (watcherrun-add-watcher (list test-watcherrun-temp-dir)
                                "echo test2" 'system nil)
        
        ;; Should have watchers
        (should (> (watcherrun-get-watcher-count) 0))
        
        ;; Clean up all watchers
        (should (watcherrun-cleanup-all-watchers))
        
        ;; Should have no watchers
        (should (= 0 (watcherrun-get-watcher-count)))
        (should (null (watcherrun-list-watchers))))
    (test-watcherrun-teardown)))

;; Test path index management
(ert-deftest watcherrun-test-path-index-management ()
  "Test path index for conflict detection."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file)
                         "echo test" 'system nil)))
        
        ;; Path should be registered
        (should (gethash (watcherrun-normalize-path test-watcherrun-temp-file)
                         watcherrun--path-index))
        
        ;; Remove watcher
        (watcherrun-remove-watcher watcher-id)
        
        ;; Path should be unregistered
        (should-not (gethash (watcherrun-normalize-path test-watcherrun-temp-file)
                             watcherrun--path-index)))
    (test-watcherrun-teardown)))

;; Test watcher with multiple paths
(ert-deftest watcherrun-test-multiple-paths ()
  "Test watcher with multiple paths."
  (test-watcherrun-setup)
  (unwind-protect
      (let ((paths (list test-watcherrun-temp-file test-watcherrun-temp-dir))
            (watcher-id (watcherrun-add-watcher 
                         (list test-watcherrun-temp-file test-watcherrun-temp-dir)
                         "echo test" 'system nil)))
        
        ;; Watcher should exist
        (should (watcherrun-find-watcher-by-id watcher-id))
        
        ;; Watcher should contain both paths
        (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
          (should (equal paths (watcherrun-watcher-paths watcher))))
        
        ;; Both paths should be in index
        (dolist (path paths)
          (should (gethash (watcherrun-normalize-path path) watcherrun--path-index))))
    (test-watcherrun-teardown)))

(provide 'test-core-manager)
;;; test-core-manager.el ends here
