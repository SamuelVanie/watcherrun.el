;;; test-support.el --- Tests for WatcherRun support functions -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors

;;; Commentary:
;; ERT tests for the WatcherRun support components, focusing on:
;; - Session storage
;; - Watcher structure
;; - Helper functions

;;; Code:

(require 'ert)
(require 'watcherrun-support)

;; Silence byte-compiler warnings for tests
(declare-function make-watcherrun-watcher "watcherrun-support")
(declare-function watcherrun-watcher-id "watcherrun-support")
(declare-function watcherrun-watcher-paths "watcherrun-support")
(declare-function watcherrun-watcher-command "watcherrun-support")
(declare-function watcherrun-watcher-command-type "watcherrun-support")
(declare-function watcherrun-watcher-recursive "watcherrun-support")
(declare-function watcherrun-watcher-file-descriptor "watcherrun-support")
(declare-function watcherrun-watcher-last-executed "watcherrun-support")
(declare-function watcherrun-watcher-execution-count "watcherrun-support")
(declare-function watcherrun-watcher-status "watcherrun-support")

;;; Test watcher structure

(ert-deftest watcherrun-test-watcher-struct ()
  "Test creating and accessing watcher structs."
  (let ((watcher (make-watcherrun-watcher 
                  :id "test-001"
                  :paths '("/path/to/file.txt")
                  :command "echo 'file changed'"
                  :command-type 'system
                  :recursive nil
                  :file-descriptor nil
                  :last-executed nil
                  :execution-count 0
                  :status 'active)))
    
    ;; Test structure fields
    (should (string= (watcherrun-watcher-id watcher) "test-001"))
    (should (equal (watcherrun-watcher-paths watcher) '("/path/to/file.txt")))
    (should (string= (watcherrun-watcher-command watcher) "echo 'file changed'"))
    (should (eq (watcherrun-watcher-command-type watcher) 'system))
    (should-not (watcherrun-watcher-recursive watcher))
    (should-not (watcherrun-watcher-file-descriptor watcher))
    (should-not (watcherrun-watcher-last-executed watcher))
    (should (= (watcherrun-watcher-execution-count watcher) 0))
    (should (eq (watcherrun-watcher-status watcher) 'active))))

;;; Test session initialization

(ert-deftest watcherrun-test-initialize-session ()
  "Test session initialization."
  ;; Add some test data to make sure it gets cleared
  (puthash "test-001" (make-watcherrun-watcher :id "test-001") watcherrun-watchers)
  (puthash "test-001" '("buffer1" "buffer2") watcherrun-buffer-associations)
  
  ;; Initialize session
  (watcherrun-initialize-session)
  
  ;; Check that data was cleared
  (should (= (hash-table-count watcherrun-watchers) 0))
  (should (= (hash-table-count watcherrun-buffer-associations) 0))
  (should (= (ring-size watcherrun-error-log) 100))
  (should (ring-empty-p watcherrun-error-log))
  
  ;; Check that statistics were reset
  (should (plist-get watcherrun-statistics :session-start-time))
  (should (= (plist-get watcherrun-statistics :total-watchers) 0))
  (should (= (plist-get watcherrun-statistics :total-executions) 0))
  (should (= (plist-get watcherrun-statistics :total-errors) 0))
  (should-not (plist-get watcherrun-statistics :most-active-watcher)))

;;; Test watcher storage functions

(ert-deftest watcherrun-test-store-get-watcher ()
  "Test storing and retrieving watchers."
  (watcherrun-initialize-session)
  
  ;; Create and store test watcher
  (let ((watcher (make-watcherrun-watcher 
                  :id "test-001"
                  :paths '("/path/to/file.txt")
                  :command "echo 'file changed'")))
    (watcherrun-store-watcher watcher)
    
    ;; Test retrieval
    (let ((retrieved (watcherrun-get-watcher "test-001")))
      (should retrieved)
      (should (string= (watcherrun-watcher-id retrieved) "test-001")))
    
    ;; Test statistics update
    (should (= (plist-get watcherrun-statistics :total-watchers) 1))
    
    ;; Test non-existent watcher
    (should-not (watcherrun-get-watcher "nonexistent"))))

;;; Test buffer association functions

(ert-deftest watcherrun-test-buffer-associations ()
  "Test buffer association functions."
  (watcherrun-initialize-session)
  
  ;; Associate buffers with a watcher
  (watcherrun-associate-buffer "test-001" "buffer1")
  (watcherrun-associate-buffer "test-001" "buffer2")
  
  ;; Test retrieval
  (let ((buffers (watcherrun-get-associated-buffers "test-001")))
    (should (= (length buffers) 2))
    (should (member "buffer1" buffers))
    (should (member "buffer2" buffers)))
  
  ;; Test unknown watcher
  (should-not (watcherrun-get-associated-buffers "unknown")))

;;; Test watcher statistics update

(ert-deftest watcherrun-test-watcher-stats ()
  "Test updating watcher statistics."
  (watcherrun-initialize-session)
  
  ;; Create and store test watchers
  (let ((watcher1 (make-watcherrun-watcher 
                   :id "test-001"
                   :paths '("/path/to/file1.txt")
                   :execution-count 0))
        (watcher2 (make-watcherrun-watcher 
                   :id "test-002"
                   :paths '("/path/to/file2.txt")
                   :execution-count 0)))
    (watcherrun-store-watcher watcher1)
    (watcherrun-store-watcher watcher2)
    
    ;; Update stats for first watcher
    (watcherrun-update-watcher-stats "test-001")
    
    ;; Check execution count and timestamp
    (let ((updated (watcherrun-get-watcher "test-001")))
      (should (= (watcherrun-watcher-execution-count updated) 1))
      (should (watcherrun-watcher-last-executed updated)))
    
    ;; Check global statistics
    (should (= (plist-get watcherrun-statistics :total-executions) 1))
    (should (string= (plist-get watcherrun-statistics :most-active-watcher) "test-001"))
    
    ;; Update second watcher multiple times
    (watcherrun-update-watcher-stats "test-002")
    (watcherrun-update-watcher-stats "test-002")
    
    ;; Check that most active watcher has changed
    (should (string= (plist-get watcherrun-statistics :most-active-watcher) "test-002"))
    (should (= (plist-get watcherrun-statistics :total-executions) 3))))

;;; Test watcher removal

(ert-deftest watcherrun-test-remove-watcher ()
  "Test removing watcher data."
  (watcherrun-initialize-session)
  
  ;; Create and store test watcher
  (let ((watcher (make-watcherrun-watcher 
                  :id "test-001"
                  :paths '("/path/to/file.txt")
                  :command "echo 'file changed'")))
    (watcherrun-store-watcher watcher)
    
    ;; Associate a buffer
    (watcherrun-associate-buffer "test-001" "buffer1")
    
    ;; Remove the watcher
    (watcherrun-remove-watcher-data "test-001")
    
    ;; Check that it's gone
    (should-not (watcherrun-get-watcher "test-001"))
    (should-not (watcherrun-get-associated-buffers "test-001"))
    (should (= (plist-get watcherrun-statistics :total-watchers) 0))))

(provide 'test-support)
;;; test-support.el ends here
