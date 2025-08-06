;;; watcherrun-support.el --- Support functions for WatcherRun -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors

;;; Commentary:
;; This file contains the support components for WatcherRun, including:
;; - Session storage (watcher registry, buffer associations, error logging)
;; - Helper functions for data access and manipulation

;;; Code:

;; Required packages
(require 'ring)
(require 'cl-lib)

;;; Watcher structure definition
(cl-defstruct watcherrun-watcher
  "Structure representing a file watcher.
Each watcher monitors paths and executes commands when files change."
  id               ; Unique identifier string
  paths            ; List of watched file/directory paths
  command          ; Command string to execute
  command-type     ; 'system or 'lisp
  recursive        ; Boolean for directory watching
  file-descriptor  ; File notification descriptor
  last-executed    ; Timestamp of last execution
  execution-count  ; Number of times executed
  status)          ; 'active, 'paused, 'error

;;; Global session storage variables

(defvar watcherrun-watchers (make-hash-table :test 'equal)
  "Hash table storing all active watchers by ID.")

(defvar watcherrun-buffer-associations (make-hash-table :test 'equal)
  "Maps watcher IDs to their associated buffers.")

(defvar watcherrun-error-log (make-ring 100)
  "Ring buffer storing the last 100 errors.")

(defvar watcherrun-statistics
  '(:total-watchers 0
    :total-executions 0
    :total-errors 0
    :session-start-time nil
    :most-active-watcher nil)
  "Statistics tracking for the current session.")

;;; Session initialization and cleanup

(defun watcherrun-initialize-session ()
  "Initialize session storage and cleanup any stale data."
  (clrhash watcherrun-watchers)
  (clrhash watcherrun-buffer-associations)
  (setq watcherrun-error-log (make-ring 100))
  (setq watcherrun-statistics
        (plist-put watcherrun-statistics :session-start-time (current-time)))
  (setq watcherrun-statistics
        (plist-put watcherrun-statistics :total-watchers 0))
  (setq watcherrun-statistics
        (plist-put watcherrun-statistics :total-executions 0))
  (setq watcherrun-statistics
        (plist-put watcherrun-statistics :total-errors 0))
  (setq watcherrun-statistics
        (plist-put watcherrun-statistics :most-active-watcher nil)))

(defun watcherrun-cleanup-session ()
  "Clean up all watchers and resources at session end."
  (maphash (lambda (id _watcher)
             (watcherrun-remove-watcher-data id))
           watcherrun-watchers)
  (watcherrun-cleanup-buffers))

(defun watcherrun-cleanup-buffers ()
  "Clean up all buffers associated with watchers."
  (maphash (lambda (watcher-id buffers)
             (dolist (buffer-name buffers)
               (when (get-buffer buffer-name)
                 (kill-buffer buffer-name))))
           watcherrun-buffer-associations)
  (clrhash watcherrun-buffer-associations))

;;; Helper functions for data access and manipulation

(defun watcherrun-get-watcher (watcher-id)
  "Get watcher with WATCHER-ID from registry."
  (gethash watcher-id watcherrun-watchers))

(defun watcherrun-store-watcher (watcher)
  "Store WATCHER in the registry."
  (puthash (watcherrun-watcher-id watcher) watcher watcherrun-watchers)
  ;; Update statistics
  (setq watcherrun-statistics
        (plist-put watcherrun-statistics 
                   :total-watchers 
                   (hash-table-count watcherrun-watchers))))

(defun watcherrun-remove-watcher-data (watcher-id)
  "Remove all data associated with WATCHER-ID."
  (let ((watcher (watcherrun-get-watcher watcher-id)))
    (when watcher
      ;; Remove file descriptor if present
      (when (watcherrun-watcher-file-descriptor watcher)
        (ignore-errors
          (file-notify-rm-watch (watcherrun-watcher-file-descriptor watcher))))
      ;; Remove from registry
      (remhash watcher-id watcherrun-watchers)
      ;; Clean up buffers
      (when (gethash watcher-id watcherrun-buffer-associations)
        (dolist (buffer-name (gethash watcher-id watcherrun-buffer-associations))
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name)))
        (remhash watcher-id watcherrun-buffer-associations))
      ;; Update statistics
      (setq watcherrun-statistics
            (plist-put watcherrun-statistics 
                       :total-watchers 
                       (hash-table-count watcherrun-watchers))))))

(defun watcherrun-associate-buffer (watcher-id buffer-name)
  "Associate BUFFER-NAME with WATCHER-ID."
  (let ((current-buffers (gethash watcher-id watcherrun-buffer-associations)))
    (if current-buffers
        (puthash watcher-id (cons buffer-name current-buffers) watcherrun-buffer-associations)
      (puthash watcher-id (list buffer-name) watcherrun-buffer-associations))))

(defun watcherrun-get-associated-buffers (watcher-id)
  "Get all buffers associated with WATCHER-ID."
  (gethash watcher-id watcherrun-buffer-associations))

(defun watcherrun-update-watcher-stats (watcher-id)
  "Update execution statistics for watcher with WATCHER-ID."
  (let ((watcher (watcherrun-get-watcher watcher-id)))
    (when watcher
      ;; Update last executed time
      (setf (watcherrun-watcher-last-executed watcher) (current-time))
      ;; Increment execution count
      (setf (watcherrun-watcher-execution-count watcher) 
            (1+ (or (watcherrun-watcher-execution-count watcher) 0)))
      ;; Update most active watcher in statistics
      (let ((most-active (plist-get watcherrun-statistics :most-active-watcher))
            (total-executions (plist-get watcherrun-statistics :total-executions)))
        (setq watcherrun-statistics
              (plist-put watcherrun-statistics :total-executions (1+ total-executions)))
        (when (or (not most-active)
                  (> (watcherrun-watcher-execution-count watcher)
                     (watcherrun-watcher-execution-count 
                      (watcherrun-get-watcher most-active))))
          (setq watcherrun-statistics
                (plist-put watcherrun-statistics :most-active-watcher watcher-id)))))))

(provide 'watcherrun-support)
;;; watcherrun-support.el ends here
