;;; watcherrun-core.el --- Core engine for watcherrun -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; Author: WatcherRun Contributors
;; Keywords: convenience, tools, processes

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Core engine implementing file notification handling, event filtering,
;; and watcher management for WatcherRun.

;;; Code:

;; file-notify is built into Emacs 27+
(require 'filenotify)
(require 'cl-lib)
(require 'watcherrun-support)
(require 'watcherrun-utils)

;; Define logging functions if not defined
(declare-function watcherrun-log-debug "watcherrun-support")
(declare-function watcherrun-log-info "watcherrun-support")
(declare-function watcherrun-log-warning "watcherrun-support")
(declare-function watcherrun-report-error "watcherrun-support")

(unless (fboundp 'watcherrun-log-debug)
  (defun watcherrun-log-debug (format-string &rest args)
    "Log debug message using FORMAT-STRING and ARGS."
    (message "[WatcherRun DEBUG] %s" (apply #'format format-string args))))

(unless (fboundp 'watcherrun-log-info)
  (defun watcherrun-log-info (format-string &rest args)
    "Log info message using FORMAT-STRING and ARGS."
    (message "[WatcherRun INFO] %s" (apply #'format format-string args))))

(unless (fboundp 'watcherrun-log-warning)
  (defun watcherrun-log-warning (format-string &rest args)
    "Log warning message using FORMAT-STRING and ARGS."
    (message "[WatcherRun WARNING] %s" (apply #'format format-string args))))

(unless (fboundp 'watcherrun-report-error)
  (defun watcherrun-report-error (message)
    "Report error with MESSAGE."
    (message "[WatcherRun ERROR] %s" message)))

;; Customizable variables
(defgroup watcherrun-core nil
  "Core settings for WatcherRun."
  :group 'watcherrun)

(defcustom watcherrun-debounce-time 0.1
  "Time in seconds to debounce rapid file change events."
  :type 'number
  :group 'watcherrun-core)

(defcustom watcherrun-ignore-patterns '("\\.tmp$" "~$" "\\.bak$" "\\.swp$")
  "List of regexp patterns for files to ignore."
  :type '(repeat string)
  :group 'watcherrun-core)

(defcustom watcherrun-ignore-hidden-files t
  "Whether to ignore hidden files (starting with '.')."
  :type 'boolean
  :group 'watcherrun-core)

;; Event tracking for debouncing
(defvar watcherrun--last-events (make-hash-table :test 'equal)
  "Hash table tracking the last event time for each file path.")

;; Forward declarations
(declare-function watcherrun-execute-command "watcherrun-exec")
(declare-function watcherrun-find-watcher-by-descriptor "watcherrun-core")

;; File pattern filtering functions
(defun watcherrun--should-ignore-file-p (file-path)
  "Return non-nil if FILE-PATH should be ignored based on patterns."
  (let ((file-name (file-name-nondirectory file-path)))
    (or 
     ;; Check if it's a hidden file
     (and watcherrun-ignore-hidden-files
          (string-match-p "^\\." file-name))
     ;; Check against ignore patterns
     (cl-some (lambda (pattern)
                (string-match-p pattern file-name))
              watcherrun-ignore-patterns))))

(defun watcherrun--should-process-event-p (action)
  "Return non-nil if ACTION type should trigger command execution."
  (memq action '(changed created renamed)))

;; Debouncing function
(defun watcherrun--debounce-event-p (file-path)
  "Return non-nil if event for FILE-PATH should be debounced."
  (let ((current-time (float-time))
        (last-time (gethash file-path watcherrun--last-events 0)))
    (if (< (- current-time last-time) watcherrun-debounce-time)
        t  ;; Debounce this event
      (puthash file-path current-time watcherrun--last-events)
      nil))) ;; Process this event

;; Main event handler
(defun watcherrun-handle-change-event (event)
  "Handle file change EVENT from file-notify."
  (let* ((descriptor (nth 0 event))
         (action (nth 1 event))
         (file (nth 2 event))
         (file1 (nth 3 event))  ;; For rename events, this is the new name
         (actual-file (if (eq action 'renamed) file1 file))
         (watcher (watcherrun-find-watcher-by-descriptor descriptor)))
    
    ;; Log the event for debugging
    (watcherrun-log-debug "File event: %S on %s" action actual-file)
    
    ;; Check if we should process this event
    (when (and watcher
               (watcherrun--should-process-event-p action)
               (not (watcherrun--should-ignore-file-p actual-file))
               (not (watcherrun--debounce-event-p actual-file)))
      
      ;; For error management, wrap in condition-case
      (condition-case err
          (progn
            (watcherrun-log-info "Executing command for %s (event: %S)" 
                                 actual-file action)
            (watcherrun-execute-command watcher actual-file))
        (error
         (watcherrun-report-error 
          (format "Error executing command for %s: %s" 
                  actual-file (error-message-string err)))
         ;; Update watcher status to error
         (when watcher
           (setf (watcherrun-watcher-status watcher) 'error)))))))

(defun watcherrun-setup-file-watcher (paths recursive callback)
  "Set up file watching for PATHS.
If RECURSIVE is non-nil, watch directories recursively.
CALLBACK is the function to call when changes are detected.
Returns a list of descriptors for all created watchers."
  (let ((descriptors nil))
    (dolist (path paths)
      (condition-case err
          (let* ((normalized-path (watcherrun-normalize-path path))
                 (descriptor (file-notify-add-watch 
                              normalized-path
                              '(change attribute-change)
                              callback)))
            (push descriptor descriptors)
            
            ;; If recursive and this is a directory, set up watchers for subdirs
            (when (and recursive (file-directory-p normalized-path))
              (dolist (item (directory-files-recursively 
                             normalized-path "" t))
                (when (and (file-directory-p item)
                           (not (string-match-p "/\\.\\.$" item))
                           (not (string-match-p "/\\.$" item)))
                  (condition-case nil
                      (push (file-notify-add-watch 
                             item '(change attribute-change) callback)
                            descriptors)
                    (error
                     (watcherrun-log-warning 
                      "Could not watch subdirectory: %s" item)))))))
        (error
         (watcherrun-report-error 
          (format "Error setting up watcher for %s: %s" 
                  path (error-message-string err))))))
    descriptors))

(defun watcherrun-cleanup-file-watcher (descriptors)
  "Clean up file watchers identified by DESCRIPTORS."
  (dolist (descriptor descriptors)
    (condition-case err
        (when (file-notify-valid-p descriptor)
          (file-notify-rm-watch descriptor)
          (watcherrun-log-debug "Removed file watcher: %S" descriptor))
      (error
       (watcherrun-log-warning 
        "Error removing file watcher: %s" (error-message-string err))))))

(defun watcherrun-restart-watcher (watcher)
  "Restart the file watcher for WATCHER that might have been invalidated."
  (when watcher
    (let ((descriptors (watcherrun-watcher-file-descriptor watcher))
          (paths (watcherrun-watcher-paths watcher))
          (recursive (watcherrun-watcher-recursive watcher)))
      
      ;; Clean up any existing descriptors
      (watcherrun-cleanup-file-watcher descriptors)
      
      ;; Set up new watchers
      (let ((new-descriptors (watcherrun-setup-file-watcher 
                              paths recursive #'watcherrun-handle-change-event)))
        ;; Update watcher with new descriptors
        (setf (watcherrun-watcher-file-descriptor watcher) new-descriptors)
        (setf (watcherrun-watcher-status watcher) 'active)
        (watcherrun-log-info "Restarted watcher for paths: %S" paths)))))

(provide 'watcherrun-core)
;;; watcherrun-core.el ends here
