;;; watcherrun-support.el --- Support functions for WatcherRun -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors

;;; Commentary:
;; This file contains the support components for WatcherRun, including:
;; - Session storage (watcher registry, buffer associations, error logging)
;; - Error handling and dedicated error buffer management
;; - Helper functions for data access and manipulation

;;; Code:

;; Required packages
(require 'ring)
(require 'cl-lib)
(require 'compile)

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

;;; Error handling system

;; Error type constants
(defconst watcherrun-error-file-error 'file-error
  "Error type for file system issues (permission denied, file not found).")

(defconst watcherrun-error-command-error 'command-error
  "Error type for system command execution failures.")

(defconst watcherrun-error-lisp-error 'lisp-error
  "Error type for Emacs Lisp evaluation errors.")

(defconst watcherrun-error-process-error 'process-error
  "Error type for async process management issues.")

(defconst watcherrun-error-validation-error 'validation-error
  "Error type for user input validation failures.")

(defconst watcherrun-error-internal-error 'internal-error
  "Error type for unexpected system errors.")

;; Error buffer and display
(defvar watcherrun-error-buffer-name "*WatcherRun Errors*"
  "Name of the buffer for displaying WatcherRun errors.")

(defvar watcherrun-show-error-notifications t
  "Whether to show notifications for errors.")

(defun watcherrun-log-error (watcher-id error-type message &optional context)
  "Log error with full context for debugging.
WATCHER-ID is the ID of the watcher that encountered the error.
ERROR-TYPE is one of the watcherrun-error-* constants.
MESSAGE is the error message.
CONTEXT is optional additional information about the error."
  (let ((error-entry (list
                     :timestamp (current-time)
                     :watcher-id watcher-id
                     :error-type error-type
                     :message message
                     :context context
                     :stack-trace (when (eq error-type 'lisp-error)
                                   (backtrace-to-string)))))
    
    ;; Add to error log
    (ring-insert watcherrun-error-log error-entry)
    
    ;; Update statistics
    (setq watcherrun-statistics
          (plist-put watcherrun-statistics
                     :total-errors
                     (1+ (or (plist-get watcherrun-statistics :total-errors) 0))))
    
    ;; Update error buffer
    (watcherrun-update-error-buffer error-entry)
    
    ;; Show notification if enabled
    (when watcherrun-show-error-notifications
      (watcherrun-show-error-notification error-entry))))

(defun watcherrun-format-error-entry (error-entry)
  "Format ERROR-ENTRY for display in the error buffer."
  (let* ((timestamp (plist-get error-entry :timestamp))
         (watcher-id (plist-get error-entry :watcher-id))
         (error-type (plist-get error-entry :error-type))
         (message (plist-get error-entry :message))
         (context (plist-get error-entry :context))
         (stack-trace (plist-get error-entry :stack-trace))
         (formatted-time (format-time-string "[%Y-%m-%d %H:%M:%S]" timestamp))
         (error-type-str (upcase (symbol-name error-type))))
    
    (concat formatted-time " " error-type-str
            (when watcher-id
              (format " (Watcher: %s)" watcher-id))
            "
"
            message
            (when context
              (concat "
Context: " context))
            (when stack-trace
              (concat "
Stack trace:
" stack-trace))
            "
"
            (watcherrun-format-error-actions error-entry)
            "

")))

(defun watcherrun-format-error-actions (error-entry)
  "Format recovery actions for ERROR-ENTRY based on error type."
  (let ((error-type (plist-get error-entry :error-type))
        (watcher-id (plist-get error-entry :watcher-id)))
    (concat "Actions: "
            (cond
             ((eq error-type 'file-error)
              "[R]etry [U]pdate Path [D]isable Watcher")
             ((eq error-type 'command-error)
              "[R]etry [D]isable Watcher [E]dit Command")
             ((eq error-type 'lisp-error)
              "[E]dit Expression [D]isable Watcher")
             ((eq error-type 'process-error)
              "[R]etry [D]isable Watcher")
             ((eq error-type 'validation-error)
              "[E]dit Command/Path [D]isable Watcher")
             ((eq error-type 'internal-error)
              "[C]lear Error [D]isable Watcher")
             (t "[C]lear")))))

(defun watcherrun-update-error-buffer (error-entry)
  "Update error buffer with ERROR-ENTRY."
  (let ((buf (get-buffer-create watcherrun-error-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (window (get-buffer-window buf))
            (pos (point)))
        
        ;; Enable watcherrun-error-mode if not already
        (unless (eq major-mode 'watcherrun-error-mode)
          (watcherrun-error-mode))
        
        ;; Insert header if empty buffer
        (when (= (buffer-size) 0)
          (insert "=== WatcherRun Error Log ===

"))
        
        ;; Insert new error at the top (after header)
        (goto-char (point-min))
        (forward-line 2)
        (insert (watcherrun-format-error-entry error-entry))
        
        ;; Restore position if viewing
        (when window
          (with-selected-window window
            (goto-char pos))))
      
      ;; Display buffer in a window if not already visible
      (unless (get-buffer-window buf)
        (display-buffer buf)))))

(defun watcherrun-show-error-notification (error-entry)
  "Show notification for ERROR-ENTRY using `message'."
  (let ((error-type (plist-get error-entry :error-type))
        (message-text (plist-get error-entry :message))
        (watcher-id (plist-get error-entry :watcher-id)))
    (message "WatcherRun %s: %s%s"
             (upcase (symbol-name error-type))
             message-text
             (if watcher-id (format " (Watcher: %s)" watcher-id) ""))))

;; Error buffer mode
(define-derived-mode watcherrun-error-mode special-mode "WatcherRun-Errors"
  "Major mode for displaying WatcherRun errors."
  :group 'watcherrun
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq truncate-lines t)
  
  ;; Font locking
  (setq font-lock-defaults
        '((
           ("\\[.*?\\]" . font-lock-constant-face)
           ("\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*?\\]" . font-lock-comment-face)
           ("COMMAND-ERROR" . compilation-error-face)
           ("FILE-ERROR" . compilation-error-face)
           ("LISP-ERROR" . compilation-error-face)
           ("PROCESS-ERROR" . compilation-error-face)
           ("VALIDATION-ERROR" . compilation-warning-face)
           ("INTERNAL-ERROR" . compilation-error-face)
           ("Watcher:.*$" . font-lock-function-name-face)
           ("Context:.*$" . font-lock-comment-face)
           ("Actions:.*$" . font-lock-keyword-face)
           ))))

;; Interactive commands for error buffer
(defun watcherrun-retry-command ()
  "Retry the command associated with the error at point."
  (interactive)
  (let ((error-entry (watcherrun-get-error-at-point))
        (watcher-id (watcherrun-get-watcher-id-at-point)))
    (when (and error-entry watcher-id)
      (message "Retrying command for watcher: %s" watcher-id)
      ;; Actual retry logic would call back to core-engine
      )))

(defun watcherrun-disable-watcher ()
  "Disable the watcher associated with the error at point."
  (interactive)
  (let ((watcher-id (watcherrun-get-watcher-id-at-point)))
    (when watcher-id
      (message "Disabling watcher: %s" watcher-id)
      (let ((watcher (watcherrun-get-watcher watcher-id)))
        (when watcher
          (setf (watcherrun-watcher-status watcher) 'paused)
          (watcherrun-store-watcher watcher))))))

(defun watcherrun-edit-command ()
  "Edit the command associated with the error at point."
  (interactive)
  (let ((watcher-id (watcherrun-get-watcher-id-at-point)))
    (when watcher-id
      (message "Editing command for watcher: %s" watcher-id)
      ;; This would open a prompt or buffer to edit the command
      )))

(defun watcherrun-update-paths ()
  "Update the paths associated with the error at point."
  (interactive)
  (let ((watcher-id (watcherrun-get-watcher-id-at-point)))
    (when watcher-id
      (message "Updating paths for watcher: %s" watcher-id)
      ;; This would open a prompt to update paths
      )))

(defun watcherrun-clear-error ()
  "Clear the error at point from the log."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((beg (line-beginning-position)))
      (while (not (looking-at "^\\[.*?\\].*$"))
        (forward-line -1))
      (let ((end (progn 
                   (forward-paragraph)
                   (point))))
        (delete-region beg end)
        (message "Error cleared")))))

(defun watcherrun-clear-all-errors ()
  "Clear all errors from the log."
  (interactive)
  (when (yes-or-no-p "Clear all errors? ")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "=== WatcherRun Error Log ===

")
      (setq watcherrun-error-log (make-ring 100))
      (message "All errors cleared"))))

(defun watcherrun-get-error-at-point ()
  "Get the error entry at point, or nil if none."
  (save-excursion
    (while (not (or (looking-at "^\\[.*?\\].*$") (bobp)))
      (forward-line -1))
    (when (looking-at "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*?\\]\\) \\([A-Z-]+\\)\\(.*\\)$")
      ;; This is simplified - in a real implementation, we'd look up the actual error entry
      (let ((timestamp (match-string 1))
            (error-type (match-string 2))
            (watcher-info (match-string 3)))
        (list :timestamp timestamp
              :error-type (intern (downcase error-type))
              :watcher-id (when (string-match "Watcher: \\(.*?\\)" watcher-info)
                            (match-string 1 watcher-info)))))))

(defun watcherrun-get-watcher-id-at-point ()
  "Get the watcher ID associated with the error at point."
  (let ((error-entry (watcherrun-get-error-at-point)))
    (when error-entry
      (plist-get error-entry :watcher-id))))

;; Key bindings for error buffer
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
  "Keymap for WatcherRun error buffer.")

(provide 'watcherrun-support)
;;; watcherrun-support.el ends here
