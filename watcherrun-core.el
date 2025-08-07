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
;; Command type detection
(defun watcherrun-determine-command-type (command)
  "Intelligently determine if COMMAND is system or lisp."
  (pcase command
    ((pred (lambda (cmd) (string-match-p "^(" cmd))) 'lisp)
    ((pred (lambda (cmd) (string-match-p "compile\\|make\\|npm" cmd))) 'system)
    (_ 'system)))

;; Command validation functions
(defun watcherrun-validate-system-command (command)
  "Validate COMMAND as a system command."
  (let* ((parsed (watcherrun-parse-command command))
         (executable (plist-get parsed :executable)))
    (unless (or (executable-find executable)
                (member executable '("echo" "cd" "pwd")))
      (warn "Executable '%s' not found in PATH" executable))
    t))

(defun watcherrun-validate-lisp-command (command)
  "Validate COMMAND as a Lisp expression."
  (condition-case err
      (progn
        ;; Try to read the expression
        (read-from-string command)
        ;; Check for dangerous functions
        (when (string-match-p "delete-file\\|delete-directory\\|shell-command" command)
          (warn "Potentially dangerous Lisp command: %s" command))
        t)
    (error
     (error "Invalid Lisp syntax in command: %s" (error-message-string err)))))

(defun watcherrun-validate-command (command command-type)
  "Validate COMMAND based on COMMAND-TYPE."
  (pcase command-type
    ('system (watcherrun-validate-system-command command))
    ('lisp (watcherrun-validate-lisp-command command))
    (_ (error "Unknown command type: %s" command-type))))

;; Main command executor
(defun watcherrun-execute-command (watcher file-path)
  "Execute command for WATCHER with FILE-PATH context."
  (when (and watcher file-path)
    (let* ((command (watcherrun-watcher-command watcher))
           (command-type (watcherrun-watcher-command-type watcher))
           (expanded-command (watcherrun-expand-placeholders command file-path)))
      
      ;; Update execution statistics
      (setf (watcherrun-watcher-last-executed watcher) (current-time))
      (setf (watcherrun-watcher-execution-count watcher)
            (1+ (or (watcherrun-watcher-execution-count watcher) 0)))
      
      ;; Validate and execute command
      (condition-case err
          (progn
            (watcherrun-validate-command expanded-command command-type)
            (pcase command-type
              ('system (watcherrun--execute-system-command expanded-command watcher))
              ('lisp (watcherrun--execute-lisp-command expanded-command watcher))
              (_ (error "Invalid command type: %s" command-type)))
            (setf (watcherrun-watcher-status watcher) 'active))
        (error
         (watcherrun-report-error
          (format "Command execution failed for watcher %s: %s"
                  (watcherrun-watcher-id watcher)
                  (error-message-string err)))
         (setf (watcherrun-watcher-status watcher) 'error))))))

;; System command execution
(defun watcherrun--execute-system-command (command watcher)
  "Execute system COMMAND for WATCHER."
  (let ((buffer-name (format "*WatcherRun-%s*" (watcherrun-watcher-id watcher))))
    (watcherrun-log-info "Executing system command: %s" command)
    (async-shell-command command buffer-name)))

;; Lisp command execution
(defun watcherrun--execute-lisp-command (command watcher)
  "Execute Lisp COMMAND for WATCHER."
  (watcherrun-log-info "Executing Lisp command: %s" command)
  (condition-case err
      (eval (read-from-string command) t)
    (error
     (error "Lisp execution error: %s" (error-message-string err)))))

;; Watcher registry and path index
(defvar watcherrun--path-index (make-hash-table :test 'equal)
  "Hash table mapping file paths to watcher IDs for conflict detection.")

;; Counter for unique ID generation
(defvar watcherrun--watcher-counter 0
  "Counter for generating unique watcher IDs.")

;; Watcher manager functions

(defun watcherrun-find-watcher-by-id (watcher-id)
  "Find watcher by WATCHER-ID, return watcher structure or nil."
  (gethash watcher-id watcherrun-watchers))

(defun watcherrun-find-watcher-by-descriptor (descriptor)
  "Find watcher by file notification DESCRIPTOR."
  (cl-loop for watcher being the hash-values of watcherrun-watchers
           when (member descriptor (watcherrun-watcher-file-descriptor watcher))
           return watcher))

(defun watcherrun--generate-watcher-id ()
  "Generate a unique watcher ID."
  (let ((timestamp (format-time-string "%Y%m%d%H%M%S" (current-time)))
        (counter (cl-incf watcherrun--watcher-counter)))
    (format "watcher-%s-%03d" timestamp counter)))

(defun watcherrun--check-path-conflicts (paths)
  "Check if any of PATHS are already being watched.
Returns list of conflicting watcher IDs or nil if no conflicts."
  (let (conflicts)
    (dolist (path paths)
      (let ((normalized-path (watcherrun-normalize-path path)))
        (when-let ((existing-id (gethash normalized-path watcherrun--path-index)))
          (push existing-id conflicts))))
    conflicts))

(defun watcherrun--register-path-index (paths watcher-id)
  "Register PATHS for WATCHER-ID in the path index."
  (dolist (path paths)
    (let ((normalized-path (watcherrun-normalize-path path)))
      (puthash normalized-path watcher-id watcherrun--path-index))))

(defun watcherrun--unregister-path-index (paths)
  "Remove PATHS from the path index."
  (dolist (path paths)
    (let ((normalized-path (watcherrun-normalize-path path)))
      (remhash normalized-path watcherrun--path-index))))

(defun watcherrun-add-watcher (paths command command-type recursive)
  "Add new watcher with validation and registration.
PATHS is a list of file/directory paths to watch.
COMMAND is the command string to execute.
COMMAND-TYPE is either 'system or 'lisp.
RECURSIVE indicates whether to watch directories recursively.
Returns watcher ID on success, signals error on failure."
  ;; Input validation
  (unless (and paths (listp paths) (not (null paths)))
    (error "Paths must be a non-empty list"))
  (unless (stringp command)
    (error "Command must be a string"))
  (unless (memq command-type '(system lisp))
    (error "Command type must be 'system or 'lisp"))
  
  ;; Validate all paths exist
  (dolist (path paths)
    (unless (file-exists-p path)
      (error "Path does not exist: %s" path)))
  
  ;; Check for path conflicts
  (when-let ((conflicts (watcherrun--check-path-conflicts paths)))
    (error "Paths already being watched by: %s" (string-join conflicts ", ")))
  
  ;; Validate command
  (condition-case err
      (watcherrun-validate-command command command-type)
    (error (error "Command validation failed: %s" (error-message-string err))))
  
  ;; Generate unique ID
  (let ((watcher-id (watcherrun--generate-watcher-id)))
    
    ;; Set up file notification
    (let ((descriptors (watcherrun-setup-file-watcher 
                        paths recursive #'watcherrun-handle-change-event)))
      
      ;; Create watcher structure
      (let ((watcher (make-watcherrun-watcher
                      :id watcher-id
                      :paths paths
                      :command command
                      :command-type command-type
                      :recursive recursive
                      :file-descriptor descriptors
                      :last-executed nil
                      :execution-count 0
                      :status 'active)))
        
        ;; Store in registry and path index
        (puthash watcher-id watcher watcherrun-watchers)
        (watcherrun--register-path-index paths watcher-id)
        
        (watcherrun-log-info "Added watcher %s for paths: %S" watcher-id paths)
        watcher-id))))

(defun watcherrun-remove-watcher (watcher-id)
  "Remove watcher by WATCHER-ID and clean up all resources.
Returns t on success, signals error if watcher not found."
  (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
    (unless watcher
      (error "Watcher not found: %s" watcher-id))
    
    ;; Stop file notification
    (when-let ((descriptors (watcherrun-watcher-file-descriptor watcher)))
      (watcherrun-cleanup-file-watcher descriptors))
    
    ;; Clean up path index
    (watcherrun--unregister-path-index (watcherrun-watcher-paths watcher))
    
    ;; Remove from registry
    (remhash watcher-id watcherrun-watchers)
    
    (watcherrun-log-info "Removed watcher %s" watcher-id)
    t))

(defun watcherrun-modify-watcher (watcher-id new-command &optional new-command-type)
  "Modify watcher command for WATCHER-ID.
NEW-COMMAND replaces the existing command.
NEW-COMMAND-TYPE optionally changes the command type.
Preserves all other watcher settings including ID."
  (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
    (unless watcher
      (error "Watcher not found: %s" watcher-id))
    
    ;; Determine command type
    (let ((command-type (or new-command-type 
                            (watcherrun-watcher-command-type watcher))))
      
      ;; Validate new command
      (condition-case err
          (watcherrun-validate-command new-command command-type)
        (error (error "Command validation failed: %s" (error-message-string err))))
      
      ;; Update watcher
      (setf (watcherrun-watcher-command watcher) new-command)
      (setf (watcherrun-watcher-command-type watcher) command-type)
      (setf (watcherrun-watcher-status watcher) 'active)
      
      (watcherrun-log-info "Modified watcher %s command to: %s" watcher-id new-command)
      t)))

(defun watcherrun-list-watchers ()
  "Return a formatted list of all active watchers for display.
Returns list of strings suitable for UI presentation."
  (let (watcher-list)
    (maphash (lambda (id watcher)
               (push (format "[%s] %s -> %s (%s)"
                             id
                             (string-join (watcherrun-watcher-paths watcher) ", ")
                             (watcherrun-watcher-command watcher)
                             (watcherrun-watcher-command-type watcher))
                     watcher-list))
             watcherrun-watchers)
    (sort watcher-list #'string<)))

(defun watcherrun-get-watcher-count ()
  "Return the number of active watchers."
  (hash-table-count watcherrun-watchers))

(defun watcherrun-pause-watcher (watcher-id)
  "Pause watcher by WATCHER-ID, stopping file notifications."
  (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
    (unless watcher
      (error "Watcher not found: %s" watcher-id))
    
    ;; Stop file notifications but keep watcher in registry
    (when-let ((descriptors (watcherrun-watcher-file-descriptor watcher)))
      (watcherrun-cleanup-file-watcher descriptors)
      (setf (watcherrun-watcher-file-descriptor watcher) nil))
    
    (setf (watcherrun-watcher-status watcher) 'paused)
    (watcherrun-log-info "Paused watcher %s" watcher-id)
    t))

(defun watcherrun-resume-watcher (watcher-id)
  "Resume paused watcher by WATCHER-ID."
  (let ((watcher (watcherrun-find-watcher-by-id watcher-id)))
    (unless watcher
      (error "Watcher not found: %s" watcher-id))
    
    (unless (eq (watcherrun-watcher-status watcher) 'paused)
      (error "Watcher %s is not paused" watcher-id))
    
    ;; Re-establish file notifications
    (let ((descriptors (watcherrun-setup-file-watcher
                        (watcherrun-watcher-paths watcher)
                        (watcherrun-watcher-recursive watcher)
                        #'watcherrun-handle-change-event)))
      (setf (watcherrun-watcher-file-descriptor watcher) descriptors)
      (setf (watcherrun-watcher-status watcher) 'active)
      
      (watcherrun-log-info "Resumed watcher %s" watcher-id)
      t)))

(defun watcherrun-cleanup-all-watchers ()
  "Clean up all watchers and reset the registry.
Useful for session end or emergency cleanup."
  (maphash (lambda (id watcher)
             (when-let ((descriptors (watcherrun-watcher-file-descriptor watcher)))
               (watcherrun-cleanup-file-watcher descriptors)))
           watcherrun-watchers)
  
  ;; Clear all data structures
  (clrhash watcherrun-watchers)
  (clrhash watcherrun--path-index)
  (setq watcherrun--watcher-counter 0)
  
  (watcherrun-log-info "Cleaned up all watchers")
  t)

;;; watcherrun-core.el ends here
