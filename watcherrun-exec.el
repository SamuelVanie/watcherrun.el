;;; watcherrun-exec.el --- Command execution layer for watcherrun -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; Author: WatcherRun Contributors
;; Keywords: convenience, tools, processes

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Command execution layer implementing async system command runner,
;; Lisp expression evaluator, and compilation buffer management.

;;; Code:

(require 'cl-lib)
(require 'compile)
;; (require 'watcherrun-support)
;; (require 'watcherrun-utils)

;; Process tracking for cleanup
(defvar watcherrun--active-processes (make-hash-table :test 'equal)
  "Hash table tracking active processes by watcher-id.")

;; Buffer tracking for compilation commands
(defvar watcherrun--compilation-buffers (make-hash-table :test 'equal)
  "Hash table tracking compilation buffers by watcher-id.")

;; Customizable variables
(defgroup watcherrun-exec nil
  "Execution settings for WatcherRun."
  :group 'watcherrun)

(defcustom watcherrun-clear-output-on-execution t
  "Whether to clear output buffer before new execution."
  :type 'boolean
  :group 'watcherrun-exec)

(defcustom watcherrun-compilation-commands '("make" "compile" "build" "test" "npm" "yarn" "cargo" "go")
  "List of commands that should use compilation mode."
  :type '(repeat string)
  :group 'watcherrun-exec)

;; Forward declarations
(declare-function watcherrun-log-debug "watcherrun-support")
(declare-function watcherrun-log-info "watcherrun-support")
(declare-function watcherrun-log-warning "watcherrun-support")
(declare-function watcherrun-report-error "watcherrun-support")
(declare-function watcherrun-watcher-id "watcherrun-support")

;; Define logging functions if not defined
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

;; Add function to expand placeholders for now
(defun watcherrun-expand-placeholders (command file-path)
  "Simple placeholder expansion for testing."
  (replace-regexp-in-string "{{file}}" file-path command))

;; Main execution dispatcher
(defun watcherrun-execute-system-command (command file-path watcher-id)
  "Execute system COMMAND asynchronously with proper error handling.
FILE-PATH provides context for placeholder expansion.
WATCHER-ID identifies the watcher for buffer naming and tracking."
  (let* ((expanded-command (watcherrun-expand-placeholders command file-path))
         (buffer-name (format "*watcherrun-output-%s*" watcher-id))
         (process-name (format "watcherrun-%s" watcher-id)))
    
    ;; Check if this is a compilation command
    (if (watcherrun-is-compilation-command-p expanded-command)
        (progn
          (watcherrun-execute-compilation-command expanded-command watcher-id)
          ;; Return nil for compilation commands since they don't return a process
          nil)
      (watcherrun-execute-regular-command expanded-command buffer-name process-name watcher-id))))

(defun watcherrun-is-compilation-command-p (command)
  "Determine if COMMAND should use compilation mode."
  (or (cl-some (lambda (cmd) (string-match-p (concat "\\b" cmd "\\b") command))
               watcherrun-compilation-commands)
      (string-match-p "\\b\\(npm\\|yarn\\|cargo\\|go\\)\\s-+\\(run\\|build\\|test\\)" command)
      (string-prefix-p "compile" command)))

(defun watcherrun-execute-regular-command (command buffer-name process-name watcher-id)
  "Execute regular system COMMAND in dedicated buffer.
BUFFER-NAME is the output buffer name.
PROCESS-NAME is the process identifier.
WATCHER-ID is used for tracking and cleanup."
  ;; Clean up any existing process for this watcher
  (watcherrun-cleanup-process watcher-id)
  
  ;; Create or get the output buffer
  (let ((output-buffer (get-buffer-create buffer-name)))
    (with-current-buffer output-buffer
      ;; Clear buffer if configured to do so
      (when watcherrun-clear-output-on-execution
        (erase-buffer))
      
      ;; Add timestamp header
      (goto-char (point-max))
      (insert (format "\n=== Execution started at %s ===\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Command: %s\n\n" command)))
    
    ;; Start the async process
    (condition-case err
        (let ((process (start-process-shell-command
                       process-name output-buffer command)))
          
          ;; Track the process
          (puthash watcher-id process watcherrun--active-processes)
          
          ;; Set up process sentinel
          (set-process-sentinel process
                               (lambda (proc event)
                                 (watcherrun-process-sentinel proc event watcher-id)))
          
          ;; Set up process filter for real-time output
          (set-process-filter process
                             (lambda (proc output)
                               (watcherrun-process-filter proc output)))
          
          ;; Display the buffer
          (display-buffer output-buffer)
          
          (watcherrun-log-info "Started process %s for watcher %s" process-name watcher-id)
          process)
      
      (error
       (watcherrun-report-error
        (format "Failed to start process for watcher %s: %s"
                watcher-id (error-message-string err)))
       nil))))

(defun watcherrun-execute-compilation-command (command watcher-id)
  "Execute compilation COMMAND with automatic buffer renaming.
WATCHER-ID is used for unique buffer naming and tracking."
  (let ((target-buffer-name (format "*compilation-watcher-%s*" watcher-id))
        (original-compilation-buffer nil))
    
    ;; Clean up any existing compilation buffer for this watcher
    (when-let ((old-buffer (gethash watcher-id watcherrun--compilation-buffers)))
      (when (buffer-live-p old-buffer)
        (kill-buffer old-buffer)))
    
    ;; Set up buffer rename hook
    (let ((rename-hook (lambda (proc)
                        (when (and (process-live-p proc)
                                  (string= (buffer-name (process-buffer proc)) "*compilation*"))
                          (with-current-buffer (process-buffer proc)
                            (rename-buffer target-buffer-name t)
                            (puthash watcher-id (current-buffer) watcherrun--compilation-buffers)
                            (setq original-compilation-buffer (current-buffer)))))))
      
      ;; Add the hook temporarily
      (add-hook 'compilation-start-hook rename-hook)
      
      ;; Execute the compilation
      (condition-case err
          (progn
            (compile command)
            (watcherrun-log-info "Started compilation for watcher %s: %s" watcher-id command))
        (error
         (watcherrun-report-error
          (format "Failed to start compilation for watcher %s: %s"
                  watcher-id (error-message-string err)))))
      
      ;; Clean up hook after brief delay
      (run-with-timer 1.0 nil
                     (lambda ()
                       (remove-hook 'compilation-start-hook rename-hook))))))

(defun watcherrun-process-sentinel (process event watcher-id)
  "Process sentinel for handling PROCESS completion.
EVENT describes what happened to the process.
WATCHER-ID identifies the associated watcher."
  (let ((status (process-status process))
        (exit-code (process-exit-status process))
        (buffer (process-buffer process)))
    
    ;; Log the event
    (watcherrun-log-debug "Process %s for watcher %s: %s" 
                         (process-name process) watcher-id (string-trim event))
    
    ;; Handle different process states
    (cond
     ((eq status 'exit)
      (if (= exit-code 0)
          (progn
            (watcherrun-log-info "Process completed successfully for watcher %s" watcher-id)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert (format "\n=== Execution completed successfully at %s ===\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")))))
        (progn
          (watcherrun-log-warning "Process failed for watcher %s with exit code %d" 
                                 watcher-id exit-code)
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert (format "\n=== Execution failed at %s (exit code: %d) ===\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S") exit-code))))))
     
     ((eq status 'signal)
      (watcherrun-log-warning "Process killed by signal for watcher %s: %s" 
                             watcher-id event)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (format "\n=== Execution terminated by signal at %s ===\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S")))))
     
     (t
      (watcherrun-log-debug "Process status change for watcher %s: %s" watcher-id event)))
    
    ;; Clean up finished process
    (when (memq status '(exit signal))
      (remhash watcher-id watcherrun--active-processes))))

(defun watcherrun-process-filter (process output)
  "Process filter for real-time OUTPUT display.
PROCESS is the process generating the output."
  (when (and (process-live-p process) (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (point-max))
        (insert output))
      ;; Auto-scroll if buffer is visible
      (when-let ((window (get-buffer-window (current-buffer))))
        (with-selected-window window
          (goto-char (point-max)))))))

(defun watcherrun-cleanup-process (watcher-id)
  "Clean up any active process for WATCHER-ID."
  (when-let ((process (gethash watcher-id watcherrun--active-processes)))
    (when (process-live-p process)
      (watcherrun-log-debug "Terminating existing process for watcher %s" watcher-id)
      (kill-process process))
    (remhash watcher-id watcherrun--active-processes)))

(defun watcherrun-cleanup-compilation-buffer (watcher-id)
  "Clean up compilation buffer for WATCHER-ID."
  (when-let ((buffer (gethash watcher-id watcherrun--compilation-buffers)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (remhash watcher-id watcherrun--compilation-buffers)))

(defun watcherrun-cleanup-watcher-resources (watcher-id)
  "Clean up all resources (processes and buffers) for WATCHER-ID."
  (watcherrun-cleanup-process watcher-id)
  (watcherrun-cleanup-compilation-buffer watcher-id)
  
  ;; Clean up regular output buffer
  (let ((buffer-name (format "*watcherrun-output-%s*" watcher-id)))
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))))

(defun watcherrun-list-active-processes ()
  "Return list of active processes for debugging."
  (let (processes)
    (maphash (lambda (watcher-id process)
               (when (process-live-p process)
                 (push (list watcher-id 
                            (process-name process)
                            (process-status process))
                       processes)))
             watcherrun--active-processes)
    processes))

(defun watcherrun-kill-all-processes ()
  "Kill all active processes. Useful for emergency cleanup."
  (maphash (lambda (watcher-id process)
             (when (process-live-p process)
               (watcherrun-log-info "Killing process for watcher %s" watcher-id)
               (kill-process process)))
           watcherrun--active-processes)
  (clrhash watcherrun--active-processes))

(provide 'watcherrun-exec)

;;; watcherrun-exec.el ends here
