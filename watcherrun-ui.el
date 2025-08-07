;;; watcherrun-ui.el --- User interface for WatcherRun -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; Author: Samuel Michael VANIE
;; Keywords: convenience, tools, processes

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Menu bar interface and interactive commands for WatcherRun.

;;; Code:

(require 'dired)

(unless (featurep 'easy-menu)
  (require 'easymenu nil t))
(unless (featurep 'easymenu)
  (defmacro easy-menu-define (symbol maps doc menu)
    "Stub for easy-menu-define when not available."
    `(defvar ,symbol ',menu ,doc)))
(require 'watcherrun-core)
(require 'watcherrun-utils)
(require 'watcherrun-support)

;;; Menu command functions

;;;###autoload
(defun watcherrun-list-watchers-menu ()
  "List all active watchers in a buffer."
  (interactive)
  (let ((watchers-list (watcherrun-list-watchers)))
    (if watchers-list
        (with-output-to-temp-buffer "*WatcherRun Watchers*"
          (princ "Active Watchers:\n\n")
          (dolist (watcher watchers-list)
            (princ (format "%s\n" watcher))))
      (message "No active watchers"))))

;;;###autoload
(defun watcherrun-add-watcher-interactive ()
  "Interactively add a file watcher with prompts and completion.

Prompts the user for:
- File or directory path (with file completion)
- Command to execute when files change
- Command type (system or lisp)
- Recursive watching option for directories

Uses `read-file-name' for path completion and validates the path exists.
Template variables like {{file}}, {{basename}} can be used in commands."
  (interactive)
  (let ((path (read-file-name "Watch file or directory: " 
                              nil nil t)))
    
    ;; Validate path exists and is accessible
    (condition-case error
        (progn
          (watcherrun-validate-path path)
          
          ;; Get command type and recursion settings
          (let ((command-type (watcherrun--prompt-command-type))
                (recursive (when (file-directory-p path)
                            (watcherrun--prompt-recursion-for-directories (list path)))))
            
            ;; Get command string
            (let ((command (read-string "Enter command to execute: ")))
              
              ;; Create the watcher
              (let ((watcher-id (watcherrun-add-watcher (list path) command command-type recursive)))
                (message "Added watcher %s for %s" watcher-id path)))))
      
      (error
       (message "Error: %s" (error-message-string error))))))

;;;###autoload
(defun watcherrun-delete-watcher-interactive ()
  "Interactively select and delete a watcher.

Displays a list of all active watchers for selection using completing-read.
Shows watcher details to help with identification."
  (interactive)
  (let ((watchers-list (watcherrun-list-watchers)))
    (if watchers-list
        (let* ((watcher-choices (mapcar (lambda (w)
                                          (cons w (substring w 1 (string-match "]" w))))
                                        watchers-list))
               (selected (completing-read "Delete watcher: " watcher-choices nil t))
               (watcher-id (cdr (assoc selected watcher-choices))))
          (condition-case err
              (progn
                (watcherrun-remove-watcher watcher-id)
                (message "Deleted watcher %s" watcher-id))
            (error (message "Error deleting watcher: %s" (error-message-string err)))))
      (message "No watchers to delete"))))

;;;###autoload
(defun watcherrun-modify-watcher-interactive ()
  "Interactively select and modify a watcher's configuration.

Allows modification of:
- Command string
- Command type (system or lisp)

Uses completing-read to select from active watchers."
  (interactive)
  (let ((watchers-list (watcherrun-list-watchers)))
    (if watchers-list
        (let* ((watcher-choices (mapcar (lambda (w)
                                          (cons w (substring w 1 (string-match "]" w))))
                                        watchers-list))
               (selected (completing-read "Modify watcher: " watcher-choices nil t))
               (watcher-id (cdr (assoc selected watcher-choices)))
               (new-command (read-string "New command: ")))
          (condition-case err
              (progn
                (watcherrun-modify-watcher watcher-id new-command)
                (message "Modified watcher %s" watcher-id))
            (error (message "Error modifying watcher: %s" (error-message-string err)))))
      (message "No watchers to modify"))))

;;;###autoload
(defun watcherrun-show-errors ()
  "Display the WatcherRun error buffer.

Shows all logged errors, warnings, and debug information.
The error buffer includes timestamps and actionable error details."
  (interactive)
  (let ((error-buffer (get-buffer-create "*WatcherRun Errors*")))
    (with-current-buffer error-buffer
      ;; Ensure error mode is enabled
      (unless (eq major-mode 'watcherrun-error-mode)
        (watcherrun-error-mode))
      ;; Add header if buffer is empty
      (when (= (buffer-size) 0)
        (let ((inhibit-read-only t))
          (insert "=== WatcherRun Error Log ===\n\n")
          (insert "No errors logged yet.\n\n")
          (insert "Errors will appear here when watchers encounter issues.\n"))))
    (display-buffer error-buffer)))

;;; Menu definition

(defvar watcherrun-menu nil
  "Menu for WatcherRun.")

(easy-menu-define watcherrun-menu nil
  "WatcherRun menu for managing file watchers."
  '("WatcherRun"
    ["List All Watchers" watcherrun-list-watchers-menu
     :keys "C-c w l"
     :help "Show all active watchers"]
    ["Add New Watcher" watcherrun-add-watcher-interactive
     :keys "C-c w a"
     :help "Add a new file watcher"]
    "---"
    ["Delete Watcher" watcherrun-delete-watcher-interactive
     :keys "C-c w d"
     :enable (> (watcherrun-get-watcher-count) 0)
     :help "Delete an existing watcher"]
    ["Modify Watcher" watcherrun-modify-watcher-interactive
     :keys "C-c w m"
     :enable (> (watcherrun-get-watcher-count) 0)
     :help "Modify watcher command"]
    "---"
    ["Show Errors" watcherrun-show-errors
     :keys "C-c w e"
     :help "Show error log buffer"]))

;;; Keyboard shortcuts

(defvar watcherrun-mode-map nil
  "Keymap for WatcherRun commands.")

(unless watcherrun-mode-map
  (setq watcherrun-mode-map (make-sparse-keymap))
  (define-key watcherrun-mode-map (kbd "C-c w l") 'watcherrun-list-watchers-menu)
  (define-key watcherrun-mode-map (kbd "C-c w a") 'watcherrun-add-watcher-interactive)
  (define-key watcherrun-mode-map (kbd "C-c w d") 'watcherrun-delete-watcher-interactive)
  (define-key watcherrun-mode-map (kbd "C-c w m") 'watcherrun-modify-watcher-interactive)
  (define-key watcherrun-mode-map (kbd "C-c w e") 'watcherrun-show-errors))

;;; Menu state management

(defun watcherrun-update-menu-state ()
  "Update menu item states based on current watcher count."
  (easy-menu-add-item nil '("Tools") watcherrun-menu "Search Files (Grep)..."))

;;; Initialization

;;; Dired integration functions

;;;###autoload
(defun watcherrun--prompt-command-type ()
  "Prompt user for command type with single character input."
  (let ((choice (read-char-choice 
                 "Execute (s)ystem command or (l)isp expression? " 
                 '(?s ?l))))
    (pcase choice
      (?s 'system)
      (?l 'lisp))))

(defun watcherrun--prompt-recursion-for-directories (paths)
  "Prompt for recursion setting if directories are included."
  (when (seq-some #'file-directory-p paths)
    (let ((choice (read-char-choice
                   "Watch directories (i)mmediately or (r)ecursively? "
                   '(?i ?r))))
      (pcase choice
        (?i nil)
        (?r t)))))

(defun watcherrun--create-watchers-for-paths (paths command command-type recursive)
  "Create watchers for PATHS with COMMAND of COMMAND-TYPE and RECURSIVE setting."
  (let (successful-ids failed-paths)
    (dolist (path paths)
      (condition-case err
          (let ((watcher-id (watcherrun-add-watcher
                             (list path)
                             command
                             command-type
                             recursive)))
            (push watcher-id successful-ids)
            (message "Added watcher %s for %s" watcher-id path))
        (error 
         (push (cons path (error-message-string err)) failed-paths)
         (message "Error creating watcher for %s: %s" 
                  path (error-message-string err)))))
    
    ;; Report summary
    (if successful-ids
        (message "Created %d watcher(s) successfully"
                 (length successful-ids))
      (message "No watchers created"))
    
    (when failed-paths
      (message "Failed to create watchers for: %s"
               (mapconcat (lambda (fp) (car fp)) failed-paths ", ")))
    
    successful-ids))

;;;###autoload
(defun watcherrun-dired-add-watcher ()
  "Add watcher for marked files in Dired."
  (interactive)
  (let ((marked-files (dired-get-marked-files))
        (all-files (dired-get-marked-files nil nil)))
    ;; Check if any files are actually marked (more than just current file)
    (if (= (length marked-files) (length all-files))
        (message "No files marked. Use 'm' to mark files first.")
      
      ;; Get command type
      (let ((command-type (watcherrun--prompt-command-type))
            (recursive (watcherrun--prompt-recursion-for-directories marked-files)))
        
        ;; Get command string
        (let ((command (read-string "Enter command to execute: ")))
          
          ;; Validate and create watchers
          (watcherrun--create-watchers-for-paths marked-files command command-type recursive))))))

;;; Dired keybinding
(defun watcherrun-setup-dired-integration ()
  "Set up Dired integration for WatcherRun."
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "W") 'watcherrun-dired-add-watcher)))

(add-hook 'after-init-hook 'watcherrun-update-menu-state)
(add-hook 'after-init-hook 'watcherrun-setup-dired-integration)

(provide 'watcherrun-ui)
;;; watcherrun-ui.el ends here
