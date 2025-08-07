;;; watcherrun-ui.el --- User interface for WatcherRun -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; Author: WatcherRun Contributors
;; Keywords: convenience, tools, processes

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Menu bar interface and interactive commands for WatcherRun.

;;; Code:

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
  "Add a new file watcher interactively."
  (interactive)
  (let* ((path (read-file-name "Path to watch: " default-directory nil t))
         (normalized-path (watcherrun-normalize-path path))
         (is-dir (file-directory-p normalized-path))
         (recursive (if is-dir
                        (y-or-n-p "Watch directory recursively? ")
                      nil))
         (command-type-char (read-char-choice
                             "Command type: (s)ystem or (l)isp? "
                             '(?s ?l)))
         (command-type (if (eq command-type-char ?s) 'system 'lisp))
         (command (read-string "Command to execute: ")))
    (condition-case err
        (let ((watcher-id (watcherrun-add-watcher
                           (list normalized-path)
                           command
                           command-type
                           recursive)))
          (message "Added watcher %s for %s" watcher-id normalized-path))
      (error (message "Error adding watcher: %s" (error-message-string err))))))

;;;###autoload
(defun watcherrun-delete-watcher-menu ()
  "Delete a watcher by prompting for ID."
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
(defun watcherrun-modify-watcher-menu ()
  "Modify a watcher's command by prompting for ID and new command."
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
(defun watcherrun-show-error-buffer ()
  "Show the WatcherRun error buffer."
  (interactive)
  (let ((error-buffer (get-buffer "*WatcherRun Errors*")))
    (if error-buffer
        (display-buffer error-buffer)
      (message "No error buffer found"))))

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
    ["Delete Watcher" watcherrun-delete-watcher-menu
     :keys "C-c w d"
     :enable (> (watcherrun-get-watcher-count) 0)
     :help "Delete an existing watcher"]
    ["Modify Watcher" watcherrun-modify-watcher-menu
     :keys "C-c w m"
     :enable (> (watcherrun-get-watcher-count) 0)
     :help "Modify watcher command"]
    "---"
    ["Show Error Buffer" watcherrun-show-error-buffer
     :keys "C-c w e"
     :help "Show error log buffer"]))

;;; Keyboard shortcuts

(defvar watcherrun-mode-map nil
  "Keymap for WatcherRun commands.")

(unless watcherrun-mode-map
  (setq watcherrun-mode-map (make-sparse-keymap))
  (define-key watcherrun-mode-map (kbd "C-c w l") 'watcherrun-list-watchers-menu)
  (define-key watcherrun-mode-map (kbd "C-c w a") 'watcherrun-add-watcher-interactive)
  (define-key watcherrun-mode-map (kbd "C-c w d") 'watcherrun-delete-watcher-menu)
  (define-key watcherrun-mode-map (kbd "C-c w m") 'watcherrun-modify-watcher-menu)
  (define-key watcherrun-mode-map (kbd "C-c w e") 'watcherrun-show-error-buffer))

;;; Menu state management

(defun watcherrun-update-menu-state ()
  "Update menu item states based on current watcher count."
  (easy-menu-add-item nil '("Tools") watcherrun-menu "Search Files (Grep)..."))

;;; Initialization

(add-hook 'after-init-hook 'watcherrun-update-menu-state)

(provide 'watcherrun-ui)
;;; watcherrun-ui.el ends here
