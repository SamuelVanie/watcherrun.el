;;; -*- lexical-binding: t; -*-
;;; watcherrun.el --- Watch files and run commands when they change -*- lexical-binding: t; -*-

;; Author: The WatcherRun Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, files, processes
;; URL: https://github.com/username/watcherrun.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; WatcherRun.el is an Emacs package that lets you run system commands or Emacs
;; Lisp expressions automatically when files change.  It uses Emacs' native
;; file-notify APIs (no external daemons) and integrates with Dired and an
;; Emacs menu for a smooth workflow.
;;
;; Main features:
;; - Run commands when files change (shell commands or Emacs Lisp)
;; - Uses Emacs' built-in file-notify APIs (no external dependencies)
;; - Multiple concurrent watchers with separate compilation buffers
;; - Dired integration for easy setup
;; - Comprehensive error handling
;;
;; See README.md for more information.

;;; Code:

(defgroup watcherrun nil
  "Watch files and run commands when they change."
  :group 'convenience
  :prefix "watcherrun-")

;; Customization options

(defcustom watcherrun-show-error-notifications t
  "If non-nil, show notifications when errors occur."
  :type 'boolean
  :group 'watcherrun)

;; Core variables will be defined here

;; Declare autoloads for other files
;; (These will be implemented in other modules)

;;;###autoload
(defun watcherrun-add-watcher ()
  "Add a new file watcher."
  (interactive)
  (message "watcherrun-add-watcher not yet implemented"))

;;;###autoload
(defun watcherrun-list-watchers ()
  "List all active watchers."
  (interactive)
  (message "watcherrun-list-watchers not yet implemented"))

;;;###autoload
(defun watcherrun-remove-watcher ()
  "Remove a watcher."
  (interactive)
  (message "watcherrun-remove-watcher not yet implemented"))

;;;###autoload
(defun watcherrun-pause-watcher ()
  "Pause a watcher."
  (interactive)
  (message "watcherrun-pause-watcher not yet implemented"))

;;;###autoload
(defun watcherrun-resume-watcher ()
  "Resume a paused watcher."
  (interactive)
  (message "watcherrun-resume-watcher not yet implemented"))

;; Menu definition
(defvar watcherrun-menu nil
  "Menu for WatcherRun.")

(easy-menu-define watcherrun-menu nil
  "Menu for WatcherRun."
  '("WatcherRun"
    ["Add Watcher" watcherrun-add-watcher t]
    ["List Watchers" watcherrun-list-watchers t]
    ["Remove Watcher" watcherrun-remove-watcher t]
    "--"
    ["Pause Watcher" watcherrun-pause-watcher t]
    ["Resume Watcher" watcherrun-resume-watcher t]
    "--"
    ["Show Error Log" (display-buffer "*WatcherRun Errors*") t]))

;; Load menu on package load
(easy-menu-add-item nil '("Tools") watcherrun-menu "Search Files (Grep)...")

(provide 'watcherrun)
;;; watcherrun.el ends here
