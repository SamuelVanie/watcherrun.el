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
;; - Native Emacs Integration: Uses built-in file-notify APIs, no external dependencies
;; - Flexible Command Execution: Run system commands or Emacs Lisp expressions
;; - Multiple Watchers: Manage concurrent file watchers with dedicated compilation buffers
;; - Smart Menu Integration: Menu bar interface with keyboard shortcuts (C-c w [l|a|d|m|e])
;; - Dired Integration: Add watchers directly from Dired with 'W' key
;; - Cross-Platform: Works on Linux (inotify), macOS (fsevents), and Windows (w32notify)
;; - Template Variables: Use {{file}}, {{basename}}, {{dirname}} in commands
;; - Error Handling: Dedicated error buffer with structured logging
;; - Session Storage: No configuration files - clean and simple
;;
;; Quick Start:
;; 1. Enable WatcherRun: M-x watcherrun-mode
;; 2. Add a watcher: C-c w a or use the WatcherRun menu
;; 3. Edit your file and watch the command execute automatically
;;
;; Template Variables:
;; - {{file}} - Full path to the changed file
;; - {{basename}} - Filename without extension
;; - {{dirname}} - Directory containing the file
;; - {{extension}} - File extension (without dot)
;;
;; See README.md for comprehensive documentation.

;;; Code:

(require 'easymenu)
(require 'watcherrun-support)
(require 'watcherrun-core)
(require 'watcherrun-ui)

(defgroup watcherrun nil
  "Watch files and run commands when they change."
  :group 'convenience
  :prefix "watcherrun-")

;; Customization options

(defcustom watcherrun-show-error-notifications t
  "Whether to show desktop notifications when errors occur.

When enabled, critical errors like command failures will trigger
desktop notifications to alert the user even when Emacs is not
in the foreground."
  :type 'boolean
  :group 'watcherrun)

;;; Package activation/deactivation

;;;###autoload
(define-minor-mode watcherrun-mode
  "Minor mode for WatcherRun file watching functionality."
  :global t
  :lighter " WatcherRun"
  :keymap watcherrun-mode-map
  (if watcherrun-mode
      (watcherrun--enable)
    (watcherrun--disable)))

(defun watcherrun--enable ()
  "Enable WatcherRun functionality."
  (watcherrun-initialize-session)
  (easy-menu-add-item global-map '(menu-bar tools) watcherrun-menu)
  (watcherrun-setup-dired-integration)
  (message "WatcherRun enabled"))

(defun watcherrun--disable ()
  "Disable WatcherRun and clean up resources."
  (watcherrun-cleanup-session)
  (easy-menu-remove-item global-map '(menu-bar tools) "WatcherRun")
  (message "WatcherRun disabled"))

(provide 'watcherrun)
;;; watcherrun.el ends here
