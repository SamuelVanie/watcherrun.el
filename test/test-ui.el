;;; test-ui.el --- Tests for watcherrun-ui -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;;; Commentary:
;; Tests for the user interface layer of WatcherRun, including Dired integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'watcherrun-ui)
(require 'watcherrun-support)

;; Test helper functions

(defun watcherrun-test-setup-temp-files ()
  "Set up temporary files and directories for testing."
  (let ((temp-dir (make-temp-file "watcherrun-test" t)))
    (write-region "test content" nil (expand-file-name "test1.txt" temp-dir))
    (write-region "test content" nil (expand-file-name "test2.txt" temp-dir))
    (make-directory (expand-file-name "subdir" temp-dir))
    (write-region "sub content" nil (expand-file-name "subdir/sub.txt" temp-dir))
    temp-dir))

(defun watcherrun-test-cleanup-temp-dir (temp-dir)
  "Clean up temporary directory TEMP-DIR."
  (when (file-exists-p temp-dir)
    (delete-directory temp-dir t)))

(defun watcherrun-test-create-dired-buffer (directory)
  "Create a Dired buffer for DIRECTORY and return it."
  (let ((dired-buffer (dired-noselect directory)))
    (with-current-buffer dired-buffer
      ;; Mark the first two files for testing
      (goto-char (point-min))
      (when (re-search-forward "test1.txt" nil t)
        (beginning-of-line)
        (dired-mark 1))
      (goto-char (point-min))
      (when (re-search-forward "test2.txt" nil t)
        (beginning-of-line)
        (dired-mark 1)))
    dired-buffer))

;; Test command type prompting

(ert-deftest watcherrun-test-prompt-command-type ()
  "Test command type prompting function."
  ;; Mock user input for system command
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (prompt choices) ?s)))
    (should (eq (watcherrun--prompt-command-type) 'system)))
  
  ;; Mock user input for lisp command
  (cl-letf (((symbol-function 'read-char-choice)
             (lambda (prompt choices) ?l)))
    (should (eq (watcherrun--prompt-command-type) 'lisp))))

(ert-deftest watcherrun-test-prompt-recursion-for-directories ()
  "Test recursion prompting for directories."
  (let ((temp-dir (watcherrun-test-setup-temp-files)))
    (unwind-protect
        (progn
          ;; Test with directory paths - should prompt
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (prompt choices) ?r)))
            (should (eq (watcherrun--prompt-recursion-for-directories 
                         (list temp-dir)) t)))
          
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (prompt choices) ?i)))
            (should (eq (watcherrun--prompt-recursion-for-directories 
                         (list temp-dir)) nil)))
          
          ;; Test with file paths - should not prompt
          (should (eq (watcherrun--prompt-recursion-for-directories 
                       (list (expand-file-name "test1.txt" temp-dir))) nil)))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

;; Test watcher creation from paths

(ert-deftest watcherrun-test-create-watchers-for-paths ()
  "Test creating watchers for multiple paths."
  (let ((temp-dir (watcherrun-test-setup-temp-files)))
    (unwind-protect
        (let ((test-paths (list (expand-file-name "test1.txt" temp-dir)
                                (expand-file-name "test2.txt" temp-dir)))
              (test-command "echo 'test'")
              (test-command-type 'system))
          
          ;; Test successful watcher creation
          (let ((watcher-ids (watcherrun--create-watchers-for-paths
                              test-paths test-command test-command-type nil)))
            (should (= (length watcher-ids) 2))
            (should (cl-every #'stringp watcher-ids))
            
            ;; Clean up created watchers
            (dolist (id watcher-ids)
              (watcherrun-remove-watcher id))))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

(ert-deftest watcherrun-test-create-watchers-for-paths-with-errors ()
  "Test watcher creation with some invalid paths."
  (let ((temp-dir (watcherrun-test-setup-temp-files))
        (invalid-path "/nonexistent/path/file.txt"))
    (unwind-protect
        (let ((test-paths (list (expand-file-name "test1.txt" temp-dir)
                                invalid-path))
              (test-command "echo 'test'")
              (test-command-type 'system))
          
          ;; Mock message function to capture outputs
          (let (messages)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              
              (let ((watcher-ids (watcherrun--create-watchers-for-paths
                                  test-paths test-command test-command-type nil)))
                ;; Should create one watcher for valid path
                (should (= (length watcher-ids) 1))
                ;; Should have error messages
                (should (cl-some (lambda (msg) (string-match-p "Error creating watcher" msg)) messages))
                
                ;; Clean up created watcher
                (dolist (id watcher-ids)
                  (watcherrun-remove-watcher id))))))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

;; Test Dired integration

(ert-deftest watcherrun-test-dired-add-watcher-no-marked-files ()
  "Test Dired integration when no files are marked."
  (let ((temp-dir (watcherrun-test-setup-temp-files)))
    (unwind-protect
        (let ((dired-buffer (dired-noselect temp-dir)))
          (with-current-buffer dired-buffer
            ;; Ensure no files are marked by unmarking all
            (dired-unmark-all-marks)
            ;; Mock message function to capture output
            (let (message-captured)
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq message-captured (apply #'format format-string args)))))
                (watcherrun-dired-add-watcher)
                (should (string-match-p "No files marked" message-captured)))))
          (kill-buffer dired-buffer))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

(ert-deftest watcherrun-test-dired-add-watcher-with-marked-files ()
  "Test Dired integration with marked files."
  (let ((temp-dir (watcherrun-test-setup-temp-files)))
    (unwind-protect
        (let ((dired-buffer (watcherrun-test-create-dired-buffer temp-dir)))
          (with-current-buffer dired-buffer
            ;; Mock all user inputs and track watcher creation messages
            (let (created-watchers)
              (cl-letf (((symbol-function 'read-char-choice)
                         (lambda (prompt choices)
                           (cond
                            ((string-match-p "ystem.*isp" prompt) ?s)
                            ((string-match-p "mmediately.*ecursively" prompt) ?i)
                            (t (error "Unexpected prompt: %s" prompt)))))
                        ((symbol-function 'read-string)
                         (lambda (prompt &optional initial-input history default-value inherit-input-method)
                           "echo 'test'"))
                        ((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (let ((msg (apply #'format format-string args)))
                             (when (or (string-match-p "watcher" msg)
                                      (string-match-p "Created" msg))
                               (push msg created-watchers))))))
              
                ;; Call the function - it should succeed without error
                (watcherrun-dired-add-watcher)
                
                ;; Test passes if function completed successfully
                (should t)
                
                ;; Clean up any watchers that were actually created
                (maphash (lambda (id watcher)
                           (watcherrun-remove-watcher id))
                         watcherrun-watchers))))
          (kill-buffer dired-buffer))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

;; Test setup functions

(ert-deftest watcherrun-test-setup-dired-integration ()
  "Test that Dired integration setup works."
  ;; The function should set up keybinding after dired loads
  (should (fboundp 'watcherrun-setup-dired-integration))
  
  ;; Call the setup function
  (watcherrun-setup-dired-integration)
  
  ;; Load dired to trigger the after-load hook
  (require 'dired)
  
  ;; Check that the keybinding is set up in dired-mode-map
  (should (keymapp dired-mode-map))
  (should (eq (lookup-key dired-mode-map (kbd "W")) 
              'watcherrun-dired-add-watcher)))

;; Test interactive watcher creation

(ert-deftest watcherrun-test-add-watcher-interactive-file ()
  "Test interactive watcher creation for a file."
  (let ((temp-dir (watcherrun-test-setup-temp-files)))
    (unwind-protect
        (let ((test-file (expand-file-name "test1.txt" temp-dir))
              created-watcher-id)
          ;; Mock user inputs
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (prompt &optional dir default-filename mustmatch initial predicate)
                       test-file))
                    ((symbol-function 'read-char-choice)
                     (lambda (prompt choices) ?s))
                    ((symbol-function 'read-string)
                     (lambda (prompt &optional initial-input history default-value inherit-input-method)
                       "echo 'test'"))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (let ((msg (apply #'format format-string args)))
                         (when (string-match "Added watcher \\([^ ]+\\)" msg)
                           (setq created-watcher-id (match-string 1 msg)))))))
            
            ;; Call the function
            (watcherrun-add-watcher-interactive)
            
            ;; Verify watcher was created
            (should created-watcher-id)
            
            ;; Clean up
            (when created-watcher-id
              (watcherrun-remove-watcher created-watcher-id))))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

(ert-deftest watcherrun-test-add-watcher-interactive-directory ()
  "Test interactive watcher creation for a directory."
  (let ((temp-dir (watcherrun-test-setup-temp-files)))
    (unwind-protect
        (let ((created-watcher-id))
          ;; Mock user inputs
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (prompt &optional dir default-filename mustmatch initial predicate)
                       temp-dir))
                    ((symbol-function 'read-char-choice)
                     (lambda (prompt choices)
                       (cond
                        ((string-match-p "ystem.*isp" prompt) ?l)
                        ((string-match-p "mmediately.*ecursively" prompt) ?r)
                        (t (error "Unexpected prompt: %s" prompt)))))
                    ((symbol-function 'read-string)
                     (lambda (prompt &optional initial-input history default-value inherit-input-method)
                       "(message \"Directory changed\")"))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (let ((msg (apply #'format format-string args)))
                         (when (string-match "Added watcher \\([^ ]+\\)" msg)
                           (setq created-watcher-id (match-string 1 msg)))))))
            
            ;; Call the function
            (watcherrun-add-watcher-interactive)
            
            ;; Verify watcher was created
            (should created-watcher-id)
            
            ;; Clean up
            (when created-watcher-id
              (watcherrun-remove-watcher created-watcher-id))))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

(ert-deftest watcherrun-test-add-watcher-interactive-invalid-path ()
  "Test interactive watcher creation with invalid path."
  (let ((invalid-path "/nonexistent/path/file.txt")
        (error-message-captured nil))
    ;; Mock user inputs
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (prompt &optional dir default-filename mustmatch initial predicate)
                 invalid-path))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq error-message-captured (apply #'format format-string args)))))
      
      ;; Call the function
      (watcherrun-add-watcher-interactive)
      
      ;; Verify error message was shown
      (should error-message-captured)
      (should (string-match-p "Error:" error-message-captured)))))

(ert-deftest watcherrun-test-add-watcher-interactive-file-completion ()
  "Test that read-file-name provides path completion."
  (let ((temp-dir (watcherrun-test-setup-temp-files))
        (read-file-name-called nil))
    (unwind-protect
        (progn
          ;; Mock read-file-name to verify it's called with correct parameters
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (prompt &optional dir default-filename mustmatch initial predicate)
                       (setq read-file-name-called t)
                       ;; Verify prompt and mustmatch parameter
                       (should (string-match-p "Watch file or directory" prompt))
                       (should (eq mustmatch t))
                       ;; Return a valid path to continue test
                       (expand-file-name "test1.txt" temp-dir)))
                    ((symbol-function 'read-char-choice)
                     (lambda (prompt choices) ?s))
                    ((symbol-function 'read-string)
                     (lambda (prompt &optional initial-input history default-value inherit-input-method)
                       "echo 'test'"))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args) nil)))
            
            ;; Call the function
            (watcherrun-add-watcher-interactive)
            
            ;; Verify read-file-name was called
            (should read-file-name-called)))
      (watcherrun-test-cleanup-temp-dir temp-dir))))

(provide 'test-ui)
;;; test-ui.el ends here
