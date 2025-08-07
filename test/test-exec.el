;;; test-exec.el --- Tests for WatcherRun execution layer -*- lexical-binding: t -*-

;; Copyright (C) 2023 WatcherRun Contributors

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Tests for the execution layer of WatcherRun, including system command
;; execution, process management, and compilation buffer handling.

;;; Code:

(require 'ert)
(require 'watcherrun-exec)
;; (require 'watcherrun-utils)

;; Test helper functions
(defun watcherrun-test--cleanup-test-resources ()
  "Clean up test resources like buffers and processes."
  ;; Clean up any test processes first
  (watcherrun-kill-all-processes)
  
  ;; Clean up buffers without prompting
  (dolist (buffer-name '("*watcherrun-output-test-watcher*" 
                        "*compilation-watcher-test-watcher*"
                        "*watcherrun-output-test-watcher-1*"
                        "*watcherrun-output-test-watcher-2*"))
    (when-let ((buffer (get-buffer buffer-name)))
      (with-current-buffer buffer
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defun watcherrun-test--wait-for-process (watcher-id &optional timeout)
  "Wait for process associated with WATCHER-ID to complete.
TIMEOUT defaults to 3 seconds."
  (let ((timeout (or timeout 3))
        (start-time (float-time)))
    (while (and (gethash watcher-id watcherrun--active-processes)
                (< (- (float-time) start-time) timeout))
      (sleep-for 0.1))))

;; Test compilation command detection
(ert-deftest watcherrun-test-is-compilation-command-p ()
  "Test detection of compilation commands."
  (should (watcherrun-is-compilation-command-p "make all"))
  (should (watcherrun-is-compilation-command-p "npm run build"))
  (should (watcherrun-is-compilation-command-p "cargo test"))
  (should (watcherrun-is-compilation-command-p "go build main.go"))
  (should (watcherrun-is-compilation-command-p "yarn test"))
  (should (watcherrun-is-compilation-command-p "compile my-file.el"))
  
  ;; Should not match
  (should-not (watcherrun-is-compilation-command-p "cat file.txt"))
  (should-not (watcherrun-is-compilation-command-p "ls -la"))
  (should-not (watcherrun-is-compilation-command-p "python script.py")))

;; Test system command execution
(ert-deftest watcherrun-test-execute-system-command ()
  "Test basic system command execution."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Execute a simple command - use non-compilation command
          (let ((process (watcherrun-execute-system-command 
                         "ls /tmp" test-file watcher-id)))
            
            ;; Verify process was created
            (should process)
            (should (process-live-p process))
            
            ;; Wait for completion
            (watcherrun-test--wait-for-process watcher-id)
            
            ;; Check output buffer was created
            (let ((buffer (get-buffer "*watcherrun-output-test-watcher*")))
              (should buffer)
              (with-current-buffer buffer
                (should (string-match-p "tmp" (buffer-string)))))))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

;; Test placeholder expansion in commands
(ert-deftest watcherrun-test-command-placeholder-expansion ()
  "Test placeholder expansion during command execution."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Execute command with placeholders - simple command that works
          (let ((process (watcherrun-execute-system-command 
                         "wc -l {{file}}" test-file watcher-id)))
            
            ;; Wait for completion
            (watcherrun-test--wait-for-process watcher-id)
            
            ;; Check that placeholder was expanded (for compilation commands, buffer is different)
            (let ((buffer (or (get-buffer "*watcherrun-output-test-watcher*")
                             (get-buffer "*compilation-watcher-test-watcher*"))))
              (should buffer)
              (with-current-buffer buffer
                (should (string-match-p test-file (buffer-string)))))))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

;; Test process cleanup
(ert-deftest watcherrun-test-process-cleanup ()
  "Test that processes are properly cleaned up."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Start a long-running process
          (watcherrun-execute-system-command "sleep 10" test-file watcher-id)
          
          ;; Verify process is tracked
          (should (gethash watcher-id watcherrun--active-processes))
          
          ;; Clean up the process
          (watcherrun-cleanup-process watcher-id)
          
          ;; Verify process is no longer tracked
          (should-not (gethash watcher-id watcherrun--active-processes)))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

;; Test compilation command execution
(ert-deftest watcherrun-test-compilation-command ()
  "Test compilation command execution and buffer naming."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher"))
    
    (unwind-protect
        (progn
          ;; Execute a compilation command
          (watcherrun-execute-compilation-command "echo 'compilation test'" watcher-id)
          
          ;; Give some time for buffer creation and renaming
          (sleep-for 0.5)
          
          ;; Check that compilation buffer was created with correct name
          (let ((buffer (get-buffer "*compilation-watcher-test-watcher*")))
            (should buffer)
            (should (gethash watcher-id watcherrun--compilation-buffers))))
      
      ;; Cleanup
      (watcherrun-test--cleanup-test-resources))))

;; Test buffer cleanup
(ert-deftest watcherrun-test-buffer-cleanup ()
  "Test cleanup of watcher-related buffers."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Create some buffers by executing commands - use non-compilation command
          (watcherrun-execute-system-command "ls /tmp" test-file watcher-id)
          (watcherrun-test--wait-for-process watcher-id)
          
          ;; Verify buffer exists
          (should (get-buffer "*watcherrun-output-test-watcher*"))
          
          ;; Clean up watcher resources
          (watcherrun-cleanup-watcher-resources watcher-id)
          
          ;; Verify buffer is gone
          (should-not (get-buffer "*watcherrun-output-test-watcher*")))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

;; Test error handling in command execution
(ert-deftest watcherrun-test-command-error-handling ()
  "Test error handling during command execution."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Execute a command that will fail
          (let ((process (watcherrun-execute-system-command 
                         "false" test-file watcher-id)))  ; 'false' always exits with code 1
            
            ;; Wait for process to complete
            (watcherrun-test--wait-for-process watcher-id)
            
            ;; Check that error was logged in buffer
            (let ((buffer (get-buffer "*watcherrun-output-test-watcher*")))
              (should buffer)
              (with-current-buffer buffer
                (should (string-match-p "failed" (buffer-string)))))))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

;; Test multiple simultaneous processes
(ert-deftest watcherrun-test-multiple-processes ()
  "Test handling multiple simultaneous processes."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id-1 "test-watcher-1")
        (watcher-id-2 "test-watcher-2")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Start two processes
          (watcherrun-execute-system-command "ls /tmp" test-file watcher-id-1)
          (watcherrun-execute-system-command "ls /var" test-file watcher-id-2)
          
          ;; Wait for both to complete
          (watcherrun-test--wait-for-process watcher-id-1)
          (watcherrun-test--wait-for-process watcher-id-2)
          
          ;; Verify both buffers exist
          (should (get-buffer "*watcherrun-output-test-watcher-1*"))
          (should (get-buffer "*watcherrun-output-test-watcher-2*")))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

;; Test process listing
(ert-deftest watcherrun-test-list-active-processes ()
  "Test listing of active processes."
  (watcherrun-test--cleanup-test-resources)
  
  (let ((watcher-id "test-watcher")
        (test-file "/tmp/test-file.txt"))
    
    ;; Create a test file
    (with-temp-buffer
      (write-file test-file))
    
    (unwind-protect
        (progn
          ;; Start a process
          (watcherrun-execute-system-command "sleep 1" test-file watcher-id)
          
          ;; List active processes
          (let ((processes (watcherrun-list-active-processes)))
            (should (> (length processes) 0))
            (should (cl-some (lambda (proc) (string= (car proc) watcher-id)) processes)))
          
          ;; Wait for process to complete
          (watcherrun-test--wait-for-process watcher-id)
          
          ;; List should be empty now
          (let ((processes (watcherrun-list-active-processes)))
            (should-not (cl-some (lambda (proc) (string= (car proc) watcher-id)) processes))))
      
      ;; Cleanup
      (delete-file test-file)
      (watcherrun-test--cleanup-test-resources))))

(provide 'test-exec)

;;; test-exec.el ends here
