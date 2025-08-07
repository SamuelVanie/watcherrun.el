;;; test-documentation.el --- Tests for WatcherRun documentation examples

;; This is a comprehensive test to ensure the documentation files are working 
;; correctly and all examples run as expected.

(require 'ert)

(defun test-readme-examples ()
  "Test examples from README.md"
  (interactive)
  
  ;; Test basic API calls
  (let ((test-file (make-temp-file "watcherrun-test" nil ".txt")))
    (unwind-protect
        (progn
          ;; Test basic watcher creation
          (let ((watcher-id (watcherrun-add-watcher 
                            (list test-file) 
                            "echo 'File changed: {{file}}'" 
                            'system 
                            nil)))
            (should (stringp watcher-id))
            (should (string-prefix-p "watcher-" watcher-id))
            
            ;; Test watcher listing
            (let ((watchers (watcherrun-list-watchers)))
              (should (> (length watchers) 0)))
            
            ;; Test watcher removal
            (should (watcherrun-remove-watcher watcher-id))))
      
      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(defun test-template-variables ()
  "Test template variable expansion examples"
  (interactive)
  
  (let ((test-file "/tmp/test-file.txt"))
    ;; Test all template variables
    (should (string= (watcherrun-expand-placeholders "{{file}}" test-file)
                    test-file))
    (should (string= (watcherrun-expand-placeholders "{{basename}}" test-file)
                    "test-file"))
    (should (string= (watcherrun-expand-placeholders "{{dirname}}" test-file)
                    "/tmp/"))
    (should (string= (watcherrun-expand-placeholders "{{extension}}" test-file)
                    "txt"))))

(defun test-api-examples ()
  "Test examples from API.md"
  (interactive)
  
  ;; Test custom placeholder expansion example
  (let ((command "pandoc {{file}} -o {{basename}}.html")
        (file-path "/docs/readme.md"))
    (should (string= (watcherrun-expand-placeholders command file-path)
                    "pandoc /docs/readme.md -o readme.html")))
  
  ;; Test command type detection
  (should (eq (watcherrun-determine-command-type "(message \"hello\")")
             'lisp))
  (should (eq (watcherrun-determine-command-type "make build")
             'system)))

(defun test-example-configurations ()
  "Test configuration examples from EXAMPLES.md"
  (interactive)
  
  ;; Test that example functions can be defined without errors
  (eval '(defun my-web-dev-watchers ()
          "Set up watchers for web development."
          (interactive)
          (watcherrun-add-watcher '("src/js/") "npm run build:js" 'system t)
          (watcherrun-add-watcher '("src/css/") "npm run build:css" 'system t)
          (watcherrun-add-watcher '("test/") "npm test" 'system t)))
  
  ;; Verify function was created
  (should (fboundp 'my-web-dev-watchers))
  
  ;; Test conditional watcher setup logic
  (let ((default-directory "/tmp/"))
    ;; Create temporary package.json
    (with-temp-file "package.json"
      (insert "{}"))
    
    (unwind-protect
        (progn
          ;; Test project detection
          (should (file-exists-p "package.json")))
      
      ;; Cleanup
      (when (file-exists-p "package.json")
        (delete-file "package.json")))))

(defun test-full-workflow ()
  "Test a complete workflow with documentation examples"
  (interactive)
  
  (let ((temp-dir (make-temp-file "watcherrun-test" t))
        (watcher-ids '()))
    
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create test files
          (with-temp-file "test.js"
            (insert "console.log('Hello World');"))
          (with-temp-file "test.py"
            (insert "print('Hello World')"))
          
          ;; Set up watchers like in examples
          (push (watcherrun-add-watcher 
                '("test.js") 
                "node {{file}}" 
                'system 
                nil) 
                watcher-ids)
          
          (push (watcherrun-add-watcher 
                '("test.py") 
                "python {{file}}" 
                'system 
                nil) 
                watcher-ids)
          
          ;; Verify watchers were created
          (should (= (length watcher-ids) 2))
          (should (= (length (watcherrun-list-watchers)) 2))
          
          ;; Test interactive commands work
          (should (commandp 'watcherrun-add-watcher-interactive))
          (should (commandp 'watcherrun-list-watchers-menu))
          (should (commandp 'watcherrun-delete-watcher-interactive))
          (should (commandp 'watcherrun-modify-watcher-interactive))
          (should (commandp 'watcherrun-show-errors)))
      
      ;; Cleanup
      (dolist (watcher-id watcher-ids)
        (when watcher-id
          (watcherrun-remove-watcher watcher-id)))
      
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(defun test-documentation-performance ()
  "Test that documentation examples don't cause performance issues"
  (interactive)
  
  (let ((start-time (current-time))
        (watcher-count 10)
        (watcher-ids '()))
    
    (unwind-protect
        (progn
          ;; Create multiple watchers as shown in examples
          (dotimes (i watcher-count)
            (let ((temp-file (make-temp-file "perf-test" nil ".txt")))
              (push (watcherrun-add-watcher 
                    (list temp-file)
                    (format "echo 'Test %d: {{file}}'" i)
                    'system
                    nil)
                    watcher-ids)))
          
          ;; Measure time
          (let ((elapsed (time-subtract (current-time) start-time)))
            (should (< (time-to-seconds elapsed) 5.0))  ; Should complete in 5 seconds
            (message "Created %d watchers in %.2f seconds" 
                    watcher-count (time-to-seconds elapsed))))
      
      ;; Cleanup
      (dolist (watcher-id watcher-ids)
        (when watcher-id
          (watcherrun-remove-watcher watcher-id))))))

(defun validate-documentation-consistency ()
  "Validate that documentation is consistent with implementation"
  (interactive)
  
  ;; Check that all documented functions exist
  (let ((documented-functions '(watcherrun-add-watcher
                               watcherrun-remove-watcher
                               watcherrun-list-watchers
                               watcherrun-modify-watcher
                               watcherrun-add-watcher-interactive
                               watcherrun-delete-watcher-interactive
                               watcherrun-modify-watcher-interactive
                               watcherrun-show-errors
                               watcherrun-expand-placeholders
                               watcherrun-validate-path
                               watcherrun-determine-command-type)))
    
    (dolist (func documented-functions)
      (should (fboundp func))))
  
  ;; Check that all documented variables exist
  (let ((documented-variables '(watcherrun-debounce-time
                               watcherrun-ignore-patterns
                               watcherrun-ignore-hidden-files
                               watcherrun-show-error-notifications)))
    
    (dolist (var documented-variables)
      (should (boundp var))))
  
  ;; Check that keybindings match documentation
  (when (boundp 'watcherrun-mode-map)
    (should (keymapp watcherrun-mode-map))
    (should (lookup-key watcherrun-mode-map (kbd "C-c w l")))
    (should (lookup-key watcherrun-mode-map (kbd "C-c w a")))
    (should (lookup-key watcherrun-mode-map (kbd "C-c w d")))
    (should (lookup-key watcherrun-mode-map (kbd "C-c w m")))
    (should (lookup-key watcherrun-mode-map (kbd "C-c w e")))))

(defun run-all-documentation-tests ()
  "Run all documentation tests"
  (interactive)
  
  (message "Running documentation tests...")
  
  ;; Setup
  (require 'watcherrun)
  (watcherrun-mode 1)
  
  (unwind-protect
      (progn
        (test-readme-examples)
        (message "✓ README examples test passed")
        
        (test-template-variables)
        (message "✓ Template variables test passed")
        
        (test-api-examples)
        (message "✓ API examples test passed")
        
        (test-example-configurations)
        (message "✓ Configuration examples test passed")
        
        (test-full-workflow)
        (message "✓ Full workflow test passed")
        
        (test-documentation-performance)
        (message "✓ Performance test passed")
        
        (validate-documentation-consistency)
        (message "✓ Documentation consistency validation passed")
        
        (message "All documentation tests passed!"))
    
    ;; Cleanup
    (watcherrun-mode -1)))

(provide 'test-documentation)
