# Task ID: EXEC-002

**Title:** Implement Lisp Expression Evaluator with Safety Sandbox

## Description
Create a safe Lisp expression evaluator that can execute Emacs Lisp code within the context of file changes. This component must provide security through expression validation and sandboxing while offering access to useful context variables.

**Step-by-step breakdown:**
1. Implement safe expression parsing and validation
2. Create security sandbox with forbidden functions list
3. Set up context variables (file-var, watcher-id-var, etc.)
4. Implement `watcherrun-evaluate-lisp-expression` function
5. Add timeout mechanism to prevent infinite loops
6. Integrate with error handling for evaluation errors

## Inputs
- Lisp evaluator specification from `component_execution_layer.md`
- Security requirements and forbidden functions list
- Context variable requirements
- Common use cases for Lisp expressions

## Expected Output
- `watcherrun-evaluate-lisp-expression` function
- Expression validation and security system
- Context variable setup
- Error handling for evaluation failures
- Timeout mechanism for long-running expressions

## Estimated Time
4 hours

## Component
['execution_layer']

## Level
advanced

## Best Practices / Notes
- **SECURITY CRITICAL**: Never allow unchecked evaluation of user input
- Use `read` to parse expressions safely before evaluation
- Implement timeout using `with-timeout` to prevent infinite loops
- Blacklist dangerous functions like `delete-file`, `shell-command`, etc.
- Provide useful context variables but don't expose sensitive data
- Always wrap evaluation in `condition-case` for error handling

**Forbidden Functions (Security):**
```elisp
(defvar watcherrun-forbidden-functions
  '(delete-file delete-directory shell-command
    kill-emacs save-buffers-kill-emacs
    eval-expression shell-command-to-string
    start-process call-process)
  "Functions that are not allowed in watcher expressions.")
```

**Context Variables:**
When evaluating expressions, these variables are automatically available:
- `file-var`: Full path to the changed file
- `watcher-id-var`: ID of the current watcher  
- `change-time`: Timestamp of the file change
- `buffer-context`: Current buffer when file changed (if applicable)

**Safe Evaluation Framework:**
```elisp
(defun watcherrun-evaluate-lisp-expression (expression file-path watcher-id)
  "Safely evaluate Lisp expression with file context."
  (condition-case error
      (with-timeout (5.0)  ; 5 second timeout
        (let ((file-var file-path)
              (watcher-id-var watcher-id)
              (change-time (current-time)))
          
          ;; Parse and validate expression
          (let ((parsed-expr (read expression)))
            (when (watcherrun-validate-lisp-expression parsed-expr)
              (eval parsed-expr)))))
    
    (error (watcherrun-log-error watcher-id 'lisp-error 
                                (format "Evaluation error: %s" error)))))
```

**Expression Validation:**
- Check for forbidden functions recursively
- Validate that all referenced functions exist
- Detect potentially dangerous operations
- Ensure expression is well-formed

**Common Use Cases to Support:**
- Compilation: `(compile "make -C ~/my-project")`
- Buffer management: `(when (string-suffix-p ".el" file-var) (load-file file-var))`
- Notifications: `(message "File %s changed" (file-name-nondirectory file-var))`

**Dependencies:** Requires CORE-002 (command executor), SUPPORT-004 (error handling), and SUPPORT-003 (utilities).
