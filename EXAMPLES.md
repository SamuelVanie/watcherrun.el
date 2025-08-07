# Example Configurations

This file contains practical example configurations for different development workflows using WatcherRun.

## Table of Contents

- [Web Development](#web-development)
- [Python Development](#python-development)
- [Emacs Lisp Development](#emacs-lisp-development)
- [Documentation Workflows](#documentation-workflows)
- [Build Systems](#build-systems)
- [Testing Workflows](#testing-workflows)
- [Language-Specific Examples](#language-specific-examples)

## Web Development

### React Development
```elisp
(defun my-react-watchers ()
  "Set up watchers for React development."
  (interactive)
  ;; Build when source files change
  (watcherrun-add-watcher '("src/") "npm run build" 'system t)
  ;; Run tests when test files change
  (watcherrun-add-watcher '("src/__tests__/") "npm test" 'system t)
  ;; Lint when JavaScript files change
  (watcherrun-add-watcher '("src/") "npm run lint" 'system t)
  ;; Start dev server (run once)
  (watcherrun-add-watcher '("package.json") "npm start" 'system nil))
```

### Vue.js Development
```elisp
(defun my-vue-watchers ()
  "Set up watchers for Vue.js development."
  (interactive)
  (watcherrun-add-watcher '("src/") "npm run serve" 'system t)
  (watcherrun-add-watcher '("src/") "npm run test:unit" 'system t)
  (watcherrun-add-watcher '("src/") "vue-cli-service lint {{file}}" 'system nil))
```

### SASS/SCSS Compilation
```elisp
(defun my-sass-watchers ()
  "Set up SASS compilation watchers."
  (interactive)
  (watcherrun-add-watcher 
   '("styles/") 
   "sass {{file}} {{dirname}}/{{basename}}.css" 
   'system t))
```

### TypeScript Compilation
```elisp
(defun my-typescript-watchers ()
  "Set up TypeScript compilation."
  (interactive)
  (watcherrun-add-watcher '("src/") "tsc" 'system t)
  (watcherrun-add-watcher '("src/") "tslint {{file}}" 'system nil))
```

## Python Development

### Django Development
```elisp
(defun my-django-watchers ()
  "Set up watchers for Django development."
  (interactive)
  ;; Run tests when test files change
  (watcherrun-add-watcher 
   '("tests/") 
   "python manage.py test {{basename}}" 
   'system t)
  ;; Check migrations when models change
  (watcherrun-add-watcher 
   '("models.py") 
   "python manage.py makemigrations" 
   'system nil)
  ;; Lint Python files
  (watcherrun-add-watcher 
   '("*.py") 
   "flake8 {{file}}" 
   'system nil))
```

### Flask Development
```elisp
(defun my-flask-watchers ()
  "Set up watchers for Flask development."
  (interactive)
  (watcherrun-add-watcher '("app/") "python -m pytest tests/" 'system t)
  (watcherrun-add-watcher '("app/") "python -m flask run" 'system t)
  (watcherrun-add-watcher '("*.py") "black {{file}}" 'system nil))
```

### Data Science Workflow
```elisp
(defun my-datascience-watchers ()
  "Set up watchers for data science projects."
  (interactive)
  ;; Convert notebooks to Python scripts
  (watcherrun-add-watcher 
   '("notebooks/") 
   "jupyter nbconvert --to script {{file}}" 
   'system t)
  ;; Run analysis scripts
  (watcherrun-add-watcher 
   '("analysis/") 
   "python {{file}}" 
   'system nil)
  ;; Update documentation
  (watcherrun-add-watcher 
   '("docs/") 
   "sphinx-build -b html . _build" 
   'system t))
```

## Emacs Lisp Development

### Package Development
```elisp
(defun my-elisp-dev-watchers ()
  "Set up watchers for Emacs Lisp package development."
  (interactive)
  ;; Byte-compile on save
  (watcherrun-add-watcher 
   '("*.el") 
   "(byte-compile-file \"{{file}}\")" 
   'lisp nil)
  ;; Run tests
  (watcherrun-add-watcher 
   '("test/") 
   "emacs -batch -l ert -l {{file}} -f ert-run-tests-batch-and-exit" 
   'system t)
  ;; Check syntax
  (watcherrun-add-watcher 
   '("*.el") 
   "(check-parens)" 
   'lisp nil)
  ;; Load into current session
  (watcherrun-add-watcher 
   '("*.el") 
   "(load-file \"{{file}}\")" 
   'lisp nil))
```

### Configuration Management
```elisp
(defun my-emacs-config-watchers ()
  "Set up watchers for Emacs configuration."
  (interactive)
  ;; Reload configuration files
  (watcherrun-add-watcher 
   '("~/.emacs.d/init.el") 
   "(load-file \"{{file}}\")" 
   'lisp nil)
  ;; Reload specific modules
  (watcherrun-add-watcher 
   '("~/.emacs.d/lisp/") 
   "(load-file \"{{file}}\")" 
   'lisp t)
  ;; Validate syntax
  (watcherrun-add-watcher 
   '("~/.emacs.d/") 
   "(check-parens)" 
   'lisp t))
```

## Documentation Workflows

### Markdown to HTML
```elisp
(defun my-markdown-watchers ()
  "Set up watchers for Markdown documentation."
  (interactive)
  ;; Convert to HTML
  (watcherrun-add-watcher 
   '("docs/") 
   "pandoc {{file}} -o {{dirname}}/{{basename}}.html" 
   'system t)
  ;; Generate table of contents
  (watcherrun-add-watcher 
   '("README.md") 
   "gh-md-toc {{file}} > toc.md" 
   'system nil)
  ;; Spell check
  (watcherrun-add-watcher 
   '("*.md") 
   "aspell check {{file}}" 
   'system nil))
```

### Sphinx Documentation
```elisp
(defun my-sphinx-watchers ()
  "Set up watchers for Sphinx documentation."
  (interactive)
  (watcherrun-add-watcher 
   '("source/") 
   "sphinx-build -b html source build" 
   'system t)
  (watcherrun-add-watcher 
   '("source/") 
   "sphinx-build -b epub source build" 
   'system t))
```

### LaTeX Documents
```elisp
(defun my-latex-watchers ()
  "Set up watchers for LaTeX documents."
  (interactive)
  (watcherrun-add-watcher 
   '("*.tex") 
   "pdflatex {{file}}" 
   'system nil)
  (watcherrun-add-watcher 
   '("*.bib") 
   "bibtex {{basename}}" 
   'system nil))
```

## Build Systems

### Make-based Projects
```elisp
(defun my-make-watchers ()
  "Set up watchers for Make-based projects."
  (interactive)
  ;; Build when source files change
  (watcherrun-add-watcher '("src/") "make" 'system t)
  ;; Run tests
  (watcherrun-add-watcher '("tests/") "make test" 'system t)
  ;; Clean and rebuild on Makefile changes
  (watcherrun-add-watcher '("Makefile") "make clean && make" 'system nil))
```

### CMake Projects
```elisp
(defun my-cmake-watchers ()
  "Set up watchers for CMake projects."
  (interactive)
  (watcherrun-add-watcher '("src/") "cmake --build build/" 'system t)
  (watcherrun-add-watcher '("CMakeLists.txt") "cmake -B build" 'system nil)
  (watcherrun-add-watcher '("tests/") "ctest --test-dir build" 'system t))
```

### Gradle Projects
```elisp
(defun my-gradle-watchers ()
  "Set up watchers for Gradle projects."
  (interactive)
  (watcherrun-add-watcher '("src/") "./gradlew build" 'system t)
  (watcherrun-add-watcher '("src/test/") "./gradlew test" 'system t)
  (watcherrun-add-watcher '("build.gradle") "./gradlew clean build" 'system nil))
```

## Testing Workflows

### Continuous Testing
```elisp
(defun my-continuous-testing ()
  "Set up continuous testing watchers."
  (interactive)
  ;; Jest for JavaScript
  (watcherrun-add-watcher '("src/") "npm test -- --watchAll=false" 'system t)
  ;; PyTest for Python
  (watcherrun-add-watcher '("tests/") "pytest {{file}}" 'system nil)
  ;; Go tests
  (watcherrun-add-watcher '("*.go") "go test ./..." 'system nil)
  ;; Rust tests
  (watcherrun-add-watcher '("src/") "cargo test" 'system t))
```

### Coverage Reports
```elisp
(defun my-coverage-watchers ()
  "Set up test coverage watchers."
  (interactive)
  (watcherrun-add-watcher 
   '("src/") 
   "pytest --cov=src tests/" 
   'system t)
  (watcherrun-add-watcher 
   '("src/") 
   "coverage html" 
   'system t))
```

## Language-Specific Examples

### Go Development
```elisp
(defun my-go-watchers ()
  "Set up watchers for Go development."
  (interactive)
  ;; Build on save
  (watcherrun-add-watcher '("*.go") "go build" 'system nil)
  ;; Run tests
  (watcherrun-add-watcher '("*_test.go") "go test -v {{dirname}}" 'system nil)
  ;; Format code
  (watcherrun-add-watcher '("*.go") "gofmt -w {{file}}" 'system nil)
  ;; Lint
  (watcherrun-add-watcher '("*.go") "golint {{file}}" 'system nil))
```

### Rust Development
```elisp
(defun my-rust-watchers ()
  "Set up watchers for Rust development."
  (interactive)
  (watcherrun-add-watcher '("src/") "cargo check" 'system t)
  (watcherrun-add-watcher '("src/") "cargo test" 'system t)
  (watcherrun-add-watcher '("src/") "cargo clippy" 'system t)
  (watcherrun-add-watcher '("src/") "cargo fmt" 'system t))
```

### Java Development
```elisp
(defun my-java-watchers ()
  "Set up watchers for Java development."
  (interactive)
  (watcherrun-add-watcher '("src/") "mvn compile" 'system t)
  (watcherrun-add-watcher '("src/test/") "mvn test" 'system t)
  (watcherrun-add-watcher '("*.java") "checkstyle -c checkstyle.xml {{file}}" 'system nil))
```

### C/C++ Development
```elisp
(defun my-cpp-watchers ()
  "Set up watchers for C++ development."
  (interactive)
  ;; Compile single files
  (watcherrun-add-watcher 
   '("*.cpp") 
   "g++ -std=c++17 -Wall -g {{file}} -o {{basename}}" 
   'system nil)
  ;; Run tests
  (watcherrun-add-watcher '("tests/") "make test" 'system t)
  ;; Format code
  (watcherrun-add-watcher '("*.cpp") "clang-format -i {{file}}" 'system nil))
```

## Advanced Configurations

### Multi-Stage Build Pipeline
```elisp
(defun my-pipeline-watchers ()
  "Set up a complete build pipeline."
  (interactive)
  ;; Stage 1: Lint
  (watcherrun-add-watcher '("src/") "npm run lint" 'system t)
  ;; Stage 2: Test
  (watcherrun-add-watcher '("src/") "npm test" 'system t)
  ;; Stage 3: Build
  (watcherrun-add-watcher '("src/") "npm run build" 'system t)
  ;; Stage 4: Deploy (only on specific files)
  (watcherrun-add-watcher '("dist/") "npm run deploy" 'system t))
```

### Conditional Watchers
```elisp
(defun my-conditional-watchers ()
  "Set up conditional watchers based on project type."
  (interactive)
  (cond
   ;; Node.js project
   ((file-exists-p "package.json")
    (watcherrun-add-watcher '("src/") "npm run build" 'system t))
   ;; Python project
   ((file-exists-p "requirements.txt")
    (watcherrun-add-watcher '("*.py") "python -m pytest" 'system nil))
   ;; Rust project
   ((file-exists-p "Cargo.toml")
    (watcherrun-add-watcher '("src/") "cargo build" 'system t))
   ;; Default
   (t
    (message "Unknown project type, setting up generic watchers")
    (watcherrun-add-watcher '(".") "make" 'system t))))
```

### Environment-Specific Watchers
```elisp
(defun my-env-specific-watchers ()
  "Set up watchers based on environment."
  (interactive)
  (let ((env (or (getenv "NODE_ENV") "development")))
    (cond
     ((string= env "development")
      (watcherrun-add-watcher '("src/") "npm run dev" 'system t))
     ((string= env "test")
      (watcherrun-add-watcher '("src/") "npm test" 'system t))
     ((string= env "production")
      (watcherrun-add-watcher '("src/") "npm run build" 'system t)))))
```

## Utility Functions

### Project-Aware Watcher Setup
```elisp
(defun watcherrun-setup-project ()
  "Automatically set up watchers based on project type."
  (interactive)
  (let ((project-root (or (locate-dominating-file default-directory ".git")
                         default-directory)))
    (let ((default-directory project-root))
      (cond
       ((file-exists-p "package.json") (my-react-watchers))
       ((file-exists-p "Cargo.toml") (my-rust-watchers))
       ((file-exists-p "pom.xml") (my-java-watchers))
       ((file-exists-p "requirements.txt") (my-django-watchers))
       ((file-exists-p "Makefile") (my-make-watchers))
       (t (message "Unknown project type"))))))
```

### Bulk Watcher Management
```elisp
(defun watcherrun-clear-all-watchers ()
  "Remove all active watchers."
  (interactive)
  (when (yes-or-no-p "Remove all watchers? ")
    (dolist (watcher-id (hash-table-keys watcherrun--watchers))
      (watcherrun-remove-watcher watcher-id))
    (message "All watchers removed")))

(defun watcherrun-list-watchers-by-command (command-pattern)
  "List watchers matching COMMAND-PATTERN."
  (interactive "sCommand pattern: ")
  (let ((matching-watchers
         (seq-filter (lambda (w)
                      (string-match-p command-pattern (plist-get w :command)))
                    (hash-table-values watcherrun--watchers))))
    (if matching-watchers
        (message "Found %d matching watchers" (length matching-watchers))
      (message "No watchers found matching pattern: %s" command-pattern))))
```

## Tips for Effective Usage

1. **Start Small**: Begin with one or two watchers, then expand as needed
2. **Use Specific Paths**: Prefer specific file patterns over broad directory watching
3. **Leverage Templates**: Use `{{file}}`, `{{basename}}` for flexible commands
4. **Group Related Watchers**: Create setup functions for related workflows
5. **Test Commands First**: Verify commands work manually before setting up watchers
6. **Monitor Performance**: Use `watcherrun-debug` to monitor file events
7. **Use Compilation Mode**: For build commands, leverage Emacs' compilation features
8. **Environment Variables**: Incorporate environment-specific configurations

## Troubleshooting Common Issues

- **Too many events**: Use more specific file patterns
- **Commands not found**: Check `exec-path` configuration
- **Performance issues**: Avoid watching large directories
- **Permission errors**: Ensure proper file permissions

---

*Copy these examples and adapt them to your specific needs. Each project is unique, so customize the commands and paths accordingly.*
