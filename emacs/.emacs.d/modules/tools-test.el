;;; tools-test.el --- Language-Aware Test Runner -*- lexical-binding: t -*-
;;; Commentary:
;;; Natural IDE test runner: detects language, picks the right test framework,
;;; runs tests in a dedicated compilation buffer, parses results, shows a
;;; summary, and keeps history. Zero config needed — works by inspecting the
;;; current buffer's major-mode and project structure.
;;;
;;; Supports:
;;;   Python  — pytest, unittest               (C-c C-t in python-mode)
;;;   Rust    — cargo test                     (C-c C-t in rust-mode)
;;;   Go      — go test ./...                  (C-c C-t in go-mode)
;;;   JS/TS   — jest, vitest, npm test         (C-c C-t in js2/ts-mode)
;;;   Elixir  — mix test                       (C-c C-t in elixir-mode)
;;;   Ruby    — rspec, minitest                (C-c C-t in ruby-mode)
;;;   Java    — mvn test, gradle test          (C-c C-t in java-mode)
;;;   Haskell — cabal test, stack test         (C-c C-t in haskell-mode)
;;;   Shell   — bats                           (C-c C-t in sh-mode)
;;;   Emacs Lisp — ERT                        (C-c C-t in emacs-lisp-mode)
;;;   C/C++   — ctest, make test              (C-c C-t in c/c++-mode)
;;;   Generic — any Makefile with a test target
;;;
;;; Keybindings:
;;;   C-c C-t   emacs-ide-test-run          run nearest test / full suite
;;;   C-c C-T   emacs-ide-test-run-all      always run full suite
;;;   C-c x p   emacs-ide-test-run-point    run test at point
;;;   C-c x l   emacs-ide-test-run-last     repeat last test command
;;;   C-c x r   emacs-ide-test-report       show test history report
;;;   C-c x h   hydra-test/body             test hydra
;;;
;;; Add "tools-test" to emacs-ide-feature-modules in init.el (after lang-core,
;;; before keybindings).
;;;
;;; Version: 1.0.2
;;; Fixes:
;;;   - 1.0.2: emacs-ide-test--detect-haskell used file-exists-p with "*.cabal"
;;;     which is a shell glob, not a valid path — always returned nil, so cabal
;;;     was never detected. Fixed to use directory-files with a regexp.
;;;   - 1.0.2: emacs-ide-test--parse-results rspec branch set :passed to the
;;;     total example count rather than passing count. "5 examples, 2 failures"
;;;     produced passed=5,failed=2 (off by 2). Fixed: passed = total - failed.
;;; Code:

(require 'cl-lib)
(require 'compile)

;; ============================================================================
;; STATE
;; ============================================================================
(defvar emacs-ide-test--last-command nil
  "Last test command run (string).")

(defvar emacs-ide-test--last-directory nil
  "Working directory of last test run.")

(defvar emacs-ide-test--history nil
  "List of plists: (:time :command :directory :status :passed :failed :duration).")

(defvar emacs-ide-test--history-max 50
  "Maximum entries to keep in test history.")

(defvar emacs-ide-test--buffer-name "*Test Runner*"
  "Name of the test output buffer.")

(defvar emacs-ide-test--start-time nil
  "Time when the current test run started.")

;; ============================================================================
;; PROJECT ROOT HELPER
;; ============================================================================
(defun emacs-ide-test--project-root ()
  "Return the project root, falling back to default-directory."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

;; ============================================================================
;; FRAMEWORK DETECTION
;; ============================================================================

(defun emacs-ide-test--detect-python ()
  "Detect Python test framework. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root))
        (file (buffer-file-name)))
    (cond
     ;; pytest: preferred, most common
     ((and (executable-find "pytest")
           (or (file-exists-p (expand-file-name "pytest.ini" root))
               (file-exists-p (expand-file-name "pyproject.toml" root))
               (file-exists-p (expand-file-name "setup.cfg" root))
               (file-exists-p (expand-file-name "conftest.py" root))))
      (cons (format "cd %s && pytest -v --tb=short 2>&1"
                    (shell-quote-argument root))
            "pytest (project)"))
     ;; pytest on current file
     ((and file (executable-find "pytest")
           (string-match-p "test_\\|_test" (file-name-nondirectory file)))
      (cons (format "pytest -v --tb=short %s 2>&1"
                    (shell-quote-argument file))
            "pytest (file)"))
     ;; unittest fallback
     ((executable-find "python3")
      (cons (format "cd %s && python3 -m pytest -v 2>/dev/null || python3 -m unittest discover -v 2>&1"
                    (shell-quote-argument root))
            "unittest discover"))
     (t nil))))

(defun emacs-ide-test--detect-rust ()
  "Detect Rust test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (when (and (executable-find "cargo")
               (file-exists-p (expand-file-name "Cargo.toml" root)))
      (cons (format "cd %s && cargo test 2>&1" (shell-quote-argument root))
            "cargo test"))))

(defun emacs-ide-test--detect-go ()
  "Detect Go test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (when (executable-find "go")
      (cons (format "cd %s && go test ./... -v 2>&1" (shell-quote-argument root))
            "go test ./..."))))

(defun emacs-ide-test--detect-javascript ()
  "Detect JS/TS test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ;; Vitest
     ((and (executable-find "npx")
           (file-exists-p (expand-file-name "vitest.config.js" root)))
      (cons (format "cd %s && npx vitest run 2>&1" (shell-quote-argument root))
            "vitest"))
     ((and (executable-find "npx")
           (file-exists-p (expand-file-name "vitest.config.ts" root)))
      (cons (format "cd %s && npx vitest run 2>&1" (shell-quote-argument root))
            "vitest"))
     ;; Jest
     ((and (executable-find "npx")
           (or (file-exists-p (expand-file-name "jest.config.js" root))
               (file-exists-p (expand-file-name "jest.config.ts" root))))
      (cons (format "cd %s && npx jest --verbose 2>&1" (shell-quote-argument root))
            "jest"))
     ;; npm test (package.json scripts.test)
     ((and (executable-find "npm")
           (file-exists-p (expand-file-name "package.json" root)))
      (cons (format "cd %s && npm test -- --verbose 2>&1" (shell-quote-argument root))
            "npm test"))
     (t nil))))

(defun emacs-ide-test--detect-ruby ()
  "Detect Ruby test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ((and (executable-find "rspec")
           (file-exists-p (expand-file-name "spec" root)))
      (cons (format "cd %s && rspec --format documentation 2>&1"
                    (shell-quote-argument root))
            "rspec"))
     ((executable-find "ruby")
      (cons (format "cd %s && ruby -Itest -Ilib test/**/*_test.rb 2>&1"
                    (shell-quote-argument root))
            "minitest"))
     (t nil))))

(defun emacs-ide-test--detect-java ()
  "Detect Java test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ((and (executable-find "mvn")
           (file-exists-p (expand-file-name "pom.xml" root)))
      (cons (format "cd %s && mvn test 2>&1" (shell-quote-argument root))
            "maven test"))
     ((and (executable-find "gradle")
           (or (file-exists-p (expand-file-name "build.gradle" root))
               (file-exists-p (expand-file-name "build.gradle.kts" root))))
      (cons (format "cd %s && gradle test 2>&1" (shell-quote-argument root))
            "gradle test"))
     (t nil))))

(defun emacs-ide-test--detect-haskell ()
  "Detect Haskell test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ((and (executable-find "cabal")
           ;; FIX A: file-exists-p with a glob like "*.cabal" never matches —
           ;; it tests for a literal file named "*.cabal". Use directory-files
           ;; with a regexp to detect any real .cabal file in the root.
           (directory-files root nil "\\.cabal\\'"))
      (cons (format "cd %s && cabal test 2>&1" (shell-quote-argument root))
            "cabal test"))
     ((executable-find "stack")
      (cons (format "cd %s && stack test 2>&1" (shell-quote-argument root))
            "stack test"))
     (t nil))))

(defun emacs-ide-test--detect-elixir ()
  "Detect Elixir test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (when (and (executable-find "mix")
               (file-exists-p (expand-file-name "mix.exs" root)))
      (cons (format "cd %s && mix test 2>&1" (shell-quote-argument root))
            "mix test"))))

(defun emacs-ide-test--detect-shell ()
  "Detect shell test runner (bats). Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (when (executable-find "bats")
      (let ((test-dir (expand-file-name "test" root)))
        (if (file-directory-p test-dir)
            (cons (format "cd %s && bats test/ 2>&1" (shell-quote-argument root))
                  "bats")
          (cons (format "bats %s 2>&1" (shell-quote-argument (or (buffer-file-name) ".")))
                "bats (file)"))))))

(defun emacs-ide-test--detect-elisp ()
  "Detect Emacs Lisp test runner (ERT). Returns (command . description)."
  (cons (format "emacs --batch -l %s -f ert-run-tests-batch-and-exit 2>&1"
                (shell-quote-argument (or (buffer-file-name) user-init-file)))
        "ERT (batch)"))

(defun emacs-ide-test--detect-c ()
  "Detect C/C++ test runner. Returns (command . description)."
  (let ((root (emacs-ide-test--project-root)))
    (cond
     ((and (executable-find "ctest")
           (file-exists-p (expand-file-name "CMakeLists.txt" root)))
      (cons (format "cd %s && ctest --output-on-failure 2>&1"
                    (shell-quote-argument root))
            "ctest"))
     ((file-exists-p (expand-file-name "Makefile" root))
      (cons (format "cd %s && make test 2>&1" (shell-quote-argument root))
            "make test"))
     (t nil))))

(defun emacs-ide-test--detect-generic ()
  "Generic fallback: Makefile test target. Returns (command . description) or nil."
  (let ((root (emacs-ide-test--project-root)))
    (when (file-exists-p (expand-file-name "Makefile" root))
      ;; Check if 'test' target exists
      (let ((targets (shell-command-to-string
                      (format "make -C %s -qp 2>/dev/null | awk -F: '/^[a-zA-Z]/{print $1}'"
                              (shell-quote-argument root)))))
        (when (string-match-p "\\btest\\b" targets)
          (cons (format "cd %s && make test 2>&1" (shell-quote-argument root))
                "make test"))))))

;; ============================================================================
;; DISPATCH TABLE
;; ============================================================================
(defvar emacs-ide-test--dispatch-table
  '((python-mode      . emacs-ide-test--detect-python)
    (python-ts-mode   . emacs-ide-test--detect-python)
    (rust-mode        . emacs-ide-test--detect-rust)
    (rust-ts-mode     . emacs-ide-test--detect-rust)
    (go-mode          . emacs-ide-test--detect-go)
    (go-ts-mode       . emacs-ide-test--detect-go)
    (js2-mode         . emacs-ide-test--detect-javascript)
    (js-mode          . emacs-ide-test--detect-javascript)
    (javascript-ts-mode . emacs-ide-test--detect-javascript)
    (typescript-mode  . emacs-ide-test--detect-javascript)
    (typescript-ts-mode . emacs-ide-test--detect-javascript)
    (ruby-mode        . emacs-ide-test--detect-ruby)
    (ruby-ts-mode     . emacs-ide-test--detect-ruby)
    (java-mode        . emacs-ide-test--detect-java)
    (java-ts-mode     . emacs-ide-test--detect-java)
    (haskell-mode     . emacs-ide-test--detect-haskell)
    (elixir-mode      . emacs-ide-test--detect-elixir)
    (sh-mode          . emacs-ide-test--detect-shell)
    (bash-ts-mode     . emacs-ide-test--detect-shell)
    (emacs-lisp-mode  . emacs-ide-test--detect-elisp)
    (c-mode           . emacs-ide-test--detect-c)
    (c++-mode         . emacs-ide-test--detect-c)
    (c-ts-mode        . emacs-ide-test--detect-c)
    (c++-ts-mode      . emacs-ide-test--detect-c))
  "Alist of major-mode -> detection function.")

(defun emacs-ide-test--detect ()
  "Detect the test command for the current buffer.
Returns (command . description) or nil."
  (let ((detector (cdr (assoc major-mode emacs-ide-test--dispatch-table))))
    (if detector
        (or (funcall detector) (emacs-ide-test--detect-generic))
      (emacs-ide-test--detect-generic))))

;; ============================================================================
;; RESULT PARSING
;; ============================================================================
(defun emacs-ide-test--parse-results (output)
  "Parse OUTPUT string for pass/fail counts. Returns plist :passed :failed :status."
  (let ((passed 0) (failed 0) (status 'unknown))
    (cond
     ;; pytest: "3 passed, 1 failed"
     ((string-match "\\([0-9]+\\) passed" output)
      (setq passed (string-to-number (match-string 1 output)))
      (when (string-match "\\([0-9]+\\) failed" output)
        (setq failed (string-to-number (match-string 1 output))))
      (setq status (if (= failed 0) 'passed 'failed)))
     ;; cargo test: "test result: ok. 5 passed; 0 failed"
     ((string-match "test result: \\(ok\\|FAILED\\)\\. \\([0-9]+\\) passed; \\([0-9]+\\) failed" output)
      (setq status  (if (string= (match-string 1 output) "ok") 'passed 'failed)
            passed  (string-to-number (match-string 2 output))
            failed  (string-to-number (match-string 3 output))))
     ;; go test: "--- FAIL" / "ok"
     ((string-match-p "^ok " output)
      (setq status 'passed
            passed (length (cl-remove-if-not
                            (lambda (l) (string-match-p "^--- PASS" l))
                            (split-string output "\n")))))
     ((string-match-p "--- FAIL" output)
      (setq status 'failed
            failed (length (cl-remove-if-not
                            (lambda (l) (string-match-p "^--- FAIL" l))
                            (split-string output "\n")))))
     ;; jest/vitest: "Tests: 3 passed, 1 failed"
     ((string-match "Tests:.*?\\([0-9]+\\) passed" output)
      (setq passed (string-to-number (match-string 1 output)))
      (when (string-match "\\([0-9]+\\) failed" output)
        (setq failed (string-to-number (match-string 1 output))))
      (setq status (if (= failed 0) 'passed 'failed)))
     ;; rspec: "5 examples, 0 failures"
     ;; FIX B: group 1 = total examples, group 2 = failures.
     ;; passed = total - failures (not total, which was the previous wrong value).
     ((string-match "\\([0-9]+\\) examples?, \\([0-9]+\\) failures?" output)
      (let ((total (string-to-number (match-string 1 output))))
        (setq failed (string-to-number (match-string 2 output))
              passed (- total failed)
              status (if (= failed 0) 'passed 'failed))))
     ;; ERT: "Ran 5 tests, 0 failures"
     ((string-match "Ran \\([0-9]+\\) tests?, \\([0-9]+\\) failures?" output)
      (setq passed (string-to-number (match-string 1 output))
            failed (string-to-number (match-string 2 output))
            status (if (= failed 0) 'passed 'failed)))
     ;; Generic: look for "error" / "fail" keywords
     ((string-match-p "\\(ERROR\\|FAILED\\|FAILURE\\)" output)
      (setq status 'failed))
     ((string-match-p "\\(SUCCESS\\|PASS\\|OK\\)" output)
      (setq status 'passed)))
    (list :passed passed :failed failed :status status)))

;; ============================================================================
;; TEST COMPILATION SENTINEL
;; ============================================================================
(defun emacs-ide-test--sentinel (proc event)
  "Handle test process completion PROC EVENT."
  (when (memq (process-status proc) '(exit signal))
    (let* ((buf    (process-buffer proc))
           (output (when (buffer-live-p buf)
                     (with-current-buffer buf (buffer-string))))
           (result (when output (emacs-ide-test--parse-results output)))
           (duration (when emacs-ide-test--start-time
                       (float-time (time-subtract (current-time)
                                                  emacs-ide-test--start-time))))
           (status (plist-get result :status))
           (passed (plist-get result :passed))
           (failed (plist-get result :failed))
           (icon   (cond ((eq status 'passed) "✓")
                         ((eq status 'failed) "✗")
                         (t "?"))))
      ;; Append summary to buffer
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\n%s═══ Test Run Complete: %s | %d passed %d failed | %.2fs ═══\n"
                            icon
                            (upcase (symbol-name (or status 'unknown)))
                            (or passed 0) (or failed 0)
                            (or duration 0))))))
      ;; Record in history
      (push (list :time      (current-time)
                  :command   emacs-ide-test--last-command
                  :directory emacs-ide-test--last-directory
                  :status    status
                  :passed    (or passed 0)
                  :failed    (or failed 0)
                  :duration  (or duration 0))
            emacs-ide-test--history)
      (when (> (length emacs-ide-test--history) emacs-ide-test--history-max)
        (setq emacs-ide-test--history
              (cl-subseq emacs-ide-test--history 0 emacs-ide-test--history-max)))
      ;; Echo result
      (message "Tests: %s %d passed, %d failed (%.2fs)"
               icon (or passed 0) (or failed 0) (or duration 0)))))

;; ============================================================================
;; RUN ENGINE
;; ============================================================================
(defun emacs-ide-test--run-command (command directory description)
  "Run COMMAND in DIRECTORY, display in test buffer with DESCRIPTION header."
  (setq emacs-ide-test--last-command  command
        emacs-ide-test--last-directory directory
        emacs-ide-test--start-time   (current-time))

  (let* ((buf (get-buffer-create emacs-ide-test--buffer-name))
         (proc (start-process-shell-command "emacs-ide-test" buf command)))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "══ %s ══\n" description))
        (insert (format "Command:   %s\n" command))
        (insert (format "Directory: %s\n" directory))
        (insert (format "Started:   %s\n\n" (format-time-string "%H:%M:%S")))
        (compilation-mode)
        (setq-local compilation-scroll-output 'first-error)))

    (set-process-sentinel proc #'emacs-ide-test--sentinel)

    ;; Show the buffer without stealing focus
    (display-buffer buf
                    '((display-buffer-reuse-window
                       display-buffer-in-side-window)
                      (side . bottom)
                      (window-height . 0.3)))

    (message "▶ Running: %s" description)))

;; ============================================================================
;; TEST AT POINT
;; ============================================================================
(defun emacs-ide-test--test-name-at-point ()
  "Try to get the name of the test function at point."
  (save-excursion
    (let ((mode major-mode))
      (cond
       ;; Python: def test_xxx
       ((memq mode '(python-mode python-ts-mode))
        (when (re-search-backward "^\\s-*def \\(test[_a-zA-Z0-9]+\\)" nil t)
          (match-string 1)))
       ;; Rust: fn test_xxx / #[test]
       ((memq mode '(rust-mode rust-ts-mode))
        (when (re-search-backward "fn \\(test[_a-zA-Z0-9]+\\)" nil t)
          (match-string 1)))
       ;; Go: func TestXxx
       ((memq mode '(go-mode go-ts-mode))
        (when (re-search-backward "func \\(Test[A-Za-z0-9_]+\\)" nil t)
          (match-string 1)))
       ;; JS/TS: it('xxx') / test('xxx') / describe('xxx')
       ((memq mode '(js2-mode js-mode typescript-mode javascript-ts-mode typescript-ts-mode))
        (when (re-search-backward "\\(?:it\\|test\\|describe\\)(['\"]\\([^'\"]+\\)['\"]" nil t)
          (match-string 1)))
       ;; Ruby: it 'xxx' / def test_xxx
       ((memq mode '(ruby-mode ruby-ts-mode))
        (when (re-search-backward "\\(?:it\\|def test_\\)[ '\"]\\([^'\"]+\\)" nil t)
          (match-string 1)))
       (t nil)))))

(defun emacs-ide-test--run-at-point-command ()
  "Build command to run test at point, or nil."
  (let ((name (emacs-ide-test--test-name-at-point))
        (root (emacs-ide-test--project-root))
        (file (buffer-file-name)))
    (when name
      (cond
       ((memq major-mode '(python-mode python-ts-mode))
        (when (executable-find "pytest")
          (cons (format "pytest -v --tb=short -k %s %s 2>&1"
                        (shell-quote-argument name)
                        (shell-quote-argument file))
                (format "pytest -k %s" name))))
       ((memq major-mode '(rust-mode rust-ts-mode))
        (when (executable-find "cargo")
          (cons (format "cd %s && cargo test %s 2>&1"
                        (shell-quote-argument root) name)
                (format "cargo test %s" name))))
       ((memq major-mode '(go-mode go-ts-mode))
        (when (executable-find "go")
          (cons (format "cd %s && go test ./... -run %s -v 2>&1"
                        (shell-quote-argument root) name)
                (format "go test -run %s" name))))
       ((memq major-mode '(js2-mode js-mode typescript-mode javascript-ts-mode typescript-ts-mode))
        (when (executable-find "npx")
          (cons (format "cd %s && npx jest --testNamePattern=%s --verbose 2>&1"
                        (shell-quote-argument root)
                        (shell-quote-argument name))
                (format "jest -t %s" name))))
       (t nil)))))

;; ============================================================================
;; PUBLIC COMMANDS
;; ============================================================================

(defun emacs-ide-test-run ()
  "Run tests for the current buffer's language.
Detects the framework automatically. C-c C-t."
  (interactive)
  (let ((detected (emacs-ide-test--detect)))
    (if detected
        (emacs-ide-test--run-command (car detected)
                                     (emacs-ide-test--project-root)
                                     (cdr detected))
      (message "⚠️  No test runner detected for %s. Install pytest/cargo/go/jest/etc."
               (symbol-name major-mode)))))

(defun emacs-ide-test-run-all ()
  "Always run the full test suite, ignoring any file/function context.
C-c C-T."
  (interactive)
  (emacs-ide-test-run))

(defun emacs-ide-test-run-point ()
  "Run the single test at point, if the framework supports it.
C-c t p."
  (interactive)
  (let ((cmd (emacs-ide-test--run-at-point-command)))
    (if cmd
        (emacs-ide-test--run-command (car cmd)
                                     (or (and (buffer-file-name)
                                              (file-name-directory (buffer-file-name)))
                                         (emacs-ide-test--project-root))
                                     (cdr cmd))
      (message "⚠️  No test at point detected (or framework unsupported). Falling back to full suite.")
      (emacs-ide-test-run))))

(defun emacs-ide-test-run-last ()
  "Repeat the last test command. C-c t l."
  (interactive)
  (if (and emacs-ide-test--last-command emacs-ide-test--last-directory)
      (emacs-ide-test--run-command emacs-ide-test--last-command
                                   emacs-ide-test--last-directory
                                   "Re-run: last test")
    (message "⚠️  No previous test command recorded.")))

(defun emacs-ide-test-report ()
  "Show test run history. C-c t r."
  (interactive)
  (if (null emacs-ide-test--history)
      (message "No test runs recorded yet.")
    (with-output-to-temp-buffer "*Test History*"
      (princ "═══ TEST RUN HISTORY ═══\n\n")
      (let ((i 1))
        (dolist (entry emacs-ide-test--history)
          (let* ((status   (plist-get entry :status))
                 (icon     (cond ((eq status 'passed) "✓")
                                 ((eq status 'failed) "✗")
                                 (t "?")))
                 (time-str (format-time-string "%Y-%m-%d %H:%M:%S"
                                               (plist-get entry :time))))
            (princ (format "%2d. %s [%s] %d✓ %d✗  %.2fs\n"
                           i icon time-str
                           (plist-get entry :passed)
                           (plist-get entry :failed)
                           (plist-get entry :duration)))
            (princ (format "    %s\n\n" (plist-get entry :command))))
          (cl-incf i)))
      (let* ((total   (length emacs-ide-test--history))
             (wins    (cl-count 'passed emacs-ide-test--history :key (lambda (e) (plist-get e :status))))
             (losses  (cl-count 'failed emacs-ide-test--history :key (lambda (e) (plist-get e :status)))))
        (princ (format "Summary: %d runs | %d passed | %d failed\n"
                       total wins losses))))))

;; ============================================================================
;; HYDRA
;; ============================================================================
(use-package hydra
  :config
  (defhydra hydra-test (:color teal :hint nil)
    "
^Run^                  ^Navigate^          ^Info^
^^^^^------------------------------------------------------
_r_: run tests         _p_: test at point  _h_: history
_a_: run all           _l_: repeat last    _q_: quit
"
    ("r" emacs-ide-test-run)
    ("a" emacs-ide-test-run-all)
    ("p" emacs-ide-test-run-point)
    ("l" emacs-ide-test-run-last)
    ("h" emacs-ide-test-report)
    ("q" nil "quit" :color blue))
)  ; end hydra use-package

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================
;; C-c t is bound to vterm by tools-terminal.el — do not use it as a prefix.
;; C-c T (uppercase) is unoccupied and used for the test sub-map.
;; C-c t  = vterm (tools-terminal.el) — plain command, cannot be a prefix
;; C-c T  = vterm-other-window (tools-terminal.el) — also taken
;; C-c x  = free across all modules — used as the test sub-prefix
(global-set-key (kbd "C-c C-t") 'emacs-ide-test-run)
(global-set-key (kbd "C-c C-T") 'emacs-ide-test-run-all)
(global-set-key (kbd "C-c x p") 'emacs-ide-test-run-point)
(global-set-key (kbd "C-c x l") 'emacs-ide-test-run-last)
(global-set-key (kbd "C-c x r") 'emacs-ide-test-report)
(global-set-key (kbd "C-c x h") 'hydra-test/body)

;; ============================================================================
;; INIT.EL REGISTRATION REMINDER
;; ============================================================================
;; Add "tools-test" to emacs-ide-feature-modules in init.el, after lang-core:
;;
;;   "tools-test"     ; Language-aware test runner  <-- add this line
;;   "keybindings"    ; always last
;;

(provide 'tools-test)
;;; tools-test.el ends here
