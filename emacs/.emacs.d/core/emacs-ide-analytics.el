;;; emacs-ide-analytics.el --- Code Analytics -*- lexical-binding: t -*-
;;;
;;; Version: 3.5.0
;;; Code:

(require 'cl-lib)

;; Use ide-common's canonical project-root and shell helpers when available.
(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory) t))

;;;; ── Config helpers ──────────────────────────────────────────────────────────

(defun emacs-ide-analytics--cfg (key &optional default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'analytics key default)
    default))

(defun emacs-ide-analytics--project-root ()
  ;; Prefer ide-common's centralised implementation; fall back to inline.
  (if (fboundp 'ide-common-project-root)
      (ide-common-project-root)
    (or (and (fboundp 'projectile-project-root)
             (ignore-errors (projectile-project-root)))
        default-directory)))

(defun emacs-ide-analytics--exclude-args ()
  "Return shell-exclude flags appropriate for the current exclude-dirs config."
  (let ((dirs (or (emacs-ide-analytics--cfg 'exclude-dirs nil)
                  '(".git" "node_modules" "vendor" "__pycache__"
                    "target" "build" "dist" ".venv" "venv"))))
    (mapconcat (lambda (d) (format "--exclude-dir=%s" (shell-quote-argument d)))
               dirs " ")))

;;;; ── Utility ─────────────────────────────────────────────────────────────────

(defun emacs-ide-analytics--shell-lines (cmd)
  "Run CMD and return trimmed non-empty lines, or nil.
Delegates to `ide-common-shell-command-to-lines' when available."
  (if (fboundp 'ide-common-shell-command-to-lines)
      (ide-common-shell-command-to-lines cmd)
    (with-temp-buffer
      (when (= 0 (call-process shell-file-name nil t nil shell-command-switch cmd))
        (cl-remove-if #'string-empty-p
                      (mapcar #'string-trim (split-string (buffer-string) "\n")))))))

(defun emacs-ide-analytics--format-bytes (bytes)
  "Delegates to `ide-common-format-bytes' when available."
  (if (fboundp 'ide-common-format-bytes)
      (ide-common-format-bytes bytes)
    (cond
     ((< bytes 1024)              (format "%dB"     bytes))
     ((< bytes (* 1024 1024))     (format "%.1fKB"  (/ bytes 1024.0)))
     ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes (* 1024.0 1024.0))))
     (t                            (format "%.2fGB" (/ bytes (* 1024.0 1024.0 1024.0)))))))

;;;; ── LOC — 29-language registry ─────────────────────────────────────────────

(defconst emacs-ide-analytics--lang-extensions
  '(("Python"      . ("py" "pyw"))
    ("JavaScript"  . ("js" "mjs" "cjs"))
    ("TypeScript"  . ("ts" "tsx"))
    ("HTML"        . ("html" "htm" "jinja" "jinja2"))
    ("CSS"         . ("css" "scss" "sass" "less"))
    ("Rust"        . ("rs"))
    ("Go"          . ("go"))
    ("C"           . ("c" "h"))
    ("C++"         . ("cpp" "cc" "cxx" "hpp" "hxx"))
    ("C#"          . ("cs" "csx"))
    ("Java"        . ("java"))
    ("Kotlin"      . ("kt" "kts"))
    ("Scala"       . ("scala" "sc"))
    ("Groovy"      . ("groovy" "gradle"))
    ("Lua"         . ("lua"))
    ("Shell"       . ("sh" "bash" "zsh"))
    ("SQL"         . ("sql"))
    ("R"           . ("r"))
    ("Julia"       . ("jl"))
    ("Haskell"     . ("hs" "lhs"))
    ("Clojure"     . ("clj" "cljs" "cljc" "edn"))
    ("Elixir"      . ("ex" "exs"))
    ("Erlang"      . ("erl" "hrl"))
    ("OCaml"       . ("ml" "mli"))
    ("Zig"         . ("zig"))
    ("Nix"         . ("nix"))
    ("Ruby"        . ("rb" "rake" "gemspec"))
    ("PHP"         . ("php" "phtml"))
    ("Dart"        . ("dart")))
  "29-language extension map for the Elisp LOC counter fallback.")

(defun emacs-ide-analytics--count-files-by-lang (root)
  "Count files per language in ROOT using `emacs-ide-analytics--lang-extensions'."
  (let ((counts (make-hash-table :test 'equal))
        (exclude (or (emacs-ide-analytics--cfg 'exclude-dirs nil)
                     '(".git" "node_modules" "vendor" "__pycache__"
                       "target" "build" "dist" ".venv" "venv"))))
    (cl-labels ((scandir (dir)
                  (dolist (entry (ignore-errors (directory-files dir t nil t)))
                    (let ((base (file-name-nondirectory entry)))
                      (cond
                       ((member base '("." "..")))
                       ((file-directory-p entry)
                        (unless (member base exclude)
                          (scandir entry)))
                       (t
                        (let ((ext (downcase (or (file-name-extension entry) ""))))
                          (dolist (lang-entry emacs-ide-analytics--lang-extensions)
                            (when (member ext (cdr lang-entry))
                              (puthash (car lang-entry)
                                       (1+ (gethash (car lang-entry) counts 0))
                                       counts))))))))))
      (scandir root))
    counts))

(defun emacs-ide-analytics-loc ()
  "Show lines-of-code breakdown.  Prefers tokei → cloc → Elisp file count."
  (interactive)
  (let ((root (emacs-ide-analytics--project-root))
        (tool (emacs-ide-analytics--cfg 'loc-tool 'auto)))
    (with-output-to-temp-buffer "*LOC Analysis*"
      (princ (format "=== LOC ANALYSIS — %s ===\n\n"
                     (abbreviate-file-name root)))
      (cond
       ((and (not (eq tool 'elisp))
             (executable-find "tokei"))
        (princ "Tool: tokei\n\n")
        (dolist (l (emacs-ide-analytics--shell-lines
                     (format "tokei %s" (shell-quote-argument root))))
          (princ l) (princ "\n")))
       ((and (not (eq tool 'elisp))
             (executable-find "cloc"))
        (princ "Tool: cloc\n\n")
        (dolist (l (emacs-ide-analytics--shell-lines
                     (format "cloc --quiet %s" (shell-quote-argument root))))
          (princ l) (princ "\n")))
       (t
        (princ "Tool: Elisp file count (install tokei or cloc for LOC counts)\n\n")
        (let ((counts (emacs-ide-analytics--count-files-by-lang root))
              (total  0))
          (let (langs)
            (maphash (lambda (k v) (push (cons k v) langs)) counts)
            (setq langs (sort langs (lambda (a b) (> (cdr a) (cdr b)))))
            (dolist (entry langs)
              (princ (format "  %-16s %d files\n" (car entry) (cdr entry)))
              (cl-incf total (cdr entry))))
          (princ (format "\nTotal: %d source files\n" total))))))))

;;;; ── Git stats ───────────────────────────────────────────────────────────────

(defun emacs-ide-analytics--bar (count max-count width)
  "Return an ASCII bar of WIDTH proportional to COUNT/MAX-COUNT."
  (let ((n (if (zerop max-count) 0
             (round (* width (/ (float count) max-count))))))
    (concat (make-string n ?█) (make-string (- width n) ?░))))

(defun emacs-ide-analytics-git-stats ()
  "Show git commit counts, top contributors, and a 12-week activity chart."
  (interactive)
  (let ((root (emacs-ide-analytics--project-root)))
    (unless (file-directory-p (expand-file-name ".git" root))
      (user-error "Not a git repository: %s" root))
    (let* ((default-directory root))
      (with-output-to-temp-buffer "*Git Stats*"
        (princ (format "=== GIT STATS — %s ===\n\n"
                       (abbreviate-file-name root)))

        ;; Commit totals
        (let ((total  (car (emacs-ide-analytics--shell-lines
                             "git rev-list --count HEAD 2>/dev/null")))
              (last7  (car (emacs-ide-analytics--shell-lines
                             "git rev-list --count --since='7 days ago' HEAD 2>/dev/null")))
              (last30 (car (emacs-ide-analytics--shell-lines
                             "git rev-list --count --since='30 days ago' HEAD 2>/dev/null"))))
          (princ (format "Total commits:   %s\n" (or total "?")))
          (princ (format "Last 7 days:     %s\n" (or last7  "?")))
          (princ (format "Last 30 days:    %s\n\n" (or last30 "?"))))

        ;; Top contributors
        (princ "Top contributors:\n")
        (dolist (l (seq-take
                    (emacs-ide-analytics--shell-lines
                     "git shortlog -sn --no-merges HEAD 2>/dev/null")
                    10))
          (princ (format "  %s\n" l)))

        ;; 12-week activity bar chart
        (princ "\nWeekly commit activity (last 12 weeks):\n\n")
        (let* ((week-counts
                (cl-loop for w from 11 downto 0
                         collect
                         (let* ((since (format "%d weeks ago" (1+ w)))
                                (until (if (zerop w) "now" (format "%d weeks ago" w)))
                                (n (car (emacs-ide-analytics--shell-lines
                                          (format "git rev-list --count --since='%s' --until='%s' HEAD 2>/dev/null"
                                                  since until)))))
                           (string-to-number (or n "0")))))
               (max-count (apply #'max (cons 1 week-counts))))
          (cl-loop for n in week-counts
                   for w from 11 downto 0
                   do (princ (format "  wk-%-2d %s %d\n"
                                     w (emacs-ide-analytics--bar n max-count 30) n))))

        ;; Most changed files
        (princ "\nMost changed files (last 90 days):\n")
        (dolist (l (seq-take
                    (emacs-ide-analytics--shell-lines
                     "git log --since='90 days ago' --name-only --format='' 2>/dev/null | sort | uniq -c | sort -rn 2>/dev/null")
                    10))
          (princ (format "  %s\n" l)))))))

;;;; ── Coverage reader ─────────────────────────────────────────────────────────

(defun emacs-ide-analytics--read-python-coverage (root)
  "Try to read pytest-cov's coverage.xml in ROOT."
  (let ((f (expand-file-name "coverage.xml" root)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (when (re-search-forward "line-rate=\"\\([0-9.]+\\)\"" nil t)
          (format "%.1f%%" (* 100 (string-to-number (match-string 1)))))))))

(defun emacs-ide-analytics--read-jest-coverage (root)
  "Try to read jest's coverage-summary.json in ROOT."
  (let ((f (expand-file-name "coverage/coverage-summary.json" root)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (when (re-search-forward "\"lines\":{\"total\":[0-9]+,\"covered\":\\([0-9]+\\),\"skipped\":[0-9]+,\"pct\":\\([0-9.]+\\)}" nil t)
          (format "%.1f%%" (string-to-number (match-string 2))))))))

(defun emacs-ide-analytics--read-go-coverage (root)
  "Try to read go test cover.out in ROOT."
  (let ((f (expand-file-name "cover.out" root)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-max))
        (when (re-search-backward "total:.*?\\([0-9.]+\\)%" nil t)
          (format "%s%%" (match-string 1)))))))

(defun emacs-ide-analytics--read-cargo-coverage (root)
  "Try to read cargo-tarpaulin's tarpaulin-report.json in ROOT."
  (let ((f (expand-file-name "tarpaulin-report.json" root)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (when (re-search-forward "\"covered_lines\":[[:space:]]*\\([0-9]+\\)" nil t)
          (let ((covered (string-to-number (match-string 1))))
            (when (re-search-forward "\"total_lines\":[[:space:]]*\\([0-9]+\\)" nil t)
              (let ((total (string-to-number (match-string 1))))
                (when (> total 0)
                  (format "%.1f%%" (* 100 (/ (float covered) total))))))))))))

(defun emacs-ide-analytics-coverage ()
  "Show test coverage from saved report files (no test re-runs)."
  (interactive)
  (let ((root (emacs-ide-analytics--project-root)))
    (with-output-to-temp-buffer "*Coverage Report*"
      (princ (format "=== COVERAGE — %s ===\n\n"
                     (abbreviate-file-name root)))
      (princ "Reading saved report files (no tests re-run):\n\n")
      (let ((found nil))
        (let ((pct (emacs-ide-analytics--read-python-coverage root)))
          (when pct (princ (format "  Python  (coverage.xml):              %s\n" pct))
                    (setq found t)))
        (let ((pct (emacs-ide-analytics--read-jest-coverage root)))
          (when pct (princ (format "  Jest    (coverage/coverage-summary.json): %s\n" pct))
                    (setq found t)))
        (let ((pct (emacs-ide-analytics--read-go-coverage root)))
          (when pct (princ (format "  Go      (cover.out):                  %s\n" pct))
                    (setq found t)))
        (let ((pct (emacs-ide-analytics--read-cargo-coverage root)))
          (when pct (princ (format "  Rust    (tarpaulin-report.json):      %s\n" pct))
                    (setq found t)))
        (unless found
          (princ "  No coverage report files found.\n\n")
          (princ "  Generate them with:\n")
          (princ "    Python: pytest --cov=. --cov-report=xml\n")
          (princ "    Jest:   jest --coverage\n")
          (princ "    Go:     go test -coverprofile=cover.out ./...\n")
          (princ "    Rust:   cargo tarpaulin --out json\n"))))))

;;;; ── Complexity ──────────────────────────────────────────────────────────────

(defun emacs-ide-analytics--long-functions (root threshold)
  "Return list of (file func line count) for functions longer than THRESHOLD lines."
  (let ((results nil)
        (exclude (or (emacs-ide-analytics--cfg 'exclude-dirs nil)
                     '(".git" "node_modules" "vendor"))))
    (cl-labels ((scan-file (file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (goto-char (point-min))
                    (let ((func-start nil) (func-name nil))
                      (while (not (eobp))
                        (let ((line (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position))))
                          (when (string-match-p "^def \\|^function \\|^func \\|^fn \\|^sub " line)
                            (when (and func-start
                                       (> (- (line-number-at-pos) func-start) threshold))
                              (push (list file func-name func-start
                                          (- (line-number-at-pos) func-start))
                                    results))
                            (setq func-start (line-number-at-pos)
                                  func-name (string-trim line))))
                        (forward-line 1)))))
               (scandir (dir)
                 (dolist (f (ignore-errors (directory-files dir t nil t)))
                   (let ((base (file-name-nondirectory f)))
                     (cond
                      ((member base '("." "..")))
                      ((file-directory-p f)
                       (unless (member base exclude) (scandir f)))
                      ((member (file-name-extension f)
                               '("py" "js" "ts" "go" "rs" "rb" "php" "lua"))
                       (scan-file f)))))))
      (scandir root))
    (sort results (lambda (a b) (> (nth 3 a) (nth 3 b))))))

(defun emacs-ide-analytics-complexity ()
  "Show code complexity — radon/gocyclo/eslint if available, heuristic fallback."
  (interactive)
  (let ((root (emacs-ide-analytics--project-root)))
    (with-output-to-temp-buffer "*Complexity Analysis*"
      (princ (format "=== COMPLEXITY — %s ===\n\n" (abbreviate-file-name root)))
      (let ((found nil))
        (when (executable-find "radon")
          (setq found t)
          (princ "Python complexity (radon cc):\n")
          (dolist (l (seq-take
                      (emacs-ide-analytics--shell-lines
                       (format "radon cc -a -s %s 2>/dev/null"
                               (shell-quote-argument root)))
                      20))
            (princ (format "  %s\n" l)))
          (princ "\n"))
        (when (executable-find "gocyclo")
          (setq found t)
          (princ "Go complexity (gocyclo):\n")
          (dolist (l (seq-take
                      (emacs-ide-analytics--shell-lines
                       (format "gocyclo -over 10 %s 2>/dev/null"
                               (shell-quote-argument root)))
                      20))
            (princ (format "  %s\n" l)))
          (princ "\n"))
        (unless found
          (princ "Heuristic: long functions (> 50 lines):\n\n")
          (let ((long-fns (seq-take
                           (emacs-ide-analytics--long-functions root 50)
                           20)))
            (if long-fns
                (dolist (entry long-fns)
                  (princ (format "  %s:%d  %s  (%d lines)\n"
                                 (file-relative-name (nth 0 entry) root)
                                 (nth 2 entry)
                                 (truncate-string-to-width (nth 1 entry) 40 nil nil "…")
                                 (nth 3 entry))))
              (princ "  No functions > 50 lines found.\n"))))))))

;;;; ── Dependency audit ────────────────────────────────────────────────────────

(defun emacs-ide-analytics-deps-audit ()
  "Show outdated packages for npm/cargo/pip/bundler/go."
  (interactive)
  (let ((root (emacs-ide-analytics--project-root)))
    (with-output-to-temp-buffer "*Dependency Audit*"
      (princ (format "=== DEPENDENCY AUDIT — %s ===\n\n"
                     (abbreviate-file-name root)))
      (let ((default-directory root)
            (any nil))
        (when (and (file-exists-p "package.json") (executable-find "npm"))
          (setq any t)
          (princ "npm outdated:\n")
          (dolist (l (or (emacs-ide-analytics--shell-lines "npm outdated 2>/dev/null")
                         '("  (all up to date)")))
            (princ (format "  %s\n" l)))
          (princ "\n"))
        (when (and (file-exists-p "Cargo.toml") (executable-find "cargo"))
          (setq any t)
          (princ "cargo outdated:\n")
          (if (emacs-ide-analytics--shell-lines "cargo outdated --help 2>/dev/null")
              (dolist (l (or (emacs-ide-analytics--shell-lines "cargo outdated 2>/dev/null")
                             '("  (all up to date)")))
                (princ (format "  %s\n" l)))
            (princ "  cargo-outdated not installed (cargo install cargo-outdated)\n"))
          (princ "\n"))
        (when (and (or (file-exists-p "requirements.txt")
                       (file-exists-p "pyproject.toml")
                       (file-exists-p "Pipfile"))
                   (executable-find "pip"))
          (setq any t)
          (princ "pip outdated:\n")
          (dolist (l (or (emacs-ide-analytics--shell-lines "pip list --outdated 2>/dev/null")
                         '("  (all up to date)")))
            (princ (format "  %s\n" l)))
          (princ "\n"))
        (when (and (file-exists-p "Gemfile") (executable-find "bundle"))
          (setq any t)
          (princ "bundle outdated:\n")
          (dolist (l (or (emacs-ide-analytics--shell-lines "bundle outdated 2>/dev/null")
                         '("  (all up to date)")))
            (princ (format "  %s\n" l)))
          (princ "\n"))
        (when (and (file-exists-p "go.mod") (executable-find "go"))
          (setq any t)
          (princ "go mod outdated:\n")
          (dolist (l (or (emacs-ide-analytics--shell-lines "go list -u -m all 2>/dev/null | grep '\\[' ")
                         '("  (all up to date)")))
            (princ (format "  %s\n" l)))
          (princ "\n"))
        (unless any
          (princ "No supported package managers detected.\n"))))))

;;;; ── Unified dashboard ───────────────────────────────────────────────────────

(defun emacs-ide-analytics-dashboard ()
  "Show a unified project health dashboard."
  (interactive)
  (let ((root (emacs-ide-analytics--project-root))
        (start (float-time)))
    (with-output-to-temp-buffer "*Project Dashboard*"
      (princ (format "=== PROJECT DASHBOARD — %s ===\n" (abbreviate-file-name root)))
      (princ (format "Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))

      ;; LOC quick count
      (princ "📁 Files by language:\n")
      (let ((counts (emacs-ide-analytics--count-files-by-lang root))
            total langs)
        (maphash (lambda (k v) (push (cons k v) langs)) counts)
        (setq langs (sort langs (lambda (a b) (> (cdr a) (cdr b)))))
        (dolist (entry (seq-take langs 8))
          (princ (format "  %-16s %d\n" (car entry) (cdr entry)))
          (cl-incf (or total 0) (cdr entry)))
        (princ (format "  %-16s %d\n\n" "TOTAL" (or total 0))))

      ;; Git quick stats
      (when (file-directory-p (expand-file-name ".git" root))
        (let ((default-directory root))
          (princ "📈 Git (last 30 days):\n")
          (let ((n (car (emacs-ide-analytics--shell-lines
                          "git rev-list --count --since='30 days ago' HEAD 2>/dev/null")))
                (branch (car (emacs-ide-analytics--shell-lines
                               "git branch --show-current 2>/dev/null"))))
            (princ (format "  Branch: %s | Commits/30d: %s\n\n"
                           (or branch "?") (or n "?"))))))

      ;; Coverage
      (princ "🧪 Coverage:\n")
      (let ((pct (or (emacs-ide-analytics--read-python-coverage root)
                     (emacs-ide-analytics--read-jest-coverage root)
                     (emacs-ide-analytics--read-go-coverage root)
                     (emacs-ide-analytics--read-cargo-coverage root))))
        (if pct
            (princ (format "  %s\n\n" pct))
          (princ "  No coverage report found. Run tests with --coverage first.\n\n")))

      ;; Health checklist
      (princ "✅ Project checklist:\n")
      (dolist (check `(("README"      . ,(cl-some (lambda (f)
                                                     (file-exists-p
                                                      (expand-file-name f root)))
                                                   '("README.md" "README.rst" "README.txt" "README")))
                        (".gitignore"  . ,(file-exists-p (expand-file-name ".gitignore" root)))
                        ("Tests dir"  . ,(cl-some (lambda (d)
                                                     (file-directory-p
                                                      (expand-file-name d root)))
                                                   '("tests" "test" "spec" "__tests__")))
                        ("CI config"  . ,(cl-some (lambda (f)
                                                     (file-exists-p
                                                      (expand-file-name f root)))
                                                   '(".github/workflows" ".gitlab-ci.yml"
                                                     ".circleci" "Jenkinsfile" ".travis.yml")))))
        (princ (format "  %s %s\n"
                       (if (cdr check) "✓" "✗")
                       (car check))))

      (princ (format "\nGenerated in %.2fs\n"
                     (- (float-time) start))))))

;;;; ── Status ──────────────────────────────────────────────────────────────────

(defun emacs-ide-analytics-status ()
  "Show analytics tool availability."
  (interactive)
  (with-output-to-temp-buffer "*Analytics Status*"
    (princ "=== ANALYTICS STATUS ===\n\n")
    (dolist (entry '(("tokei"          . "LOC counter (preferred)")
                     ("cloc"           . "LOC counter (fallback)")
                     ("radon"          . "Python complexity")
                     ("gocyclo"        . "Go complexity")
                     ("git"            . "Git stats")))
      (princ (format "  %s %-18s %s\n"
                     (if (executable-find (car entry)) "✓" "✗")
                     (car entry)
                     (cdr entry))))
    (princ "\nCommands:\n")
    (princ "  M-x emacs-ide-analytics-loc           LOC breakdown\n")
    (princ "  M-x emacs-ide-analytics-git-stats     Git activity chart\n")
    (princ "  M-x emacs-ide-analytics-coverage      Coverage from saved files\n")
    (princ "  M-x emacs-ide-analytics-complexity    Complexity analysis\n")
    (princ "  M-x emacs-ide-analytics-deps-audit    Outdated packages\n")
    (princ "  M-x emacs-ide-analytics-dashboard     Unified project dashboard\n")))

;;;; ── Hydra ───────────────────────────────────────────────────────────────────

(with-eval-after-load 'hydra
  (defhydra hydra-analytics (:hint nil :color blue)
    "
  analytics
  ──────────────────────────────────────────────
  _l_ LOC breakdown      _g_ git stats
  _c_ coverage (files)   _x_ complexity
  _d_ deps audit         _D_ unified dashboard
  _s_ status
  ──────────────────────────────────────────────
  _ESC_ quit
"
    ("l" emacs-ide-analytics-loc)
    ("g" emacs-ide-analytics-git-stats)
    ("c" emacs-ide-analytics-coverage)
    ("x" emacs-ide-analytics-complexity)
    ("d" emacs-ide-analytics-deps-audit)
    ("D" emacs-ide-analytics-dashboard)
    ("s" emacs-ide-analytics-status)
    ("ESC" nil)))

;;;; ── Keybindings ─────────────────────────────────────────────────────────────

(define-prefix-command 'emacs-ide-analytics-map)
(global-set-key (kbd "C-c A") 'emacs-ide-analytics-map)
(global-set-key (kbd "C-c A l") #'emacs-ide-analytics-loc)
(global-set-key (kbd "C-c A g") #'emacs-ide-analytics-git-stats)
(global-set-key (kbd "C-c A c") #'emacs-ide-analytics-coverage)
(global-set-key (kbd "C-c A x") #'emacs-ide-analytics-complexity)
(global-set-key (kbd "C-c A d") #'emacs-ide-analytics-deps-audit)
(global-set-key (kbd "C-c A D") #'emacs-ide-analytics-dashboard)
(global-set-key (kbd "C-c A s") #'emacs-ide-analytics-status)
(global-set-key (kbd "C-c h A")
  (lambda () (interactive)
    (if (fboundp 'hydra-analytics/body)
        (hydra-analytics/body)
      (message "hydra-analytics not loaded"))))

(provide 'emacs-ide-analytics)
;;; emacs-ide-analytics.el ends here
