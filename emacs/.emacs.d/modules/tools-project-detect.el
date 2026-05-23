;;; tools-project-detect.el --- Project Type Detection & Lang Pre-warming -*- lexical-binding: t -*-
;;; Version: 3.3.0

(require 'cl-lib)
(require 'core-dev)

;;;; ── Module directory ────────────────────────────────────────────────────────
(defvar emacs-ide-modules-dir
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing feature modules (modules/ and modules/langs/).")

;;;; ── Lang → module name overrides ───────────────────────────────────────────
(defvar emacs-ide-detect--lang-module-overrides
  '(("javascript" . "lang-web")
    ("typescript" . "lang-web")
    ("haskell" . "lang-functional")
    ("clojure" . "lang-functional")
    ("elixir" . "lang-functional")
    ("erlang" . "lang-functional")
    ("ocaml" . "lang-functional")
    ("java" . "lang-jvm")
    ("kotlin" . "lang-jvm")
    ("scala" . "lang-jvm")
    ("groovy" . "lang-jvm")
    ("zig" . "lang-systems")
    ("nix" . "lang-systems")
    ("d" . "lang-systems")
    ("v" . "lang-systems")
    ("r" . "lang-data")
    ("julia" . "lang-data")
    ("yaml" . "lang-prose")
    ("dockerfile" . "lang-prose")
    ("terraform" . "lang-prose")
    ("ansible" . "lang-prose"))
  "Alist of (LANG-KEY . MODULE-NAME) for languages whose module file does
not follow the standard lang-LANG-KEY.el naming convention.")

;;;; ── Project marker database ─────────────────────────────────────────────────
(defvar emacs-ide-detect--markers
  '(;; Python
    ("pyproject.toml" . ("python" "lang-python" nil))
    ("setup.py" . ("python" "lang-python" nil))
    ("setup.cfg" . ("python" "lang-python" nil))
    ("requirements.txt" . ("python" "lang-python" nil))
    ("Pipfile" . ("python" "lang-python" nil))
    ("poetry.lock" . ("python" "lang-python" nil))
    ;; Rust
    ("Cargo.toml" . ("rust" "lang-rust" nil))
    ;; Go
    ("go.mod" . ("go" "lang-go" nil))
    ("go.sum" . ("go" "lang-go" nil))
    ;; JavaScript / TypeScript
    ("package.json" . ("javascript" "lang-web" nil))
    ("tsconfig.json" . ("typescript" "lang-web" nil))
    ("next.config.js" . ("typescript" "lang-web" nil))
    ("next.config.ts" . ("typescript" "lang-web" nil))
    ("vite.config.ts" . ("typescript" "lang-web" nil))
    ("vite.config.js" . ("javascript" "lang-web" nil))
    ;; C / C++
    ("CMakeLists.txt" . ("c" "lang-c" nil))
    ("Makefile" . ("c" "lang-c" nil))
    ("meson.build" . ("c" "lang-c" nil))
    ;; JVM
    ("pom.xml" . ("java" "lang-jvm" nil))
    ("build.gradle" . ("java" "lang-jvm" nil))
    ("build.gradle.kts" . ("kotlin" "lang-jvm" nil))
    ("settings.gradle" . ("java" "lang-jvm" nil))
    ("gradlew" . ("java" "lang-jvm" nil))
    ;; Lua
    (".luarc.json" . ("lua" "lang-lua" nil))
    ("rockspec" . ("lua" "lang-lua" nil))
    ;; Clojure
    ("project.clj" . ("clojure" "lang-functional" nil))
    ("deps.edn" . ("clojure" "lang-functional" nil))
    ("shadow-cljs.edn" . ("clojure" "lang-functional" nil))
    ;; Haskell
    ("stack.yaml" . ("haskell" "lang-functional" nil))
    ("package.yaml" . ("haskell" "lang-functional" nil))
    ("cabal.project" . ("haskell" "lang-functional" nil))
    ;; Scala
    ("build.sbt" . ("scala" "lang-jvm" nil))
    ("build.sc" . ("scala" "lang-jvm" nil))
    ;; Nix
    ("flake.nix" . ("nix" "lang-systems" nil))
    ("default.nix" . ("nix" "lang-systems" nil))
    ;; Zig
    ("build.zig" . ("zig" "lang-systems" nil))
    ("build.zig.zon" . ("zig" "lang-systems" nil))
    ;; Prose / config
    ("docker-compose.yml" . ("yaml" "lang-prose" nil))
    ("docker-compose.yaml" . ("yaml" "lang-prose" nil))
    ("Dockerfile" . ("dockerfile" "lang-prose" nil))
    (".terraform" . ("terraform" "lang-prose" nil))
    ("terraform.tf" . ("terraform" "lang-prose" nil))
    ("ansible.cfg" . ("ansible" "lang-prose" nil)))
  "Alist of (MARKER-FILENAME . (LANG-KEY MODULE-NAME WARMUP-FN-OR-NIL)).")

;;;; ── Extension → language map ───────────────────────────────────────────────
(defvar emacs-ide-detect--ext-map
  '(("py" . "python") ("pyw" . "python")
    ("rs" . "rust")
    ("go" . "go")
    ("js" . "javascript") ("mjs" . "javascript") ("cjs" . "javascript") ("jsx" . "javascript")
    ("ts" . "typescript") ("tsx" . "typescript")
    ("c" . "c") ("h" . "c") ("cpp" . "c") ("cc" . "c") ("cxx" . "c")
    ("hpp" . "c") ("hxx" . "c") ("cu" . "c")
    ("java" . "java")
    ("kt" . "kotlin") ("kts" . "kotlin")
    ("scala" . "scala") ("sc" . "scala")
    ("lua" . "lua")
    ("sh" . "shell") ("bash" . "shell") ("zsh" . "shell") ("fish" . "shell")
    ("sql" . "sql")
    ("r" . "r")
    ("jl" . "julia")
    ("hs" . "haskell") ("lhs" . "haskell")
    ("clj" . "clojure") ("cljs" . "clojure") ("cljc" . "clojure")
    ("zig" . "zig") ("nix" . "nix")
    ("rb" . "ruby")
    ("ex" . "elixir") ("exs" . "elixir")
    ("php" . "php")
    ("md" . "prose") ("org" . "prose")
    ("yml" . "prose") ("yaml" . "prose") ("toml" . "prose") ("json" . "prose"))
  "Alist of (LOWERCASE-EXTENSION . LANG-KEY).")

;;;; ── Pre-warm cache ──────────────────────────────────────────────────────────
(defvar emacs-ide-detect--pre-warmed nil
  "Hash table mapping lang-key strings to t for successfully pre-warmed langs.")

(defun emacs-ide-detect--init-cache ()
  "Initialise the pre-warm cache hash table if not already done."
  (unless (hash-table-p emacs-ide-detect--pre-warmed)
    (setq emacs-ide-detect--pre-warmed (make-hash-table :test 'equal))))

;;;; ── Project root detection ─────────────────────────────────────────────────
(defun emacs-ide-detect--project-root ()
  "Return the current project root directory string, or nil."
  (cond
   ((and (fboundp 'projectile-project-root)
         (ignore-errors (projectile-project-root)))
    (ignore-errors (projectile-project-root)))
   ((and (fboundp 'project-root)
         (project-current))
    (project-root (project-current)))
   (t nil)))

;;;; ── Marker scan ─────────────────────────────────────────────────────────────
(defun emacs-ide-detect--scan-root (root)
  "Scan ROOT for project marker files. Return a deduplicated list of lang keys."
  (when (and root (file-directory-p root))
    (let (found)
      (dolist (entry emacs-ide-detect--markers)
        (when (file-exists-p (expand-file-name (car entry) root))
          (push (car (cdr entry)) found)))
      (delete-dups found))))

;;;; ── Module name resolution ──────────────────────────────────────────────────
(defun emacs-ide-detect--module-for-lang (lang-key)
  "Return the module name for LANG-KEY from the markers table, or nil."
  (let ((entry (cl-find lang-key emacs-ide-detect--markers
                        :key (lambda (e) (car (cdr e)))
                        :test #'string=)))
    (when entry (cadr (cdr entry)))))

(defun emacs-ide-detect--resolve-module-file (lang-key)
  "Return the full path to the .el file for LANG-KEY, or nil if not found."
  (let* ((module-name (or (cdr (assoc lang-key emacs-ide-detect--lang-module-overrides))
                          (emacs-ide-detect--module-for-lang lang-key)
                          (concat "lang-" lang-key)))
         (path (expand-file-name (concat "langs/" module-name ".el")
                                 emacs-ide-modules-dir)))
    (when (file-exists-p path) path)))

;;;; ── Pre-warm entry point ────────────────────────────────────────────────────
(defun emacs-ide-detect--prewarm-lang (lang-key)
  "Schedule pre-warming of the lang module for LANG-KEY on a 0.5s idle timer."
  (emacs-ide-detect--init-cache)
  (when (and (emacs-ide-dev-lang-enabled-p lang-key)
             (not (gethash lang-key emacs-ide-detect--pre-warmed)))
    (let ((module-file (emacs-ide-detect--resolve-module-file lang-key)))
      (when module-file
        (run-with-idle-timer
         0.5 nil
         (lambda ()
           (condition-case err
               (progn
                 (load module-file nil t t)
                 (puthash lang-key t emacs-ide-detect--pre-warmed)
                 (message "project-detect: pre-warmed %s" lang-key))
             (error
              (warn "project-detect: failed to pre-warm %s: %s"
                    lang-key (error-message-string err))))))))))

;;;; ── Event hooks ─────────────────────────────────────────────────────────────
(defun emacs-ide-detect-on-project-switch ()
  "Called on `projectile-after-switch-project-hook`."
  (let* ((root (emacs-ide-detect--project-root))
         (langs (emacs-ide-detect--scan-root root)))
    (when langs
      (message "project-detect: detected [%s] in %s"
               (mapconcat #'identity langs ", ")
               (abbreviate-file-name (or root "")))
      (dolist (lang langs)
        (emacs-ide-detect--prewarm-lang lang)))))

(defun emacs-ide-detect-on-find-file ()
  "Called on `find-file-hook`."
  (when buffer-file-name
    (let* ((root (emacs-ide-detect--project-root))
           (markers (emacs-ide-detect--scan-root root))
           (ext (file-name-extension buffer-file-name))
           (lang (cdr (assoc (downcase (or ext "")) emacs-ide-detect--ext-map))))
      (when (and lang (null markers))
        (emacs-ide-detect--prewarm-lang lang)))))

(add-hook 'projectile-after-switch-project-hook #'emacs-ide-detect-on-project-switch)
(add-hook 'find-file-hook #'emacs-ide-detect-on-find-file)

;;;; ── Interactive commands ────────────────────────────────────────────────────
(defun emacs-ide-detect-current-project ()
  "Show detected languages for the current project root."
  (interactive)
  (let* ((root (emacs-ide-detect--project-root))
         (langs (emacs-ide-detect--scan-root root)))
    (if langs
        (message "Detected: %s root: %s"
                 (mapconcat #'identity langs ", ")
                 (abbreviate-file-name (or root "no root")))
      (message "No project markers found in %s — extension fallback active"
               (abbreviate-file-name (or root "no root"))))))

(defun emacs-ide-detect-reset-cache ()
  "Clear the pre-warm cache."
  (interactive)
  (setq emacs-ide-detect--pre-warmed nil)
  (message "project-detect: pre-warm cache cleared"))

(defun emacs-ide-detect-show-status ()
  "Display a buffer summarising project detection state and pre-warmed langs."
  (interactive)
  (with-output-to-temp-buffer "*Project Detect Status*"
    (princ "=== PROJECT DETECT STATUS (v3.3.0) ===\n\n")
    (let ((root (emacs-ide-detect--project-root)))
      (princ (format "Project root: %s\n" (or root "none")))
      (let ((langs (emacs-ide-detect--scan-root root)))
        (princ (format "Detected langs: %s\n\n"
                       (if langs
                           (mapconcat #'identity langs ", ")
                         "none")))))
    (princ "Pre-warmed languages:\n")
    (if (hash-table-p emacs-ide-detect--pre-warmed)
        (if (zerop (hash-table-count emacs-ide-detect--pre-warmed))
            (princ " (none yet)\n")
          (let (keys)
            (maphash (lambda (k _) (push k keys))
                     emacs-ide-detect--pre-warmed)
            (dolist (k (sort keys #'string<))
              (princ (format " ✓ %s\n" k)))))
      (princ " (cache not initialised — no file opened yet)\n"))))

(provide 'tools-project-detect)
;;; tools-project-detect.el ends here
