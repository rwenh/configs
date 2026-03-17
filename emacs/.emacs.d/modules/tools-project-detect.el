;;; tools-project-detect.el --- Project Type Detection & Lang Pre-warming -*- lexical-binding: t -*-
;;; Commentary:
;;; Reads project root markers (Cargo.toml, package.json, go.mod, etc.)
;;; and config.yml languages section to pre-warm only the lang tiers the
;;; user has enabled.  Called by projectile-after-switch-project-hook and
;;; find-file-hook (fallback: extension-based lazy load).
;;;
;;; RECALIBRATED 3.0.0:
;;;   - Detection validation: All marker lookups now verify file existence
;;;     before applying pre-warm logic.
;;;   - Cache guard: Hash table checks added before maphash operations.
;;;   - Idle timer safety: Pre-warm triggers deferred to 0.5s idle to avoid
;;;     startup blocking. Load failures logged but don't abort further langs.
;;;   - Extension fallback: Only triggers when NO project markers found
;;;     (prevents duplicate loads in mixed scenarios).
;;;
;;; Version: 3.0.0 (RECALIBRATED)
;;; Code:

(require 'cl-lib)
(require 'core-dev)

;; ============================================================================
;; PROJECT MARKER DEFINITIONS
;; Each entry: (MARKER-FILE . (LANG-KEY LANG-MODULE WARMUP-FN-OR-NIL))
;; ============================================================================

(defvar emacs-ide-detect--markers
  '(;; ── Tier 1 ──────────────────────────────────────────────────────────
    ("pyproject.toml"      . ("python"     "lang-python"     nil))
    ("setup.py"            . ("python"     "lang-python"     nil))
    ("setup.cfg"           . ("python"     "lang-python"     nil))
    ("requirements.txt"    . ("python"     "lang-python"     nil))
    ("Pipfile"             . ("python"     "lang-python"     nil))
    ("poetry.lock"         . ("python"     "lang-python"     nil))
    ("Cargo.toml"          . ("rust"       "lang-rust"       nil))
    ("go.mod"              . ("go"         "lang-go"         nil))
    ("go.sum"              . ("go"         "lang-go"         nil))
    ("package.json"        . ("javascript" "lang-web"        nil))
    ("tsconfig.json"       . ("typescript" "lang-web"        nil))
    ("next.config.js"      . ("typescript" "lang-web"        nil))
    ("vite.config.ts"      . ("typescript" "lang-web"        nil))
    ("CMakeLists.txt"      . ("c"          "lang-c"          nil))
    ("Makefile"            . ("c"          "lang-c"          nil))
    ("meson.build"         . ("c"          "lang-c"          nil))
    ;; ── Tier 2 ──────────────────────────────────────────────────────────
    ("pom.xml"             . ("java"       "lang-jvm"        nil))
    ("build.gradle"        . ("java"       "lang-jvm"        nil))
    ("build.gradle.kts"    . ("kotlin"     "lang-jvm"        nil))
    ("settings.gradle"     . ("java"       "lang-jvm"        nil))
    (".luarc.json"         . ("lua"        "lang-lua"        nil))
    ("rockspec"            . ("lua"        "lang-lua"        nil))
    ;; ── Tier 3 ────��─────────────────────────────────────────────────────
    ("project.clj"         . ("clojure"    "lang-clojure"    nil))
    ("deps.edn"            . ("clojure"    "lang-clojure"    nil))
    ("shadow-cljs.edn"     . ("clojure"    "lang-clojure"    nil))
    ("stack.yaml"          . ("haskell"    "lang-functional" nil))
    ("package.yaml"        . ("haskell"    "lang-functional" nil))
    ("cabal.project"       . ("haskell"    "lang-functional" nil))
    ("build.sbt"           . ("scala"      "lang-functional" nil))
    ("build.sc"            . ("scala"      "lang-functional" nil))
    ("flake.nix"           . ("nix"        "lang-systems"    nil))
    ("default.nix"         . ("nix"        "lang-systems"    nil))
    ("build.zig"           . ("zig"        "lang-systems"    nil))
    ("build.zig.zon"       . ("zig"        "lang-systems"    nil))
    ;; ── Tier 4 ──────────────────────────────────────────────────────────
    ("docker-compose.yml"  . ("yaml"       "lang-prose"      nil))
    ("Dockerfile"          . ("dockerfile" "lang-prose"      nil))
    (".terraform"          . ("terraform"  "lang-prose"      nil))
    ("terraform.tf"        . ("terraform"  "lang-prose"      nil))
    ("ansible.cfg"         . ("ansible"    "lang-prose"      nil)))
  "Alist of (MARKER-FILE . (LANG-KEY MODULE-NAME WARMUP-FN)) for project detection.")

;; ============================================================================
;; EXTENSION → LANG FALLBACK MAP
;; Used when no project markers are found (single files, scratch projects).
;; ============================================================================

(defvar emacs-ide-detect--ext-map
  '(("py"    . "python")
    ("pyw"   . "python")
    ("rs"    . "rust")
    ("go"    . "go")
    ("js"    . "javascript")
    ("mjs"   . "javascript")
    ("cjs"   . "javascript")
    ("ts"    . "typescript")
    ("tsx"   . "typescript")
    ("jsx"   . "javascript")
    ("c"     . "c")
    ("h"     . "c")
    ("cpp"   . "c")
    ("cc"    . "c")
    ("cxx"   . "c")
    ("hpp"   . "c")
    ("java"  . "java")
    ("kt"    . "kotlin")
    ("kts"   . "kotlin")
    ("scala" . "scala")
    ("sc"    . "scala")
    ("lua"   . "lua")
    ("sh"    . "shell")
    ("bash"  . "shell")
    ("zsh"   . "shell")
    ("fish"  . "shell")
    ("sql"   . "sql")
    ("r"     . "r")
    ("R"     . "r")
    ("jl"    . "julia")
    ("hs"    . "haskell")
    ("lhs"   . "haskell")
    ("clj"   . "clojure")
    ("cljs"  . "clojure")
    ("cljc"  . "clojure")
    ("zig"   . "zig")
    ("nix"   . "nix")
    ("rb"    . "ruby")
    ("ex"    . "elixir")
    ("exs"   . "elixir")
    ("php"   . "php")
    ("md"    . "prose")
    ("org"   . "prose")
    ("yml"   . "prose")
    ("yaml"  . "prose")
    ("toml"  . "prose")
    ("json"  . "prose"))
  "Alist of (EXTENSION . LANG-KEY) for fallback extension-based detection.")

;; ============================================================================
;; DETECTION CORE (RECALIBRATED)
;; ============================================================================

(defvar emacs-ide-detect--pre-warmed nil
  "Hash table of already pre-warmed lang keys to avoid redundant loads.")

(defun emacs-ide-detect--init-cache ()
  "Initialize pre-warm cache if needed (RECALIBRATED: added robustness)."
  (unless (hash-table-p emacs-ide-detect--pre-warmed)
    (setq emacs-ide-detect--pre-warmed (make-hash-table :test 'equal))))

(defun emacs-ide-detect--project-root ()
  "Return current project root or nil."
  (cond
   ((and (fboundp 'projectile-project-root)
         (projectile-project-root))
    (projectile-project-root))
   ((and (fboundp 'project-root)
         (project-current))
    (project-root (project-current)))
   (t nil)))

(defun emacs-ide-detect--scan-root (root)
  "Scan ROOT directory for marker files. Return list of matching lang-keys.
RECALIBRATED: Validates file existence before adding lang to result."
  (when (and root (file-directory-p root))
    (let (found)
      (dolist (entry emacs-ide-detect--markers)
        (let ((marker (car entry))
              (info   (cdr entry)))
          (let ((marker-path (expand-file-name marker root)))
            (when (file-exists-p marker-path)
              (push (car info) found)))))
      (delete-dups found))))

(defun emacs-ide-detect--module-for-lang (lang-key)
  "Return the module name for LANG-KEY from the markers table."
  (let ((entry (cl-find lang-key emacs-ide-detect--markers
                        :key (lambda (e) (car (cdr e)))
                        :test #'string=)))
    (when entry (cadr (cdr entry)))))

;; ============================================================================
;; PRE-WARMING (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-detect--prewarm-lang (lang-key)
  "Pre-warm lang module for LANG-KEY if enabled in config and not already done.
RECALIBRATED 3.0.0: Improved error handling and idle deferral."
  (emacs-ide-detect--init-cache)
  (when (and (emacs-ide-dev-lang-enabled-p lang-key)
             (not (gethash lang-key emacs-ide-detect--pre-warmed)))
    (puthash lang-key t emacs-ide-detect--pre-warmed)
    (let* ((module-name (or (emacs-ide-detect--module-for-lang lang-key)
                            (concat "lang-" lang-key)))
           (module-file (expand-file-name
                         (concat "langs/" module-name ".el")
                         emacs-ide-modules-dir)))
      (when (file-exists-p module-file)
        (run-with-idle-timer
         0.5 nil
         (lambda ()
           (condition-case err
               (progn
                 (load module-file nil t t)
                 (message "project-detect: pre-warmed %s" lang-key))
             (error
              (message "project-detect: failed to pre-warm %s: %s"
                       lang-key err)))))))))

;; ============================================================================
;; PROJECT SWITCH HOOK (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-detect-on-project-switch ()
  "Called after switching projects. Scans root, pre-warms enabled lang tiers."
  (let* ((root  (emacs-ide-detect--project-root))
         (langs (emacs-ide-detect--scan-root root)))
    (when langs
      (message "project-detect: detected langs %s in %s"
               (mapconcat #'identity langs ", ")
               (abbreviate-file-name (or root "")))
      (dolist (lang langs)
        (emacs-ide-detect--prewarm-lang lang)))))

;; ============================================================================
;; FILE OPEN FALLBACK HOOK (RECALIBRATED)
;; Only triggers when no project markers found. Prevents double-warming.
;; ============================================================================

(defun emacs-ide-detect-on-find-file ()
  "Extension-based fallback. Fires for every file open.
RECALIBRATED 3.0.0: Only use extension fallback when no project markers matched.
This prevents redundant pre-warming in mixed marker/extension scenarios."
  (when buffer-file-name
    (let* ((root    (emacs-ide-detect--project-root))
           (markers (emacs-ide-detect--scan-root root))
           (ext     (file-name-extension buffer-file-name))
           (lang    (cdr (assoc (downcase (or ext "")) emacs-ide-detect--ext-map))))
      ;; Only use extension fallback when no project markers matched
      (when (and lang (null markers))
        (emacs-ide-detect--prewarm-lang lang)))))

;; ============================================================================
;; HOOKS
;; ============================================================================

(add-hook 'projectile-after-switch-project-hook
          #'emacs-ide-detect-on-project-switch)

;; Also fire on project.el project switch
(add-hook 'project-switch-project-hook
          #'emacs-ide-detect-on-project-switch)

;; Extension fallback: fires on find-file but only pre-warms once per lang
(add-hook 'find-file-hook #'emacs-ide-detect-on-find-file)

;; ============================================================================
;; INTERACTIVE COMMANDS (RECALIBRATED)
;; ============================================================================

(defun emacs-ide-detect-current-project ()
  "Display detected languages for the current project."
  (interactive)
  (let* ((root  (emacs-ide-detect--project-root))
         (langs (emacs-ide-detect--scan-root root)))
    (if langs
        (message "Detected: %s in %s"
                 (mapconcat #'identity langs ", ")
                 (abbreviate-file-name (or root "no root")))
      (message "No project markers found — extension fallback active"))))

(defun emacs-ide-detect-reset-cache ()
  "Clear pre-warm cache. Forces re-detection on next project switch."
  (interactive)
  (setq emacs-ide-detect--pre-warmed nil)
  (message "project-detect: cache cleared"))

(defun emacs-ide-detect-show-status ()
  "Show all pre-warmed languages and registered lang modules.
RECALIBRATED: Added hash table existence guard before maphash."
  (interactive)
  (with-output-to-temp-buffer "*Project Detect Status*"
    (princ "=== PROJECT DETECT STATUS (v3.0.0) ===\n\n")
    (princ (format "Project root:  %s\n"
                   (or (emacs-ide-detect--project-root) "none")))
    (let ((langs (emacs-ide-detect--scan-root (emacs-ide-detect--project-root))))
      (princ (format "Detected langs: %s\n\n"
                     (if langs (mapconcat #'identity langs ", ") "none"))))
    (princ "Pre-warmed:\n")
    (when (hash-table-p emacs-ide-detect--pre-warmed)
      (if (> (hash-table-count emacs-ide-detect--pre-warmed) 0)
          (maphash (lambda (k _)
                     (princ (format "  ✓ %s\n" k)))
                   emacs-ide-detect--pre-warmed)
        (princ "  (none yet)\n")))
    (princ "\nRegistered lang modules:\n")
    (if (fboundp 'emacs-ide-dev-registered-langs)
        (dolist (lang (emacs-ide-dev-registered-langs))
          (let ((enabled (emacs-ide-dev-lang-enabled-p lang)))
            (princ (format "  %s %s\n" (if enabled "✓" "✗") lang))))
      (princ "  (core-dev not loaded)\n"))))

(provide 'tools-project-detect)
;;; tools-project-detect.el ends here
