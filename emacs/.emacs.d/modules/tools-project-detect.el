;;; tools-project-detect.el --- Project Type Detection & Lang Pre-warming -*- lexical-binding: t -*-
;;; Commentary:
;;; Reads project root markers (Cargo.toml, package.json, go.mod, etc.)
;;; and config.yml languages section to pre-warm only the lang tiers the
;;; user has enabled.  Called by projectile-after-switch-project-hook and
;;; find-file-hook (fallback: extension-based lazy load).
;;;
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 3.0.1 to 3.0.4.
;;;   - FIX-PROJECT-ROOT-CRASH: emacs-ide-detect--project-root called
;;;     projectile-project-root without ignore-errors. projectile-project-root
;;;     signals user-error when not in a project — this crashed find-file-hook
;;;     on every non-project file open (e.g. /tmp files, scratch buffers).
;;;     Wrapped with ignore-errors.
;;;   - FIX-MARKER-MODULE-MISMATCH: Clojure markers (project.clj, deps.edn,
;;;     shadow-cljs.edn) had module "lang-clojure" in the marker table, but
;;;     emacs-ide-detect--lang-module-overrides maps "clojure" → "lang-functional".
;;;     emacs-ide-detect--module-for-lang returned the stale "lang-clojure"
;;;     (nonexistent file) before the override was consulted. Corrected marker
;;;     table entries to "lang-functional" to match the actual module file.
;;;   - FIX-SCALA-MODULE-MISMATCH: build.sbt/build.sc had module
;;;     "lang-functional" in the marker table but the override table maps
;;;     "scala" → "lang-jvm". Corrected marker entries to "lang-jvm" to be
;;;     consistent with the override table and the actual lang-jvm.el file.
;;;   - FIX-MODULES-DIR-UNBOUND: emacs-ide-modules-dir was referenced in
;;;     emacs-ide-detect--prewarm-lang but never defined in this file. If
;;;     init.el had not set it before this module loaded, the variable would
;;;     be void. Added a defvar with a sensible default fallback.
;;;   - FIX-STATUS-VERSION: emacs-ide-detect-show-status header showed
;;;     "v3.0.0" — updated to match the file version.
;;;   - FIX-PROJECT-SWITCH-HOOK: project-switch-project-hook does not exist
;;;     in Emacs built-in project.el — adding to a nonexistent hook is
;;;     silently a no-op. Removed the spurious add-hook call. Projectile's
;;;     projectile-after-switch-project-hook is the only reliable hook here.
;;; Fixes vs 3.0.0 (retained):
;;;   - FIX-11: emacs-ide-detect--lang-module-overrides table for langs whose
;;;     module does not follow "lang-<key>" naming convention.
;;; Code:

(require 'cl-lib)
(require 'core-dev)

;; ============================================================================
;; MODULES DIRECTORY
;; FIX-MODULES-DIR-UNBOUND: defvar with fallback so this file is safe to load
;; even if init.el has not yet set emacs-ide-modules-dir.
;; ============================================================================
(defvar emacs-ide-modules-dir
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing feature modules (modules/*.el and modules/langs/*.el).
Set by init.el at startup; this defvar provides a safe default fallback.")

;; ============================================================================
;; LANG-KEY → MODULE-NAME OVERRIDE TABLE (FIX-11)
;; Lang keys whose module does NOT follow the "lang-<key>" naming convention.
;; emacs-ide-detect--prewarm-lang consults this BEFORE the marker table or
;; the concat fallback — override table is authoritative.
;; ============================================================================
(defvar emacs-ide-detect--lang-module-overrides
  '(("javascript" . "lang-web")
    ("typescript" . "lang-web")
    ("haskell"    . "lang-functional")
    ("clojure"    . "lang-functional")
    ("elixir"     . "lang-functional")
    ("erlang"     . "lang-functional")
    ("ocaml"      . "lang-functional")
    ("java"       . "lang-jvm")
    ("kotlin"     . "lang-jvm")
    ("scala"      . "lang-jvm")
    ("groovy"     . "lang-jvm")
    ("zig"        . "lang-systems")
    ("nix"        . "lang-systems")
    ("d"          . "lang-systems")
    ("v"          . "lang-systems")
    ("r"          . "lang-data")
    ("julia"      . "lang-data")
    ("yaml"       . "lang-prose")
    ("dockerfile" . "lang-prose")
    ("terraform"  . "lang-prose")
    ("ansible"    . "lang-prose"))
  "Alist of (LANG-KEY . MODULE-NAME) for langs whose module name does not
follow the standard \"lang-<key>\" pattern.
This table is authoritative — it is consulted before marker-table module
columns and before the (concat \"lang-\" lang-key) fallback.")

;; ============================================================================
;; PROJECT MARKER DEFINITIONS
;; Each entry: (MARKER-FILE . (LANG-KEY LANG-MODULE WARMUP-FN-OR-NIL))
;; IMPORTANT: LANG-MODULE column must match the actual .el filename under
;; modules/langs/. When a lang-key has an override entry above, the
;; LANG-MODULE column here is ignored — keep them consistent anyway to
;; avoid confusion during future maintenance.
;; FIX-MARKER-MODULE-MISMATCH: clojure markers corrected to "lang-functional".
;; FIX-SCALA-MODULE-MISMATCH: scala markers corrected to "lang-jvm".
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
    ;; ── Tier 3 ──────────────────────────────────────────────────────────
    ;; FIX-MARKER-MODULE-MISMATCH: was "lang-clojure" — corrected to "lang-functional"
    ("project.clj"         . ("clojure"    "lang-functional" nil))
    ("deps.edn"            . ("clojure"    "lang-functional" nil))
    ("shadow-cljs.edn"     . ("clojure"    "lang-functional" nil))
    ("stack.yaml"          . ("haskell"    "lang-functional" nil))
    ("package.yaml"        . ("haskell"    "lang-functional" nil))
    ("cabal.project"       . ("haskell"    "lang-functional" nil))
    ;; FIX-SCALA-MODULE-MISMATCH: was "lang-functional" — corrected to "lang-jvm"
    ("build.sbt"           . ("scala"      "lang-jvm"        nil))
    ("build.sc"            . ("scala"      "lang-jvm"        nil))
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
;; DETECTION CORE
;; ============================================================================
(defvar emacs-ide-detect--pre-warmed nil
  "Hash table of already pre-warmed lang keys to avoid redundant loads.")

(defun emacs-ide-detect--init-cache ()
  "Initialize pre-warm cache if needed."
  (unless (hash-table-p emacs-ide-detect--pre-warmed)
    (setq emacs-ide-detect--pre-warmed (make-hash-table :test 'equal))))

(defun emacs-ide-detect--project-root ()
  "Return current project root or nil.
FIX-PROJECT-ROOT-CRASH: projectile-project-root signals user-error when not
in a project — wrapped with ignore-errors so find-file-hook never crashes on
non-project files (e.g. /tmp scratch buffers, remote files)."
  (cond
   ((and (fboundp 'projectile-project-root)
         (ignore-errors (projectile-project-root)))
    (ignore-errors (projectile-project-root)))
   ((and (fboundp 'project-root)
         (project-current))
    (project-root (project-current)))
   (t nil)))

(defun emacs-ide-detect--scan-root (root)
  "Scan ROOT directory for marker files. Return list of matching lang-keys."
  (when (and root (file-directory-p root))
    (let (found)
      (dolist (entry emacs-ide-detect--markers)
        (let* ((marker      (car entry))
               (info        (cdr entry))
               (marker-path (expand-file-name marker root)))
          (when (file-exists-p marker-path)
            (push (car info) found))))
      (delete-dups found))))

(defun emacs-ide-detect--module-for-lang (lang-key)
  "Return the module name for LANG-KEY from the markers table.
Used as a secondary fallback after the override table."
  (let ((entry (cl-find lang-key emacs-ide-detect--markers
                        :key (lambda (e) (car (cdr e)))
                        :test #'string=)))
    (when entry (cadr (cdr entry)))))

;; ============================================================================
;; PRE-WARMING
;; ============================================================================
(defun emacs-ide-detect--prewarm-lang (lang-key)
  "Pre-warm lang module for LANG-KEY if enabled in config and not already done.
Override table is consulted first (authoritative), then marker table module
column, then (concat \"lang-\" lang-key) as last resort."
  (emacs-ide-detect--init-cache)
  (when (and (emacs-ide-dev-lang-enabled-p lang-key)
             (not (gethash lang-key emacs-ide-detect--pre-warmed)))
    (puthash lang-key t emacs-ide-detect--pre-warmed)
    (let* ((module-name (or (cdr (assoc lang-key
                                        emacs-ide-detect--lang-module-overrides))
                            (emacs-ide-detect--module-for-lang lang-key)
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
;; PROJECT SWITCH HOOK
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
;; FILE OPEN FALLBACK HOOK
;; Only triggers when no project markers found. Prevents double-warming.
;; ============================================================================
(defun emacs-ide-detect-on-find-file ()
  "Extension-based fallback. Fires for every file open.
Only pre-warms when no project markers matched — prevents redundant loads."
  (when buffer-file-name
    (let* ((root    (emacs-ide-detect--project-root))
           (markers (emacs-ide-detect--scan-root root))
           (ext     (file-name-extension buffer-file-name))
           (lang    (cdr (assoc (downcase (or ext ""))
                                emacs-ide-detect--ext-map))))
      (when (and lang (null markers))
        (emacs-ide-detect--prewarm-lang lang)))))

;; ============================================================================
;; HOOKS
;; FIX-PROJECT-SWITCH-HOOK: project-switch-project-hook does not exist in
;; Emacs built-in project.el — removed the spurious add-hook call.
;; projectile-after-switch-project-hook is the reliable hook for this.
;; ============================================================================
(add-hook 'projectile-after-switch-project-hook
          #'emacs-ide-detect-on-project-switch)

(add-hook 'find-file-hook #'emacs-ide-detect-on-find-file)

;; ============================================================================
;; INTERACTIVE COMMANDS
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
  "Show all pre-warmed languages and registered lang modules."
  (interactive)
  (with-output-to-temp-buffer "*Project Detect Status*"
    ;; FIX-STATUS-VERSION: updated from hardcoded v3.0.0 to match file version
    (princ "=== PROJECT DETECT STATUS (v3.0.4) ===\n\n")
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
    (unless (hash-table-p emacs-ide-detect--pre-warmed)
      (princ "  (cache not initialised yet)\n"))
    (princ "\nRegistered lang modules:\n")
    (if (fboundp 'emacs-ide-dev-registered-langs)
        (dolist (lang (emacs-ide-dev-registered-langs))
          (let ((enabled (emacs-ide-dev-lang-enabled-p lang)))
            (princ (format "  %s %s\n" (if enabled "✓" "✗") lang))))
      (princ "  (core-dev not loaded)\n"))))

(provide 'tools-project-detect)
;;; tools-project-detect.el ends here
