;;; init.el --- Enterprise Emacs IDE Bootstrap -*- lexical-binding: t -*-
;;; Version: 3.5.0
;;;
;;; Code:

;;;; ── Version ─────────────────────────────────────────────────────────────────

(defconst emacs-ide-version "3.5.0")

;;;; ── Directory layout ────────────────────────────────────────────────────────

(defvar emacs-ide-core-dir
  (expand-file-name "core" user-emacs-directory))
(defvar emacs-ide-modules-dir
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing feature modules (modules/ and modules/langs/).")
(defvar emacs-ide-langs-dir
  (expand-file-name "modules/langs" user-emacs-directory))
(defvar emacs-ide-lib-dir
  (expand-file-name "lib" user-emacs-directory))
(defvar emacs-ide-var-dir
  (expand-file-name "var" user-emacs-directory))

(dolist (dir (list emacs-ide-core-dir emacs-ide-modules-dir
                   emacs-ide-langs-dir emacs-ide-lib-dir
                   emacs-ide-var-dir
                   (expand-file-name "backups" emacs-ide-var-dir)))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(dolist (dir (list emacs-ide-core-dir emacs-ide-modules-dir
                   emacs-ide-langs-dir emacs-ide-lib-dir))
  (add-to-list 'load-path dir))

;;;; ── Startup phase tracking ──────────────────────────────────────────────────

(defvar emacs-ide--startup-start-time (current-time))
(defvar emacs-ide--startup-phases nil)

(defun emacs-ide--startup-phase (phase-name body-fn)
  "Execute BODY-FN, recording elapsed time under PHASE-NAME."
  (let ((start (current-time)))
    (funcall body-fn)
    (push (cons phase-name
                (float-time (time-subtract (current-time) start)))
          emacs-ide--startup-phases)))

;;;; ── straight.el bootstrap ──────────────────────────────────────────────────

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-vc-git-default-clone-depth 1)

(straight-use-package 'use-package)
(setq use-package-always-defer nil
      use-package-expand-minimally t
      use-package-verbose nil)

;;;; ── Core module: config — must load first ───────────────────────────────────

(emacs-ide--startup-phase "config"
  (lambda ()
    (require 'emacs-ide-config)
    (emacs-ide-config-load)))

;;;; ── Core + feature modules, in dependency order ────────────────────────────

(defvar emacs-ide-feature-modules
  '("emacs-ide-health" "emacs-ide-diagnose" "emacs-ide-package"
    "emacs-ide-profiler" "emacs-ide-security" "emacs-ide-telemetry"
    "emacs-ide-recovery"
    "ui-core" "ui-theme" "ui-modeline" "ui-dashboard" "ui-workspace"
    "completion-core" "completion-snippets" "editing-core" "core-dev"
    "tools-lsp" "tools-project" "tools-git" "tools-terminal"
    "tools-format" "apheleia-langs-patch"
    "tools-org" "tools-spelling" "tools-notes" "tools-rest"
    "tools-test-runner-registry" "tools-test" "debug-core"
    "tools-repl" "tools-project-detect" "tools-hydra"
    "tools-database" "tools-devops" "emacs-ide-analytics"
    "keybindings")
  "Feature modules loaded eagerly in order.
Language modules (modules/langs/*.el) load lazily via tools-project-detect.")

(emacs-ide--startup-phase "feature-modules"
  (lambda ()
    (dolist (mod emacs-ide-feature-modules)
      (condition-case err
          (require (intern mod))
        (error
         (message "init: %s failed to load: %s" mod
                  (error-message-string err)))))))

;;;; ── Diagnostics — loaded last, after everything they verify ─────────────────

(emacs-ide--startup-phase "diagnostics"
  (lambda ()
    (require 'emacs-ide-spot-check)
    (require 'emacs-ide-test)))

;;;; ── custom-file ─────────────────────────────────────────────────────────────

(when (and (boundp 'custom-file) (file-exists-p custom-file))
  (load custom-file 'noerror 'nomessage))

;;;; ── Backups (re-enabled after early-init disabled them) ────────────────────

(setq make-backup-files       t
      backup-by-copying       t
      delete-old-versions     t
      kept-new-versions       6
      kept-old-versions       2
      version-control         t
      vc-make-backup-files    t
      backup-directory-alist
      `(("." . ,(expand-file-name "backups" emacs-ide-var-dir))))

;;;; ── Core init helpers ───────────────────────────────────────────────────────

(defun emacs-ide-show-version ()
  "Show IDE version, Emacs version, and package count."
  (interactive)
  (message "Emacs IDE v%s | Emacs %s | %d packages"
           emacs-ide-version emacs-version
           (condition-case nil
               (if (and (boundp 'straight--build-cache)
                        (hash-table-p straight--build-cache))
                   (hash-table-count straight--build-cache) 0)
             (error 0))))

(defun emacs-ide-startup-report ()
  "Display a benchmark buffer showing per-phase startup timings."
  (interactive)
  (let ((total (float-time (time-subtract (current-time)
                                           emacs-ide--startup-start-time))))
    (with-output-to-temp-buffer "*Startup Benchmark*"
      (princ (format "=== EMACS IDE v%s STARTUP  (total %.3fs) ===\n\n"
                     emacs-ide-version total))
      (dolist (phase (reverse emacs-ide--startup-phases))
        (princ (format "  %-20s %.3fs  (%4.1f%%)\n"
                       (car phase) (cdr phase)
                       (if (zerop total) 0.0
                         (* 100 (/ (cdr phase) total))))))
      (princ "\nSee also: M-x emacs-ide-early-init-report\n"))))

(defun emacs-ide-reload ()
  "Reload init.el from disk."
  (interactive)
  (when (y-or-n-p "Reload init.el? Some state may not reset cleanly. ")
    (load-file user-init-file)
    (message "✓ init.el reloaded")))

(defun emacs-ide-purge-bytecode-cache ()
  "Delete eln-cache and straight build artifacts for a clean rebuild."
  (interactive)
  (when (y-or-n-p "Purge eln-cache and straight build artifacts? ")
    (let ((eln-dir  (expand-file-name "var/eln-cache" user-emacs-directory))
          (bld-dir  (expand-file-name "straight/build" user-emacs-directory)))
      (when (file-directory-p eln-dir) (delete-directory eln-dir t))
      (when (file-directory-p bld-dir) (delete-directory bld-dir t))
      (message "✓ Bytecode caches purged — restart Emacs to rebuild"))))

(defun emacs-ide-update ()
  "Update all straight.el-managed packages."
  (interactive)
  (if (fboundp 'straight-pull-all)
      (progn (message "⏳ Updating…") (straight-pull-all)
             (message "✓ Packages updated — restart to use new versions"))
    (message "init: straight.el not loaded")))

(defun emacs-ide-freeze-versions ()
  "Freeze current package versions to straight.el's lockfile."
  (interactive)
  (if (fboundp 'straight-freeze-versions)
      (progn (straight-freeze-versions)
             (message "✓ Versions frozen to straight/versions/"))
    (message "init: straight.el not loaded")))

;;;; ── kill-emacs-hook: cancel the session timer from emacs-ide-recovery ───────

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (boundp 'emacs-ide-recovery--session-timer)
                       (timerp emacs-ide-recovery--session-timer))
              (cancel-timer emacs-ide-recovery--session-timer))))

;;;; ── Post-startup ────────────────────────────────────────────────────────────

(add-hook 'emacs-startup-hook
          (lambda ()
            (push (cons "startup-complete"
                        (float-time (time-subtract (current-time)
                                                    emacs-ide--startup-start-time)))
                  emacs-ide--startup-phases)
            (when (fboundp 'emacs-ide-telemetry-log-startup)
              (emacs-ide-telemetry-log-startup
               (cdr (assoc "startup-complete" emacs-ide--startup-phases))
               gcs-done
               (condition-case nil
                   (if (and (boundp 'straight--build-cache)
                            (hash-table-p straight--build-cache))
                       (hash-table-count straight--build-cache) 0)
                 (error 0))))
            (message "Emacs IDE v%s ready in %.2fs"
                     emacs-ide-version
                     (cdr (assoc "startup-complete" emacs-ide--startup-phases))))
          80)

;;; init.el ends here
