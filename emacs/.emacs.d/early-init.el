;;; early-init.el --- Enterprise Emacs IDE Early Initialization -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; FIXES in this version:
;;;   #69  file-name-handler-alist was disabled in phase 14 of 15, after
;;;        TLS setup, native-comp, I/O, redisplay, treesit, custom-file, and
;;;        JIT-lock had all already run.  The performance benefit of disabling
;;;        it (faster require / file operations during startup) was obtained
;;;        for only ONE phase (site-lisp-disable) instead of the entire init
;;;        sequence.  Moved to phase 2, immediately after GC setup, so all
;;;        subsequent requires benefit.  The saved alist is still restored in
;;;        emacs-startup-hook (same as before).
;;;
;;;        Also fixed: the saved copy was captured at defvar time (line 44)
;;;        correctly — but then disabled AFTER TLS had already called
;;;        (require 'gnutls) which could itself trigger the handler.  Moving
;;;        disable to phase 2 means TLS runs without the handler (gnutls is a
;;;        built-in C module, so it does not need the handler).
;;;
;;;   #11  set-face-attribute for theme-flash prevention was called
;;;        unconditionally.  In a headless/TTY session or when Emacs runs as
;;;        a daemon (no initial frame), there is no `default' face instance
;;;        and the call generates warnings.  Wrapped in (when (display-graphic-p)).
;;;        For daemon mode the face will be set when a graphical frame connects,
;;;        via a server-after-make-frame-hook entry.
;;;
;;; Code:

(defvar emacs-ide--early-init-start-time (current-time))
(defvar emacs-ide--early-init-benchmark-data nil)

(defun emacs-ide--benchmark-phase (phase-name body-fn)
  "Execute BODY-FN, record elapsed time under PHASE-NAME, warn if > 0.25s."
  (let ((start (current-time)))
    (funcall body-fn)
    (let ((elapsed (float-time (time-subtract (current-time) start))))
      (push (cons phase-name elapsed) emacs-ide--early-init-benchmark-data)
      (when (> elapsed 0.25)
        (warn "Early-init phase '%s' took %.3fs" phase-name elapsed)))))

(defun emacs-ide--get-processor-count ()
  "Detect the number of logical CPU cores."
  (cond
   ((fboundp 'num-processors) (num-processors))
   (t
    (let ((n (condition-case nil
                 (string-to-number
                  (string-trim
                   (shell-command-to-string
                    "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4")))
               (error 4))))
      (if (> n 0) n 4)))))

(defvar emacs-ide-processor-count (emacs-ide--get-processor-count))

;;;; ── Phase 1: GC — defer during init ────────────────────────────────────────

(emacs-ide--benchmark-phase "gc-setup"
  (lambda ()
    (setq gc-cons-threshold  most-positive-fixnum
          gc-cons-percentage 1.0)))

;;;; ── Phase 2: Disable file-name-handler (FIX #69 — moved early) ────────────
;; Captured BEFORE disabling so emacs-startup-hook can restore it.
;; Disabling here means all subsequent (require ...) calls during startup
;; skip the handler chain, providing a measurable speedup.
;; gnutls is a built-in C module and does not need the handler.

(defvar emacs-ide--file-name-handler-alist file-name-handler-alist
  "Original file-name-handler-alist, saved before early-init disables it.
Restored in `emacs-startup-hook' at depth 100.")

(emacs-ide--benchmark-phase "file-handler-disable"
  (lambda ()
    (setq file-name-handler-alist nil)))

;;;; ── Phase 3: LSP plists ─────────────────────────────────────────────────────

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

(defvar emacs-ide--gc-timer nil
  "Idle GC timer started in `emacs-startup-hook'.")

;;;; ── Phase 4: Disable package.el ────────────────────────────────────────────

(emacs-ide--benchmark-phase "package-disable"
  (lambda ()
    (setq package-enable-at-startup nil
          package-quickstart        nil)))

;;;; ── Phase 5: Frame defaults ─────────────────────────────────────────────────

(emacs-ide--benchmark-phase "frame-optimization"
  (lambda ()
    (setq frame-inhibit-implied-resize t
          frame-resize-pixelwise       t
          window-resize-pixelwise      t
          default-frame-alist
          '((min-height             . 1)
            (min-width              . 1)
            (vertical-scroll-bars   . nil)
            (horizontal-scroll-bars . nil)
            (menu-bar-lines         . 0)
            (tool-bar-lines         . 0)
            (left-fringe            . 8)
            (right-fringe           . 8)
            (alpha-background       . 97)
            (fullscreen             . maximized)
            (internal-border-width  . 2)))))

;;;; ── Phase 6: Strip UI early ─────────────────────────────────────────────────

(emacs-ide--benchmark-phase "ui-disable"
  (lambda ()
    (when (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
    (when (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))

;;;; ── Phase 7: Wayland / pgtk ─────────────────────────────────────────────────

(emacs-ide--benchmark-phase "wayland-optimization"
  (lambda ()
    (when (eq window-system 'pgtk)
      (let ((desktop (or (getenv "XDG_CURRENT_DESKTOP") "")))
        (when (string-match-p "\\(GNOME\\|Unity\\|Budgie\\)" desktop)
          (push '(inhibit-double-buffering . t) default-frame-alist)))
      (setq pgtk-wait-for-event-timeout 0.001))
    (when (getenv "WAYLAND_DISPLAY")
      (setq x-wait-for-event-timeout 0.001))))

;;;; ── Phase 8: Warning suppression ───────────────────────────────────────────

(defvar emacs-ide--saved-byte-compile-warnings nil)

(emacs-ide--benchmark-phase "warning-suppression"
  (lambda ()
    (setq emacs-ide--saved-byte-compile-warnings
          (if (boundp 'byte-compile-warnings) byte-compile-warnings t))
    (setq warning-minimum-level  :emergency
          byte-compile-warnings  nil)
    (when (boundp 'native-comp-async-report-warnings-errors)
      (setq native-comp-async-report-warnings-errors 'silent))
    (unless (boundp 'warning-suppress-log-types)
      (defvar warning-suppress-log-types nil))
    (add-to-list 'warning-suppress-log-types '(defvaralias))))

;;;; ── Phase 9: Native compilation ────────────────────────────────────────────

(emacs-ide--benchmark-phase "native-comp-setup"
  (lambda ()
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (setq native-comp-async-report-warnings-errors 'silent
            native-comp-speed                         2
            native-comp-async-jobs-number
            (max 1 (/ (or emacs-ide-processor-count 4) 2)))
      (when (boundp 'native-comp-jit-compilation)
        (setq native-comp-jit-compilation t))
      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache
         (convert-standard-filename
          (expand-file-name "var/eln-cache/" user-emacs-directory)))))))

;;;; ── Phase 10: I/O ───────────────────────────────────────────────────────────

(emacs-ide--benchmark-phase "io-optimization"
  (lambda ()
    (setq read-process-output-max    (* 4 1024 1024)
          process-adaptive-read-buffering nil)))

;;;; ── Phase 11: Redisplay ─────────────────────────────────────────────────────

(emacs-ide--benchmark-phase "redisplay-optimization"
  (lambda ()
    (setq inhibit-compacting-font-caches         t
          fast-but-imprecise-scrolling           t
          redisplay-skip-fontification-on-input  t
          highlight-nonselected-windows          nil
          scroll-error-top-bottom                t)))

;;;; ── Phase 12: tree-sitter ───────────────────────────────────────────────────

(emacs-ide--benchmark-phase "treesit-setup"
  (lambda ()
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (setq treesit-font-lock-level 4
            treesit-extra-load-path
            (list (expand-file-name "var/tree-sitter"
                                    user-emacs-directory))))))

;;;; ── Phase 13: custom-file ───────────────────────────────────────────────────

(emacs-ide--benchmark-phase "custom-file-setup"
  (lambda ()
    (setq custom-file
          (expand-file-name "var/custom.el" user-emacs-directory))))

;;;; ── Phase 14: JIT lock ──────────────────────────────────────────────────────

(emacs-ide--benchmark-phase "jit-lock-optimization"
  (lambda ()
    (setq jit-lock-defer-time    0.025
          jit-lock-stealth-time  1
          jit-lock-stealth-nice  0.1
          jit-lock-chunk-size    1000)))

;;;; ── Phase 15: Backups off during early init ─────────────────────────────────
;; init.el re-enables versioned backups to var/backups/ after startup.

(emacs-ide--benchmark-phase "backup-disable"
  (lambda ()
    (setq auto-save-default nil
          make-backup-files nil
          create-lockfiles  nil
          auto-save-list-file-prefix
          (expand-file-name "var/auto-save-list/.saves-"
                            user-emacs-directory))))

;;;; ── Phase 16: TLS / security ────────────────────────────────────────────────
;; gnutls is a built-in C module; (require 'gnutls) does not need
;; file-name-handler-alist (which was disabled in phase 2 above).

(emacs-ide--benchmark-phase "tls-security"
  (lambda ()
    (require 'gnutls)
    (setq gnutls-verify-error   t
          gnutls-min-prime-bits 3072
          tls-checktrust        t
          tls-program '("gnutls-cli --x509cafile %t -p %p %h"))))

;;;; ── Phase 17: Inhibit default site-lisp init ───────────────────────────────

(emacs-ide--benchmark-phase "site-lisp-disable"
  (lambda ()
    (setq inhibit-default-init t)))

;;;; ── Phase 18: Initial theme flash prevention (FIX #11) ─────────────────────
;; FIX #11: wrapped in (when (display-graphic-p)) so TTY sessions and
;; daemon mode (no initial graphical frame) do not generate face warnings.
;; In daemon mode, a server-after-make-frame-hook entry applies the dark
;; background when the first graphical client connects.

(emacs-ide--benchmark-phase "initial-theme"
  (lambda ()
    (if (display-graphic-p)
        ;; GUI startup — set dark background immediately to prevent white flash
        (progn
          (set-face-attribute 'default nil
                              :background "#1a1a2e"
                              :foreground "#e0e0f0")
          (set-face-attribute 'mode-line nil
                              :background "#16213e"
                              :foreground "#e0e0f0"
                              :box nil))
      ;; Daemon or TTY — defer face setup until a graphical frame appears
      (add-hook 'server-after-make-frame-hook
                (lambda ()
                  (when (display-graphic-p)
                    (set-face-attribute 'default nil
                                        :background "#1a1a2e"
                                        :foreground "#e0e0f0")
                    (set-face-attribute 'mode-line nil
                                        :background "#16213e"
                                        :foreground "#e0e0f0"
                                        :box nil)))
                nil t))))  ;; LOCAL hook so it fires only once per frame

;;;; ── Post-startup restoration ────────────────────────────────────────────────

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore file-name-handler-alist (disabled in phase 2)
            (setq file-name-handler-alist emacs-ide--file-name-handler-alist)
            ;; Restore GC to working threshold (set by config-apply later,
            ;; but 16 MB is the safe default if config hasn't run yet)
            (unless (> gc-cons-threshold (* 16 1024 1024))
              (setq gc-cons-threshold  (* 16 1024 1024)
                    gc-cons-percentage 0.1))
            ;; Start periodic idle GC
            (unless emacs-ide--gc-timer
              (setq emacs-ide--gc-timer
                    (run-with-idle-timer 20 t #'garbage-collect))))
          100)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore warning levels suppressed during startup
            (setq warning-minimum-level :warning)
            (when (boundp 'emacs-ide--saved-byte-compile-warnings)
              (setq byte-compile-warnings
                    emacs-ide--saved-byte-compile-warnings)))
          90)

(add-hook 'kill-emacs-hook
          (lambda ()
            (when emacs-ide--gc-timer
              (cancel-timer emacs-ide--gc-timer))))

;;;; ── Safe mode flag ──────────────────────────────────────────────────────────

(defvar emacs-ide-safe-mode nil
  "Non-nil when Emacs IDE started in safe / minimal mode.")

(when (or (getenv "EMACS_SAFE_MODE")
          (member "--emacs-ide-safe" command-line-args-left))
  (setq emacs-ide-safe-mode t)
  (setq command-line-args-left
        (delete "--emacs-ide-safe" command-line-args-left))
  (message "🛡  EMACS IDE SAFE MODE — minimal configuration"))

;;;; ── Display-server detection ────────────────────────────────────────────────

(defvar emacs-ide-wayland-p (and (getenv "WAYLAND_DISPLAY") t))

(defvar emacs-ide-display-server
  (cond
   (emacs-ide-wayland-p       "Wayland")
   ((getenv "DISPLAY")        "X11")
   ((daemonp)                 "daemon")
   (t                         "TTY")))

;;;; ── Report ──────────────────────────────────────────────────────────────────

(defun emacs-ide-early-init-report ()
  "Display a benchmark buffer showing per-phase early-init timings."
  (interactive)
  (let ((total (float-time
                (time-subtract (current-time)
                               emacs-ide--early-init-start-time))))
    (with-output-to-temp-buffer "*Early-Init Benchmark*"
      (princ (format "=== EARLY-INIT PERFORMANCE  (total %.3fs) ===\n\n"
                     total))
      (dolist (phase (reverse emacs-ide--early-init-benchmark-data))
        (princ (format "  %-30s %.3fs  (%4.1f%%)\n"
                       (car phase) (cdr phase)
                       (if (zerop total) 0.0
                         (* 100 (/ (cdr phase) total)))))))))

(provide 'early-init)
;;; early-init.el ends here
