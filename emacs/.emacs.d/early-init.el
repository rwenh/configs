;;; early-init.el --- Enterprise Emacs IDE Early Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade early initialization with performance monitoring.
;;; Version: 2.2.2
;;; Fixes vs 2.2.1:
;;;   - BUG-01: `emacs-ide--init-start-time` was defined TWICE — once here and
;;;     once in init.el. The init.el definition clobbers the early-init one,
;;;     so all startup phase timings in init.el were measured from the moment
;;;     init.el ran, not from true Emacs boot.  Fix: rename the early-init var
;;;     to `emacs-ide--early-init-start-time` so both coexist correctly.
;;;   - BUG-02: `warning-suppression` benchmark phase set `byte-compile-warnings`
;;;     to nil globally and permanently.  That silences *all* byte-compiler
;;;     warnings for user code, not just during bootstrap noise.  Fix: save and
;;;     restore the original value in the emacs-startup-hook alongside the
;;;     warning-minimum-level restore.
;;;   - BUG-03: `inhibit-double-buffering` pushed to `default-frame-alist` for
;;;     PGTK unconditionally suppresses compositor-side double-buffering on all
;;;     Wayland compositors including those where it causes visual corruption
;;;     (wlroots, KDE Plasma). Fix: only push it when NOT on wlroots/KDE
;;;     (detected via WAYLAND_DISPLAY and XDG_CURRENT_DESKTOP).
;;;   - BUG-04: `auto-save-list-file-prefix` was set to nil here, but init.el
;;;     later re-enables backups and auto-saves selectively. Setting the prefix
;;;     to nil permanently breaks `recover-this-file` / `recover-session`
;;;     regardless of later init settings. Fix: redirect to var/ instead of nil.
;;;   - BUG-05: `site-run-file nil` + `inhibit-default-init t` suppresses site
;;;     configuration entirely, including /etc/emacs/site-start.d/* entries that
;;;     system admins deploy for certificate stores, proxy settings, and locale.
;;;     On enterprise Linux this silently breaks HTTPS and locale. Fix: add a
;;;     guard — only suppress when not on a system with /etc/emacs present,
;;;     or (safer) only set inhibit-default-init, not site-run-file.
;;;   - BUG-06: `jit-lock-defer-time 0` disables JIT-lock deferral entirely —
;;;     fontification fires synchronously on every keystroke, causing
;;;     significant lag in large files even though the intent was "no delay".
;;;     Setting to 0.0 has the same effect as nil for defer. Fix: use 0.025
;;;     (25ms) which gives genuine async deferral while remaining imperceptible.
;;; Code:

;; ============================================================================
;; PERFORMANCE TRACKING - START
;; BUG-01 FIX: Renamed to emacs-ide--early-init-start-time so init.el's
;; emacs-ide--init-start-time (defined later) does not clobber this one.
;; ============================================================================
(defvar emacs-ide--early-init-start-time (current-time)
  "Time when early-init started.")

(defvar emacs-ide--early-init-benchmark-data nil
  "Benchmark data for early-init phases.")

(defun emacs-ide--benchmark-phase (phase-name body-fn)
  "Benchmark PHASE-NAME executing BODY-FN."
  (let ((start (current-time)))
    (funcall body-fn)
    (let ((elapsed (float-time (time-subtract (current-time) start))))
      (push (cons phase-name elapsed) emacs-ide--early-init-benchmark-data)
      (when (> elapsed 0.25)
        (warn "Early-init phase '%s' took %.3fs (slow)" phase-name elapsed)))))

;; ============================================================================
;; UTILITY FUNCTIONS - MUST BE DEFINED EARLY
;; ============================================================================
(defun emacs-ide--get-processor-count ()
  "Get number of CPU cores safely."
  (let* ((env-count (getenv "NUMBER_OF_PROCESSORS"))
         (count (if env-count
                    (condition-case nil
                        (string-to-number env-count)
                      (error 0))
                  0)))
    (if (> count 0) count
      (condition-case nil
          (string-to-number
           (string-trim
            (shell-command-to-string
             "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4")))
        (error 4)))))

;; ============================================================================
;; GARBAGE COLLECTION - AGGRESSIVE OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "gc-setup"
  (lambda ()
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 1.0)))

(defvar emacs-ide--gc-timer nil
  "Timer for idle garbage collection.")

(defvar emacs-ide--file-name-handler-alist file-name-handler-alist
  "Backup of file-name-handler-alist.")

;; ============================================================================
;; PACKAGE.EL - DISABLE EARLY
;; ============================================================================
(emacs-ide--benchmark-phase "package-disable"
  (lambda ()
    (setq package-enable-at-startup nil
          package-quickstart nil)))

;; ============================================================================
;; FRAME OPTIMIZATION - PREVENT FLASHING
;; ============================================================================
(emacs-ide--benchmark-phase "frame-optimization"
  (lambda ()
    (setq frame-inhibit-implied-resize t
          frame-resize-pixelwise t
          window-resize-pixelwise t
          default-frame-alist
          '((min-height . 1)
            (min-width . 1)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0)
            (left-fringe . 8)
            (right-fringe . 8)
            (alpha-background . 100)
            (fullscreen . maximized)
            (internal-border-width . 0)
            (undecorated . nil)))))

;; ============================================================================
;; UI ELEMENTS - DISABLE BEFORE INIT
;; ============================================================================
(emacs-ide--benchmark-phase "ui-disable"
  (lambda ()
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

;; ============================================================================
;; WAYLAND/PGTK OPTIMIZATIONS
;; BUG-03 FIX: Only push inhibit-double-buffering on compositors where it's
;; safe (GNOME/Mutter). On wlroots and KDE it causes corruption.
;; ============================================================================
(emacs-ide--benchmark-phase "wayland-optimization"
  (lambda ()
    (when (eq window-system 'pgtk)
      ;; Only enable inhibit-double-buffering on GNOME/Mutter — safe there.
      ;; Skip on wlroots-based compositors and KDE Plasma where it corrupts.
      (let ((desktop (or (getenv "XDG_CURRENT_DESKTOP") "")))
        (when (string-match-p "\\(GNOME\\|Unity\\|Budgie\\)" desktop)
          (push '(inhibit-double-buffering . t) default-frame-alist)))
      (setq pgtk-wait-for-event-timeout 0.001)
      (when (boundp 'pgtk-use-im-context-on-new-connection)
        (setq pgtk-use-im-context-on-new-connection t)))

    (when (getenv "WAYLAND_DISPLAY")
      (setq x-wait-for-event-timeout 0.001))))

;; ============================================================================
;; NATIVE COMPILATION - SILENT & OPTIMIZED
;; ============================================================================
(emacs-ide--benchmark-phase "native-comp-setup"
  (lambda ()
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (setq native-comp-async-report-warnings-errors nil
            native-comp-speed 2
            native-comp-async-jobs-number (max 1 (/ (or (emacs-ide--get-processor-count) 4) 2))
            package-native-compile t)

      (when (boundp 'native-comp-jit-compilation)
        (setq native-comp-jit-compilation t))

      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache
         (convert-standard-filename
          (expand-file-name "var/eln-cache/" user-emacs-directory)))))))

;; ============================================================================
;; FILE HANDLER OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "file-handler-disable"
  (lambda ()
    (setq file-name-handler-alist nil)))

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (emacs-ide--benchmark-phase "post-startup-restore"
              (lambda ()
                (setq file-name-handler-alist emacs-ide--file-name-handler-alist)

                ;; Restore GC to reasonable values
                (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                      gc-cons-percentage 0.1)

                ;; Setup idle GC with proper cleanup
                (unless emacs-ide--gc-timer
                  (setq emacs-ide--gc-timer
                        (run-with-idle-timer 15 t #'garbage-collect)))

                (garbage-collect))))
          100)

;; FIX 2.2.1: Restore warning level at priority 90 (before GC restore at 100).
;; BUG-02 FIX: The save of byte-compile-warnings happens inside the
;; warning-suppression benchmark phase below (right before the setq that
;; clobbers it). A top-level (defvar ... byte-compile-warnings) evaluated
;; the initial value immediately, before the variable was bound in a clean
;; Emacs session, producing: void-variable byte-compile-warnings.
;; The hook uses (boundp) as a safety guard.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq warning-minimum-level :warning)
            (when (boundp 'emacs-ide--saved-byte-compile-warnings)
              (setq byte-compile-warnings
                    emacs-ide--saved-byte-compile-warnings)))
          90)

;; Cleanup GC timer on exit
(add-hook 'kill-emacs-hook
          (lambda ()
            (when emacs-ide--gc-timer
              (cancel-timer emacs-ide--gc-timer))))

;; ============================================================================
;; PROCESS & I/O OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "io-optimization"
  (lambda ()
    (setq read-process-output-max (* 4 1024 1024)  ; 4MB
          process-adaptive-read-buffering nil)))

;; ============================================================================
;; REDISPLAY OPTIMIZATION
;; ============================================================================
(emacs-ide--benchmark-phase "redisplay-optimization"
  (lambda ()
    (setq inhibit-compacting-font-caches t
          fast-but-imprecise-scrolling t
          redisplay-skip-fontification-on-input t
          highlight-nonselected-windows nil)))

;; ============================================================================
;; SITE-LISP OPTIMIZATION
;; BUG-05 FIX: Do NOT suppress site-run-file on enterprise Linux — system admins
;; deploy cert stores, proxy, and locale config there. Only suppress
;; inhibit-default-init (user's own default.el), which is safe everywhere.
;; ============================================================================
(emacs-ide--benchmark-phase "site-lisp-disable"
  (lambda ()
    ;; Only suppress user's default.el, not system site configuration.
    (setq inhibit-default-init t)
    ;; site-run-file is intentionally NOT set to nil here.
    ;; If you are on a personal machine with no /etc/emacs, uncomment:
    ;; (setq site-run-file nil)
    ))

;; ============================================================================
;; INITIAL APPEARANCE - MODUS VIVENDI COLORS
;; ============================================================================
(emacs-ide--benchmark-phase "initial-theme"
  (lambda ()
    (set-face-attribute 'default nil
                        :background "#000000"
                        :foreground "#ffffff")

    (set-face-attribute 'mode-line nil
                        :background "#1e1e1e"
                        :foreground "#ffffff")))

;; ============================================================================
;; REDUCE STARTUP NOISE
;; BUG-02 FIX: byte-compile-warnings saved inside this lambda (see below)
;; immediately before it is clobbered, then restored in the startup-hook above.
;; ============================================================================
(emacs-ide--benchmark-phase "warning-suppression"
  (lambda ()
    ;; BUG-02 FIX: Save byte-compile-warnings HERE, inside the lambda, right
    ;; before we clobber it.  At this point in the load sequence the variable
    ;; is guaranteed to exist (bytecomp is part of Emacs core).  Using setq
    ;; (not defvar) means the save is always refreshed on reload and there is
    ;; no top-level evaluation of the variable at file-load time.
    (defvar emacs-ide--saved-byte-compile-warnings nil)
    (setq emacs-ide--saved-byte-compile-warnings
          (if (boundp 'byte-compile-warnings) byte-compile-warnings t))
    (setq warning-minimum-level :emergency
          byte-compile-warnings nil
          native-comp-async-report-warnings-errors nil)))

;; ============================================================================
;; TREESIT PREPARATION
;; ============================================================================
(emacs-ide--benchmark-phase "treesit-setup"
  (lambda ()
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (setq treesit-font-lock-level 4
            treesit-extra-load-path
            (list (expand-file-name "var/tree-sitter" user-emacs-directory))))))

;; ============================================================================
;; CUSTOM FILE EARLY SETUP
;; ============================================================================
(emacs-ide--benchmark-phase "custom-file-setup"
  (lambda ()
    (setq custom-file (expand-file-name "var/custom.el" user-emacs-directory))))

;; ============================================================================
;; JIT LOCK OPTIMIZATION
;; BUG-06 FIX: jit-lock-defer-time 0 == nil == synchronous fontification.
;; Use 0.025 (25ms) for genuine async deferral without perceptible lag.
;; ============================================================================
(emacs-ide--benchmark-phase "jit-lock-optimization"
  (lambda ()
    (setq jit-lock-defer-time 0.025   ; BUG-06 fix: was 0 (= no deferral)
          jit-lock-stealth-time 1
          jit-lock-stealth-nice 0.1
          jit-lock-chunk-size 1000)))

;; ============================================================================
;; AUTO-SAVE & BACKUP EARLY DISABLE
;; BUG-04 FIX: auto-save-list-file-prefix redirected to var/ instead of nil.
;; Setting it to nil permanently breaks recover-session/recover-this-file
;; even after init.el re-enables backups.
;; ============================================================================
(emacs-ide--benchmark-phase "backup-disable"
  (lambda ()
    (setq auto-save-default nil
          make-backup-files nil
          create-lockfiles nil
          ;; BUG-04 fix: was nil — redirect to var/ so recover-session works
          ;; after init.el re-enables auto-save for specific modes.
          auto-save-list-file-prefix
          (expand-file-name "var/auto-save-list/.saves-" user-emacs-directory))))

;; ============================================================================
;; EARLY INIT BENCHMARK REPORT
;; ============================================================================
(defun emacs-ide-early-init-report ()
  "Display early-init benchmark report."
  (interactive)
  (let ((total-time (float-time (time-subtract (current-time)
                                               emacs-ide--early-init-start-time))))
    (with-output-to-temp-buffer "*Early-Init Benchmark*"
      (princ (format "=== EARLY-INIT PERFORMANCE REPORT ===\n\n"))
      (princ (format "Total Time: %.3fs\n\n" total-time))
      (princ "Phase Breakdown:\n")
      (dolist (phase (reverse emacs-ide--early-init-benchmark-data))
        (princ (format "  %-25s %.3fs (%.1f%%)\n"
                       (car phase)
                       (cdr phase)
                       (* 100 (/ (cdr phase) total-time)))))
      (princ "\n")
      (when (> total-time 0.5)
        (princ "⚠️  Early-init took longer than expected (target: <0.5s)\n")))))

;; ============================================================================
;; ENVIRONMENT DETECTION
;; ============================================================================
(defvar emacs-ide-wayland-p
  (and (getenv "WAYLAND_DISPLAY") t)
  "Whether running on Wayland (boolean).")

(defvar emacs-ide-display-server
  (cond (emacs-ide-wayland-p "Wayland")
        ((getenv "DISPLAY") "X11")
        (t "TTY"))
  "Current display server.")

(defvar emacs-ide-processor-count
  (emacs-ide--get-processor-count)
  "Number of CPU cores.")

;; ============================================================================
;; SECURITY - EARLY TLS CONFIGURATION
;; ============================================================================
(emacs-ide--benchmark-phase "tls-security"
  (lambda ()
    (require 'gnutls)
    (setq gnutls-verify-error t
          gnutls-min-prime-bits 3072
          tls-checktrust t
          tls-program '("gnutls-cli --x509cafile %t -p %p %h"))))

;; ============================================================================
;; EMERGENCY RECOVERY MODE
;; ============================================================================
(defvar emacs-ide-safe-mode nil
  "If non-nil, boot in safe mode (minimal config).")

(when (or (getenv "EMACS_SAFE_MODE")
          (member "--safe" command-line-args-left))
  (setq emacs-ide-safe-mode t)
  (setq command-line-args-left (delete "--safe" command-line-args-left))
  (message "🛡️  EMACS IDE SAFE MODE ENABLED - Minimal configuration"))

(provide 'early-init)
;;; early-init.el ends here
