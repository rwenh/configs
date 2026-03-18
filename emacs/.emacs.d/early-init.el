;;; early-init.el --- Enterprise Emacs IDE Early Initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Production-grade early initialization with performance monitoring.
;;; Version: 2.2.7
;;; Fixes vs 2.2.6:
;;;   - FIX-TOOLBAR: tool-bar-mode and scroll-bar-mode called unconditionally
;;;     in the ui-disable benchmark phase. Both are absent in TTY/batch mode
;;;     and some Emacs builds without GUI support. The void-function error
;;;     aborted early-init.el before emacs-ide-early-init-report was defined,
;;;     making the command permanently missing from the spot-check.
;;;     Fix: guard both with (when (fboundp ...)).
;;;   - FIX-WARNLIST: add-to-list on warning-suppress-log-types crashed with
;;;     void-variable in batch mode where the variable is not pre-bound.
;;;     Fix: ensure the variable exists with defvar before adding to it.
;;; Fixes vs 2.2.5:
;;;   - FIX-ORDER: file-handler-disable moved to LAST benchmark phase. Previously
;;;     it ran at position 7 of 17, leaving tls-security, treesit-setup,
;;;     jit-lock-optimization, backup-disable, and others running with a stripped
;;;     file-name-handler-alist. (require 'gnutls) in tls-security is the most
;;;     fragile — require needs handlers to locate .elc files.
;;;   - FIX-ORDER2: warning-suppression moved to FIRST benchmark phase (before
;;;     initial-theme, site-lisp-disable, and all others that may trigger warnings).
;;;   - FIX-DEDUP: native-comp-async-report-warnings-errors removed from
;;;     warning-suppression phase — already set in native-comp-setup (line 143).
;;;   - FIX-DEFVAR: emacs-ide--saved-byte-compile-warnings and
;;;     emacs-ide--saved-warning-suppress-log-types moved to top-level defvars
;;;     so the byte-compiler sees them as known globals.
;;;   - FIX-USELESS: package-native-compile t removed — straight.el bypasses
;;;     package.el entirely; this setting had no effect.
;;;   - FIX-COMMENT: initial-theme phase comment updated (was "MODUS VIVENDI").
;;;   - FIX-SCROLL: scroll-error-top-bottom t added in redisplay-optimization
;;;     to suppress the "Beginning of buffer [22 times]" message from dashboard.
;;; Fixes vs 2.2.4 (retained):
;;;   - FIX-WARN2: warning-suppress-log-types used (not warning-suppress-types).
;;; Fixes vs 2.2.3 (retained):
;;;   - FIX-4: emacs-ide--get-processor-count called once, cached in defvar.
;;;   - FIX-WARN2: Previous fix used warning-suppress-types, which only
;;;     suppresses the popup notification — the warnings still appear in
;;;     *Warnings* and in the log. The correct variable is
;;;     warning-suppress-log-types (Emacs 28+), which suppresses both the
;;;     popup AND the log entry. Changed both the set and restore to use
;;;     warning-suppress-log-types so the four ef-themes defvaralias lines
;;;     are completely absent from the warnings buffer.
;;; Fixes vs 2.2.3:
;;;   - FIX-WARN: (now superseded by FIX-WARN2)
;;;   - FIX-4: emacs-ide--get-processor-count called twice — cached in defvar.
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

;; FIX-4: Call once here and cache. native-comp-setup references this var
;; directly so the shell subprocess is spawned at most once during early-init.
(defvar emacs-ide-processor-count
  (emacs-ide--get-processor-count)
  "Number of CPU cores — cached at early-init time.")

;; FIX-DEFVAR: Top-level defvars for save/restore variables used in benchmark
;; phases and emacs-startup-hook. Defined here so the byte-compiler sees them
;; as known globals rather than free variables inside lambda bodies.
(defvar emacs-ide--saved-byte-compile-warnings nil
  "Saved value of byte-compile-warnings — restored in emacs-startup-hook.")
(defvar emacs-ide--saved-warning-suppress-log-types nil
  "Saved value of warning-suppress-log-types — restored in emacs-startup-hook.")

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
    ;; FIX-TOOLBAR: tool-bar-mode and scroll-bar-mode are absent in TTY/batch
    ;; and some Emacs builds. Calling them unconditionally throws void-function,
    ;; aborting early-init.el before emacs-ide-early-init-report is defined.
    (when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))))

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
;; REDUCE STARTUP NOISE
;; FIX-ORDER2: Moved to BEFORE native-comp-setup and initial-theme so that
;; all subsequent phases (including (require 'gnutls) in tls-security and
;; set-face-attribute in initial-theme) run with warnings suppressed.
;; BUG-02 FIX: byte-compile-warnings saved here (defvar is now top-level).
;; ============================================================================
(emacs-ide--benchmark-phase "warning-suppression"
  (lambda ()
    (setq emacs-ide--saved-byte-compile-warnings
          (if (boundp 'byte-compile-warnings) byte-compile-warnings t))
    (setq warning-minimum-level :emergency
          byte-compile-warnings nil)
    ;; FIX-DEDUP: native-comp-async-report-warnings-errors not set here —
    ;; already set to nil in native-comp-setup.
    ;; FIX-WARN2: warning-suppress-log-types suppresses both popup AND log.
    (setq emacs-ide--saved-warning-suppress-log-types
          (if (boundp 'warning-suppress-log-types) warning-suppress-log-types nil))
    ;; FIX-WARNLIST: add-to-list crashes if warning-suppress-log-types is void.
    ;; Ensure it exists before adding to it.
    (unless (boundp 'warning-suppress-log-types)
      (defvar warning-suppress-log-types nil))
    (add-to-list 'warning-suppress-log-types '(defvaralias))))
;; ============================================================================
;; NATIVE COMPILATION - SILENT & OPTIMIZED
;; ============================================================================
(emacs-ide--benchmark-phase "native-comp-setup"
  (lambda ()
    (when (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (setq native-comp-async-report-warnings-errors nil
            native-comp-speed 2
            ;; FIX-4: use cached emacs-ide-processor-count (not calling fn again)
            native-comp-async-jobs-number (max 1 (/ (or emacs-ide-processor-count 4) 2)))
      ;; FIX-USELESS: package-native-compile removed — straight.el bypasses
      ;; package.el so this had no effect on native compilation of packages.

      (when (boundp 'native-comp-jit-compilation)
        (setq native-comp-jit-compilation t))

      (when (fboundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache
         (convert-standard-filename
          (expand-file-name "var/eln-cache/" user-emacs-directory)))))))

;; ============================================================================
;; FILE HANDLER OPTIMIZATION

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
                    emacs-ide--saved-byte-compile-warnings))
            ;; FIX-WARN2: Restore warning-suppress-log-types after startup.
            (when (boundp 'emacs-ide--saved-warning-suppress-log-types)
              (setq warning-suppress-log-types
                    emacs-ide--saved-warning-suppress-log-types)))
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
          highlight-nonselected-windows nil
          ;; FIX-SCROLL: suppress "Beginning of buffer [N times]" message
          ;; from dashboard's scroll functions hitting the buffer boundary.
          scroll-error-top-bottom t)))

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
;; INITIAL APPEARANCE - PRE-THEME FLASH PREVENTION
;; FIX-COMMENT: Was "MODUS VIVENDI COLORS" — we use ef-themes. These are
;; ef-dark approximations used only until ui-core.el loads the real theme.
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

;; NOTE: emacs-ide-processor-count is defined earlier (after
;; emacs-ide--get-processor-count) so it is available to native-comp-setup.

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
;; FILE HANDLER OPTIMIZATION
;; FIX-ORDER: Moved to LAST benchmark phase. All other phases (tls-security,
;; treesit-setup, jit-lock-optimization, backup-disable, etc.) need the full
;; file-name-handler-alist intact — especially (require 'gnutls) in tls-security
;; which calls require to locate .elc files via the handler list.
;; ============================================================================
(emacs-ide--benchmark-phase "file-handler-disable"
  (lambda ()
    (setq file-name-handler-alist nil)))

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
