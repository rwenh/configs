;;; ui-modeline.el --- Modeline configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Activate a modeline package if available; do not force-load on startup.
;;; doom-modeline is fully configured and activated in ui-core.el.
;;; This file only: (1) handles the fallback when doom-modeline is absent,
;;; (2) defines the IDE health segment and wires it into the main format.
;;; F12 / theme toggle — see keybindings.el.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.4 to 3.0.4.
;;;   - FIX-THEME-COLORS: Health segment face colors replaced with standard
;;;     Emacs faces (error, warning, shadow, success) that adapt to the
;;;     active ef-theme. The old hardcoded hex values (#ff6c6b, #98be65,
;;;     etc.) were from the Doom One dark palette — on ef-light themes,
;;;     #98be65 (dark-mode green) was nearly invisible.
;;;   - FIX-KEYMAP-CACHE: Health segment click keymap now created once in a
;;;     defvar (emacs-ide-modeline--health-map) and reused on every redraw.
;;;     The old make-sparse-keymap inside the propertize call allocated a
;;;     new keymap object on every modeline redraw (every cursor move,
;;;     buffer switch, timer tick) — measurable GC pressure over a session.
;;;   - FIX-POWERLINE-MSG: Powerline fallback now emits a message when
;;;     neither doom-modeline nor powerline is available, so users know
;;;     why they have no enhanced modeline.
;;;   - FIX-ACTIVE-GUARD: doom-modeline--active replaced with a standard
;;;     selected-window check — the double-dash function is private and may
;;;     be renamed/removed in future doom-modeline versions.
;;; Fixes vs 2.2.4 (retained):
;;;   - M-28: one-time feature flag prevents modeline format from being
;;;     redefined on every config reload.
;;;   - featurep → locate-library for load-time availability test.
;;;   - Non-overlapping with ui-core.el doom-modeline activation.
;;; Code:

;; ============================================================================
;; FALLBACK MODELINE
;; doom-modeline is configured and activated in ui-core.el.
;; Only activate a fallback when doom-modeline is not installed at all.
;; FIX-POWERLINE-MSG: inform user when neither modeline package is available.
;; ============================================================================
(unless (locate-library "doom-modeline")
  (if (require 'powerline nil 'noerror)
      (add-hook 'after-init-hook (lambda () (powerline-default-theme)))
    (message "ui-modeline: neither doom-modeline nor powerline found — using default modeline")))

;; ============================================================================
;; HEALTH SEGMENT KEYMAP — created once, reused on every redraw
;; FIX-KEYMAP-CACHE: defvar ensures the keymap is allocated once per session
;; rather than on every modeline redraw (every cursor move / timer tick).
;; ============================================================================
(defvar emacs-ide-modeline--health-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'emacs-ide-health-check-all)
    map)
  "Keymap for the IDE health modeline segment click action.")

;; ============================================================================
;; HEALTH SEGMENT STRING
;; FIX-THEME-COLORS: use standard Emacs faces (error, warning, shadow,
;; success) instead of hardcoded hex values. These faces adapt to the
;; active ef-theme, so the segment is readable on both dark and light themes.
;; FIX-ACTIVE-GUARD: replaced (doom-modeline--active) with a standard
;; selected-window check to avoid depending on a private function.
;; ============================================================================
(defun emacs-ide-modeline--health-string ()
  "Return health status string for the modeline, or empty string if unchecked."
  (if (fboundp 'emacs-ide-health--summary-string)
      (let* ((s    (emacs-ide-health--summary-string))
             ;; FIX-THEME-COLORS: standard faces adapt to the active theme
             (face (cond
                    ((string-prefix-p "✗" s) 'error)
                    ((string-prefix-p "⚠" s) 'warning)
                    ((string-prefix-p "?" s) 'shadow)
                    (t                       'success))))
        (propertize (concat " 🏥" s " ")
                    'face       face
                    'help-echo  "IDE Health — click to run full check"
                    'mouse-face 'mode-line-highlight
                    ;; FIX-KEYMAP-CACHE: reuse cached keymap, not a new one
                    'local-map  emacs-ide-modeline--health-map))
    ""))

;; ============================================================================
;; DOOM-MODELINE HEALTH SEGMENT REGISTRATION
;; M-28 FIX (retained): one-time feature flag prevents redefining 'main on
;; every config reload within the same session.
;; FIX-ACTIVE-GUARD: segment body now uses selected-window instead of the
;; private doom-modeline--active function.
;; ============================================================================
(with-eval-after-load 'doom-modeline
  (unless (featurep 'emacs-ide-modeline-installed)
    ;; Define the health segment
    (doom-modeline-def-segment emacs-ide-health
      "IDE health status — ✓ / ⚠N / ✗N."
      ;; FIX-ACTIVE-GUARD: standard window check replaces private function
      (when (eq (selected-window) (get-buffer-window))
        (emacs-ide-modeline--health-string)))

    ;; Redefine the main modeline appending our health segment.
    ;; persp-name and workspace-name omitted — timing race with after-init-hook
    ;; means they may not be registered when with-eval-after-load fires.
    (doom-modeline-def-modeline 'main
      '(bar window-number modals matches follow buffer-info
        remote-host buffer-position word-count parrot selection-info)
      '(compilation objed-state misc-info battery grip irc
        mu4e gnus github debug repl lsp minor-modes input-method
        indent-info buffer-encoding major-mode process vcs
        emacs-ide-health))

    (provide 'emacs-ide-modeline-installed)))

;; ============================================================================
;; FALLBACK: append health status to global-mode-string when doom-modeline
;; is absent, so the indicator still appears in the default modeline.
;; ============================================================================
(unless (locate-library "doom-modeline")
  (unless (member '(:eval (emacs-ide-modeline--health-string))
                  global-mode-string)
    (add-to-list 'global-mode-string
                 '(:eval (emacs-ide-modeline--health-string))
                 t)))

(provide 'ui-modeline)
;;; ui-modeline.el ends here
