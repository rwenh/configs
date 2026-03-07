;;; ui-modeline.el --- Modeline configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Activate a modeline package if available; do not force-load on startup.
;;; Version: 2.2.3
;;; Fixes:
;;;   - doom-modeline was activated both here (via after-init-hook) AND in
;;;     ui-core.el (also via after-init-hook after fix). Having two hooks
;;;     caused doom-modeline-mode to run twice on every startup.
;;;     Resolution: ui-core.el owns doom-modeline setup (it sets all the
;;;     configuration vars). This file now only activates a FALLBACK modeline
;;;     if doom-modeline is NOT available, so the two files are non-overlapping.
;;;   - 2.2.3: (unless (featurep 'doom-modeline) ...) evaluated at load time,
;;;     before after-init-hook fires and before doom-modeline-mode runs.
;;;     featurep was always nil here, so the fallback block always ran —
;;;     it re-added a second doom-modeline-mode hook from this file.
;;;     Fixed: use (locate-library "doom-modeline") which tests availability
;;;     at load time without depending on activation order.
;;; Code:

;; doom-modeline is fully configured and activated in ui-core.el.
;; This file only handles the fallback case where doom-modeline is absent.
;; FIX 2.2.3: The previous guard was (unless (featurep 'doom-modeline) ...).
;;   featurep tests whether a feature is already loaded/provided. At load time
;;   (during the feature-modules phase of init.el), doom-modeline has not yet
;;   called (provide 'doom-modeline) because doom-modeline-mode runs in
;;   after-init-hook — so featurep was always nil and the fallback block always
;;   ran, adding a second after-init-hook for doom-modeline-mode.
;;   Fix: use locate-library which tests *availability on disk* at load time,
;;   independent of whether it has been activated yet.
(unless (locate-library "doom-modeline")
  ;; doom-modeline not installed — activate powerline as fallback if available
  (when (require 'powerline nil 'noerror)
    (add-hook 'after-init-hook (lambda () (powerline-default-theme)))))


;; ============================================================================
;; HEALTH SEGMENT — shows IDE health status in the modeline
;; Wired to emacs-ide-health--summary-string from emacs-ide-health.el.
;; Displays: "✓ Healthy" / "⚠ 2 warnings" / "✗ 1 error"
;; Clicking it runs emacs-ide-health-check-all.
;; The segment is registered after doom-modeline loads so it can be
;; appended to the modeline format safely.
;; ============================================================================
(defun emacs-ide-modeline--health-string ()
  "Return health status string for the modeline, or empty string if unchecked."
  (if (fboundp 'emacs-ide-health--summary-string)
      (let* ((s   (emacs-ide-health--summary-string))
             (face (cond
                    ((string-prefix-p "✗" s) '(:foreground "#ff6c6b" :weight bold))
                    ((string-prefix-p "⚠" s) '(:foreground "#ECBE7B" :weight bold))
                    ((string-prefix-p "?" s) '(:foreground "#888888"))
                    (t                       '(:foreground "#98be65")))))
        (propertize (concat " 🏥" s " ") 'face face
                    'help-echo "IDE Health — click to run full check"
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((map (make-sparse-keymap)))
                                  (define-key map [mode-line mouse-1]
                                    #'emacs-ide-health-check-all)
                                  map)))
    ""))

(with-eval-after-load 'doom-modeline
  ;; Define the segment
  (doom-modeline-def-segment emacs-ide-health
    "IDE health status — ✓ / ⚠N / ✗N."
    (when (doom-modeline--active)
      (emacs-ide-modeline--health-string)))

  ;; Define a new modeline format that is the standard main format
  ;; plus our health segment appended.
  ;; doom-modeline-format--main is a macro-generated symbol, not a list var —
  ;; it cannot be modified with add-to-list. Instead we redefine the modeline.
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc
      mu4e gnus github debug repl lsp minor-modes input-method
      indent-info buffer-encoding major-mode process vcs
      emacs-ide-health))  ; <-- appended here, always last
  )

;; Fallback: if doom-modeline is absent, append to the global mode string
;; so the health status still appears in the default modeline
(unless (locate-library "doom-modeline")
  (unless (member '(:eval (emacs-ide-modeline--health-string))
                  global-mode-string)
    (add-to-list 'global-mode-string
                 '(:eval (emacs-ide-modeline--health-string))
                 t)))

(provide 'ui-modeline)
;;; ui-modeline.el ends here
