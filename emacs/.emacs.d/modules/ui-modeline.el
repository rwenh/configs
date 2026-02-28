;;; ui-modeline.el --- Modeline configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Activate a modeline package if available; do not force-load on startup.
;;; Version: 2.2.1
;;; Fixes:
;;;   - doom-modeline was activated both here (via after-init-hook) AND in
;;;     ui-core.el (also via after-init-hook after fix). Having two hooks
;;;     caused doom-modeline-mode to run twice on every startup.
;;;     Resolution: ui-core.el owns doom-modeline setup (it sets all the
;;;     configuration vars). This file now only activates a FALLBACK modeline
;;;     if doom-modeline is NOT available, so the two files are non-overlapping.
;;; Code:

;; doom-modeline is fully configured and activated in ui-core.el.
;; This file only handles the fallback case where doom-modeline is absent.
(unless (featurep 'doom-modeline)
  (when (require 'doom-modeline nil 'noerror)
    ;; doom-modeline present but not yet set up — shouldn't happen normally,
    ;; but handle gracefully by deferring to after-init
    (add-hook 'after-init-hook #'doom-modeline-mode))

  ;; True fallback: doom-modeline not installed at all
  (unless (locate-library "doom-modeline")
    (when (require 'powerline nil 'noerror)
      (add-hook 'after-init-hook (lambda () (powerline-default-theme))))))

(provide 'ui-modeline)
;;; ui-modeline.el ends here
