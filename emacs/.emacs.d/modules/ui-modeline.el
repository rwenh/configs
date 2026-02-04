;;; ui-modeline.el --- Modeline configuration (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Activate a modeline package if available; do not force-load on startup.
;;; Code:

;; Prefer doom-modeline if available, otherwise use fallback
(if (require 'doom-modeline nil 'noerror)
    (progn
      (setq doom-modeline-height (or (and (boundp 'doom-modeline-height) doom-modeline-height) 30)
            doom-modeline-vcs-max-length 20)
      (add-hook 'after-init-hook #'doom-modeline-mode))
  ;; Fallback: use simple modeline or powerline if installed
  (when (require 'powerline nil 'noerror)
    (add-hook 'after-init-hook (lambda () (powerline-default-theme)))))

(provide 'ui-modeline)
;;; ui-modeline.el ends here