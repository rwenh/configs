;;; ui-dashboard.el --- Dashboard configuration (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Dashboard (deferred) with config-driven enable/disable.
;;; Code:

(when (or (not (boundp 'emacs-ide-feature-dashboard)) emacs-ide-feature-dashboard)
  (use-package dashboard
    :defer t
    :init
    (setq dashboard-startup-banner 'logo
          dashboard-center-content t
          dashboard-set-heading-icons t
          dashboard-set-file-icons t
          dashboard-items '((recents  . 15)
                           (projects . 10)
                           (bookmarks . 10)
                           (agenda . 5))
          dashboard-set-navigator t
          dashboard-set-init-info t
          dashboard-banner-logo-title "EMACS IDE - Professional Development Environment"
          dashboard-footer-messages '("Ready to code"))
    :config
    (when (fboundp 'dashboard-setup-startup-hook)
      (dashboard-setup-startup-hook))
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here