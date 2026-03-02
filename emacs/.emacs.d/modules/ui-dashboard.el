;;; ui-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Dashboard with health status widget. Health results from emacs-ide-health.el
;;; are displayed inline on startup — no M-x required to see IDE status.
;;; Version: 2.2.1
;;; Code:

;; ============================================================================
;; HEALTH WIDGET — renders inline in the dashboard
;; ============================================================================
(defun emacs-ide-dashboard--health-section (list-size)
  "Dashboard widget showing IDE health status.
LIST-SIZE is ignored — output length is fixed. Runs after the idle
health check has populated emacs-ide-health-results."
  (let* ((results  (and (boundp 'emacs-ide-health-results)
                        emacs-ide-health-results))
         (checked  (and (boundp 'emacs-ide-health-last-check)
                        emacs-ide-health-last-check))
         (errors   (if (boundp 'emacs-ide-health--last-errors)
                       emacs-ide-health--last-errors 0))
         (warnings (if (boundp 'emacs-ide-health--last-warnings)
                       emacs-ide-health--last-warnings 0)))
    (if (not checked)
        ;; Not yet run — show pending state, will refresh when idle check fires
        (insert (propertize "  🏥 Health check pending (running in background)...
"
                            'face '(:foreground "#888888")))
      ;; Results available — show summary line then per-check status
      (let* ((summary-face (cond ((> errors 0)   '(:foreground "#ff6c6b" :weight bold))
                                 ((> warnings 0) '(:foreground "#ECBE7B" :weight bold))
                                 (t              '(:foreground "#98be65" :weight bold))))
             (summary-str  (cond ((> errors 0)
                                  (format "✗ %d error%s, %d warning%s"
                                          errors   (if (= errors 1) "" "s")
                                          warnings (if (= warnings 1) "" "s")))
                                 ((> warnings 0)
                                  (format "⚠ %d warning%s"
                                          warnings (if (= warnings 1) "" "s")))
                                 (t "✓ All checks passed"))))
        (insert (propertize (format "  🏥 IDE Health: %s
" summary-str)
                            'face summary-face))
        ;; Per-check lines
        (dolist (result results)
          (let* ((name   (car result))
                 (plist  (cdr result))
                 (status (plist-get plist :status))
                 (msg    (plist-get plist :message))
                 (icon   (cond ((eq status 'ok)      "  ✓")
                               ((eq status 'warning) "  ⚠")
                               ((eq status 'error)   "  ✗")
                               (t                    "  ?")))
                 (face   (cond ((eq status 'ok)      '(:foreground "#98be65"))
                               ((eq status 'warning) '(:foreground "#ECBE7B"))
                               ((eq status 'error)   '(:foreground "#ff6c6b"))
                               (t                    '(:foreground "#888888")))))
            (insert (propertize
                     (format "%s %-16s %s
" icon name (or msg ""))
                     'face face))))
        ;; Clickable hint if issues exist
        (when (or (> errors 0) (> warnings 0))
          (insert-button "  → Run full health check"
                         'action (lambda (_) (call-interactively #'emacs-ide-health-check-all))
                         'follow-link t
                         'face '(:foreground "#51afef" :underline t))
          (insert "
"))))))

(when (or (not (boundp 'emacs-ide-feature-dashboard)) emacs-ide-feature-dashboard)
  (use-package dashboard
    :defer t
    :init
    (setq dashboard-startup-banner 'logo
          dashboard-center-content t
          dashboard-set-heading-icons t
          dashboard-set-file-icons t
          dashboard-items '((ide-health . 1)
                           (recents  . 15)
                           (projects . 10)
                           (bookmarks . 10)
                           (agenda . 5))
          dashboard-set-navigator t
          dashboard-set-init-info t
          dashboard-banner-logo-title "EMACS IDE - Professional Development Environment"
          dashboard-footer-messages '("Ready to code"))
    :config
    ;; Register the health widget
    ;; dashboard-item-generators and dashboard-item-shortcuts are plain alists —
    ;; add entries unconditionally, guarded only by dashboard being loaded.
    (add-to-list 'dashboard-item-generators
                 '(ide-health . emacs-ide-dashboard--health-section))
    (add-to-list 'dashboard-item-shortcuts
                 '(ide-health . "h"))
    (when (fboundp 'dashboard-setup-startup-hook)
      (dashboard-setup-startup-hook))
    ;; Refresh dashboard after idle health check fires (3s) so status appears
    (run-with-idle-timer 4 nil
                         (lambda ()
                           (when (get-buffer "*dashboard*")
                             (with-current-buffer "*dashboard*"
                               (when (fboundp 'dashboard-refresh-buffer)
                                 (dashboard-refresh-buffer))))))
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here