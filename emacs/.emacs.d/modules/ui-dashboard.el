;;; ui-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Dashboard with health status widget.
;;; Version: 2.2.4
;;; Fixes:
;;;   - 2.2.4: Emacs 30 / dashboard void-function nil fix.
;;;     dashboard-setup-startup-hook in Emacs 30 registers nil into
;;;     window-size-change-functions / window-configuration-change-hook,
;;;     producing repeated "Error muted by safe_call: (void-function nil)"
;;;     on every window resize or buffer switch. Fixed by purging nil
;;;     entries from the relevant hooks via a 0.5s idle timer after setup.
;;;   - 2.2.3: (inherited) Removed '(agenda . 5)' from dashboard-items.
;;;   - 2.2.2: (inherited) Fixed ide-health widget missing on first paint.
;;; Code:

;; ============================================================================
;; HEALTH WIDGET — renders inline in the dashboard
;; ============================================================================
(defun emacs-ide-dashboard--health-section (list-size)
  "Dashboard widget showing IDE health status.
LIST-SIZE is ignored — output length is fixed."
  (let* ((results  (and (boundp 'emacs-ide-health-results)
                        emacs-ide-health-results))
         (checked  (and (boundp 'emacs-ide-health-last-check)
                        emacs-ide-health-last-check))
         (errors   (if (boundp 'emacs-ide-health--last-errors)
                       emacs-ide-health--last-errors 0))
         (warnings (if (boundp 'emacs-ide-health--last-warnings)
                       emacs-ide-health--last-warnings 0)))
    (if (not checked)
        (insert (propertize "  🏥 Health check pending (running in background)...\n"
                            'face '(:foreground "#888888")))
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
        (insert (propertize (format "  🏥 IDE Health: %s\n" summary-str)
                            'face summary-face))
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
                     (format "%s %-16s %s\n" icon name (or msg ""))
                     'face face))))
        (when (or (> errors 0) (> warnings 0))
          (insert-button "  → Run full health check"
                         'action (lambda (_) (call-interactively #'emacs-ide-health-check-all))
                         'follow-link t
                         'face '(:foreground "#51afef" :underline t))
          (insert "\n"))))))

(when (or (not (boundp 'emacs-ide-feature-dashboard)) emacs-ide-feature-dashboard)

  ;; Pre-declare dashboard registration alists before the package loads.
  (defvar dashboard-item-generators nil
    "Pre-declared by ui-dashboard.el so ide-health is registered before load.")

  (defvar dashboard-item-shortcuts nil
    "Pre-declared by ui-dashboard.el for the same reason.")

  (add-to-list 'dashboard-item-generators
               '(ide-health . emacs-ide-dashboard--health-section))
  (add-to-list 'dashboard-item-shortcuts
               '(ide-health . "h"))

  (use-package dashboard
    :demand t
    :init
    (setq dashboard-startup-banner      'logo
          dashboard-center-content      t
          dashboard-set-heading-icons   t
          dashboard-set-file-icons      t
          ;; FIX 2.2.3: 'agenda removed from dashboard-items.
          ;; Including agenda forces org-mode to load and scan all
          ;; org-agenda-files synchronously during startup, adding 3-8s.
          ;; Use C-c a for agenda — it should not be on the startup
          ;; critical path.
          dashboard-items '((ide-health . 1)
                            (recents    . 15)
                            (projects   . 10)
                            (bookmarks  . 10))
          dashboard-set-navigator       t
          dashboard-set-init-info       t
          dashboard-banner-logo-title   "EMACS IDE - Professional Development Environment"
          dashboard-footer-messages     '("Ready to code"))
    :config
    (when (fboundp 'dashboard-setup-startup-hook)
      (dashboard-setup-startup-hook))
    ;; FIX 2.2.4: Emacs 30 / dashboard void-function nil.
    ;; dashboard-setup-startup-hook registers nil into window resize hooks,
    ;; producing "Error muted by safe_call: (void-function nil)" on every
    ;; window event. Purge nil entries from affected hooks after setup.
    (run-with-idle-timer
     0.5 nil
     (lambda ()
       (dolist (hook '(window-size-change-functions
                       window-configuration-change-hook
                       window-state-change-hook))
         (when (boundp hook)
           (set hook (delq nil (symbol-value hook)))))))
    ;; Refresh after idle health check fires (3s)
    (run-with-idle-timer 4 nil
                         (lambda ()
                           (when (get-buffer "*dashboard*")
                             (with-current-buffer "*dashboard*"
                               (when (fboundp 'dashboard-refresh-buffer)
                                 (dashboard-refresh-buffer))))))
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here
