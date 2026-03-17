;;; ui-dashboard.el --- Office Dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;; Version: 3.0.5
;;; Fix 3.0.5: emacs-ide-dashboard--workspace-section had 2 extra closing
;;;   parens — one after '(:foreground "#888888") and one after (insert " ").
;;;   This was a pre-existing bug in the original source. The imbalance caused
;;;   (invalid-read-syntax ) 72 22) at load time, crashing the entire module
;;;   and blocking Emacs startup (5912s hang in the warning log).
;;; Fix 3.0.4 (retained): dashboard-item-generators/shortcuts registration moved inside
;;;   the use-package :config block (FIX-7). Top-level defvar + add-to-list
;;;   before dashboard loaded clobbered dashboard's own variable defaults.
;;; Fix 3.0.3 (retained): Cancel void timers before dashboard-setup-startup-hook.
;;; Fix 3.0.2 (retained): :demand t.
;;; Code:

;; ============================================================================
;; HEALTH WIDGET
;; ============================================================================
(defun emacs-ide-dashboard--health-section (list-size)
  "Dashboard health widget. LIST-SIZE ignored."
  (let* ((results  (and (boundp 'emacs-ide-health-results) emacs-ide-health-results))
         (checked  (and (boundp 'emacs-ide-health-last-check) emacs-ide-health-last-check))
         (errors   (if (boundp 'emacs-ide-health--last-errors)   emacs-ide-health--last-errors   0))
         (warnings (if (boundp 'emacs-ide-health--last-warnings) emacs-ide-health--last-warnings 0)))
    (if (not checked)
        (insert (propertize "  󰣐 Health check pending...\n" 'face '(:foreground "#888888")))
      (let* ((face (cond ((> errors   0) '(:foreground "#ff6c6b" :weight bold))
                         ((> warnings 0) '(:foreground "#ECBE7B" :weight bold))
                         (t              '(:foreground "#98be65" :weight bold))))
             (str  (cond ((> errors   0) (format "✗ %d error%s, %d warning%s"
                                                  errors (if (= errors 1) "" "s")
                                                  warnings (if (= warnings 1) "" "s")))
                         ((> warnings 0) (format "⚠ %d warning%s" warnings (if (= warnings 1) "" "s")))
                         (t "✓ All checks passed"))))
        (insert (propertize (format "  󰣐 IDE Health: %s\n" str) 'face face))
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
            (insert (propertize (format "%s %-20s %s\n" icon name (or msg "")) 'face face))))
        (when (or (> errors 0) (> warnings 0))
          (insert-button "  → Run full health check"
                         'action (lambda (_) (call-interactively #'emacs-ide-health-check-all))
                         'follow-link t
                         'face '(:foreground "#51afef" :underline t))
          (insert "\n"))))))

;; ============================================================================
;; WORKSPACE WIDGET
;; ============================================================================
(defun emacs-ide-dashboard--workspace-section (list-size)
  "Show active workspaces. LIST-SIZE ignored."
  (if (not (fboundp 'persp-names))
      (insert (propertize "  󰡕 Workspaces: perspective.el not loaded\n"
                          'face '(:foreground "#888888")))
    (let ((names   (persp-names))
          (current (when (fboundp 'persp-current-name) (persp-current-name))))
      (insert (propertize "  󰡕 Workspaces: " 'face '(:weight bold)))
      (dolist (name names)
        (let ((active (string= name current)))
          (insert-button (format "[%s]" name)
                         'action (let ((n name)) (lambda (_) (persp-switch n)))
                         'follow-link t
                         'face (if active
                                   '(:foreground "#51afef" :weight bold)
                                 '(:foreground "#888888")))
          (insert " ")))
      (insert "\n"))))

;; ============================================================================
;; QUICK ACTIONS WIDGET
;; ============================================================================
(defun emacs-ide-dashboard--actions-section (list-size)
  "Quick-action buttons row. LIST-SIZE ignored."
  (insert "  ")
  (dolist (action '(("  New file"       . find-file)
                    ("  Find project"   . projectile-switch-project)
                    ("  Git status"     . magit-status)
                    ("  Run tests"      . emacs-ide-test-run)
                    ("  Toggle theme"   . emacs-ide-toggle-theme)
                    ("󱓟  Config"         . emacs-ide-config-edit)))
    (insert-button (car action)
                   'action (let ((cmd (cdr action))) (lambda (_) (call-interactively cmd)))
                   'follow-link t
                   'face '(:foreground "#51afef" :underline nil :box (:line-width 1)))
    (insert "  "))
  (insert "\n"))

;; ============================================================================
;; DASHBOARD SETUP
;; ============================================================================
(when (or (not (boundp 'emacs-ide-feature-dashboard)) emacs-ide-feature-dashboard)

  (use-package dashboard
    :demand t
    :init
    (setq dashboard-startup-banner      'logo
          dashboard-center-content      t
          dashboard-set-heading-icons   t
          dashboard-set-file-icons      t
          dashboard-force-refresh       nil
          dashboard-items
          '((ide-actions   . 1)
            (ide-workspace . 1)
            (ide-health    . 1)
            (recents        . 10)
            (projects       . 8)
            (bookmarks      . 5))
          dashboard-set-navigator       t
          dashboard-set-init-info       t
          dashboard-banner-logo-title
          (format "Emacs IDE v%s  ·  %s  ·  %d packages"
                  (or (bound-and-true-p emacs-ide-version) "?")
                  (format-time-string "%a %d %b")
                  (condition-case nil
                      (cond
                       ((and (boundp 'straight--recipe-cache)
                             (hash-table-p straight--recipe-cache))
                        (hash-table-count straight--recipe-cache))
                       ((and (fboundp 'straight--build-cache)
                             (hash-table-p straight--build-cache))
                        (hash-table-count straight--build-cache))
                       ((file-directory-p (expand-file-name
                                           "straight/build" user-emacs-directory))
                        (length (directory-files
                                 (expand-file-name "straight/build" user-emacs-directory)
                                 nil "^[^.]")))
                       (t 0))
                    (error 0)))
          dashboard-footer-messages
          '("M-x butterfly  ·  C-h C  ·  M-x tetris"))
    :config
    ;; FIX-7: Register custom dashboard items here, inside :config, so that
    ;; dashboard's own defvar for these variables has already run. Doing this
    ;; at top level (before dashboard loads) created nil-valued variables that
    ;; clobbered dashboard's defaults via defvar's no-op-if-already-bound rule.
    (add-to-list 'dashboard-item-generators
                 '(ide-health    . emacs-ide-dashboard--health-section))
    (add-to-list 'dashboard-item-generators
                 '(ide-workspace . emacs-ide-dashboard--workspace-section))
    (add-to-list 'dashboard-item-generators
                 '(ide-actions   . emacs-ide-dashboard--actions-section))
    (add-to-list 'dashboard-item-shortcuts '(ide-health    . "h"))
    (add-to-list 'dashboard-item-shortcuts '(ide-workspace . "w"))
    (when (fboundp 'dashboard-resize-on-hook)
      (fset 'dashboard-resize-on-hook #'ignore))
    ;; FIX 3.0.3: Cancel any void-slot timers that dashboard's own load may have
    ;; registered BEFORE calling dashboard-setup-startup-hook, which registers
    ;; more. Running this here fires the cleanup synchronously, before any of
    ;; dashboard's deferred timers can execute and print "void: nil".
    (when (fboundp 'emacs-ide--find-and-cancel-void-timers)
      (emacs-ide--find-and-cancel-void-timers))
    (when (fboundp 'dashboard-setup-startup-hook)
      (dashboard-setup-startup-hook))
    ;; Second cleanup pass after dashboard-setup-startup-hook, which may itself
    ;; register additional timers. Both passes together cover the full window.
    (when (fboundp 'emacs-ide--find-and-cancel-void-timers)
      (emacs-ide--find-and-cancel-void-timers))
    ;; Refresh after health check fires
    (run-with-idle-timer 4 nil
                         (lambda ()
                           (when (get-buffer "*dashboard*")
                             (with-current-buffer "*dashboard*"
                               (when (fboundp 'dashboard-refresh-buffer)
                                 (dashboard-refresh-buffer))))))
    (setq initial-buffer-choice
          (lambda ()
            (or (get-buffer (or (bound-and-true-p dashboard-buffer-name)
                                "*dashboard*"))
                (get-buffer-create "*dashboard*"))))))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here
