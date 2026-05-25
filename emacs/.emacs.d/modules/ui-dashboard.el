;;; ui-dashboard.el --- Office Dashboard -*- lexical-binding: t -*-
;;; Version: 3.3.0

;;; Code:

(defun emacs-ide-dashboard--health-section (_list-size)
  (let* ((results  (and (boundp 'emacs-ide-health-results)  emacs-ide-health-results))
         (checked  (and (boundp 'emacs-ide-health-last-check) emacs-ide-health-last-check))
         (errors   (if (and (boundp 'emacs-ide-health--errors)
                            (numberp emacs-ide-health--errors))
                       emacs-ide-health--errors 0))
         (warnings (if (and (boundp 'emacs-ide-health--warnings)
                            (numberp emacs-ide-health--warnings))
                       emacs-ide-health--warnings 0)))
    (if (not checked)
        (insert (propertize "  󰣐 Health check pending...\n" 'face 'shadow))
      (let* ((face (cond ((> errors   0) 'error)
                         ((> warnings 0) 'warning)
                         (t              'success)))
             (str  (cond ((> errors   0)
                          (format "✗ %d error%s, %d warning%s"
                                  errors   (if (= errors   1) "" "s")
                                  warnings (if (= warnings 1) "" "s")))
                         ((> warnings 0)
                          (format "⚠ %d warning%s"
                                  warnings (if (= warnings 1) "" "s")))
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
                 (face   (cond ((eq status 'ok)      'success)
                               ((eq status 'warning) 'warning)
                               ((eq status 'error)   'error)
                               (t                    'shadow))))
            (insert (propertize (format "%s %-20s %s\n" icon name (or msg ""))
                                'face face))))
        (when (or (> errors 0) (> warnings 0))
          (insert-button "  → Run full health check"
                         'action     (lambda (_) (call-interactively #'emacs-ide-health-check-all))
                         'follow-link t
                         'face        'link)
          (insert "\n"))))))

(defun emacs-ide-dashboard--workspace-section (_list-size)
  (if (not (fboundp 'persp-names))
      (insert (propertize "  󰡕 Workspaces: perspective.el not loaded\n" 'face 'shadow))
    (let ((names   (persp-names))
          (current (when (fboundp 'persp-current-name) (persp-current-name))))
      (insert (propertize "  󰡕 Workspaces: " 'face 'bold))
      (dolist (name names)
        (let ((active (string= name current)))
          (insert-button (format "[%s]" name)
                         'action      (let ((n name)) (lambda (_) (persp-switch n)))
                         'follow-link t
                         'face        (if active 'link 'shadow))
          (insert " ")))
      (insert "\n"))))

(defun emacs-ide-dashboard--actions-section (_list-size)
  (insert "  ")
  (dolist (action '(("  New file"       . find-file)
                    ("  Find project"   . projectile-switch-project)
                    ("  Git status"     . magit-status)
                    ("  Run tests"      . emacs-ide-test-run)
                    ("  Toggle theme"   . emacs-ide-toggle-theme)
                    ("󱓟  Config"         . emacs-ide-config-edit)))
    (insert-button (car action)
                   'action      (let ((cmd (cdr action)))
                                  (lambda (_) (call-interactively cmd)))
                   'follow-link t
                   'face        '(:inherit link :box (:line-width 1)))
    (insert "  "))
  (insert "\n"))

(defun emacs-ide-dashboard--kill-on-file-open ()
  "Kill dashboard when opening first file."
  (when (and buffer-file-name
             (not (string= (buffer-name) "*dashboard*")))
    (let ((db-buf (get-buffer (or (bound-and-true-p dashboard-buffer-name)
                                  "*dashboard*"))))
      (when db-buf
        (let ((db-win (get-buffer-window db-buf)))
          (when (and db-win (not (one-window-p)))
            (delete-window db-win)))
        (kill-buffer db-buf))))
  (remove-hook 'find-file-hook #'emacs-ide-dashboard--kill-on-file-open))

;; Main configuration
(when (or (not (boundp 'emacs-ide-feature-dashboard)) emacs-ide-feature-dashboard)

  (use-package dashboard
    :demand t
    :init
    (setq dashboard-startup-banner    'ascii
          dashboard-center-content    t
          dashboard-set-heading-icons t
          dashboard-set-file-icons    t
          dashboard-items
          '((ide-actions   . 1)
            (ide-workspace . 1)
            (ide-health    . 1)
            (recents        . 10)
            (projects       . 8)
            (bookmarks      . 5))
          dashboard-set-navigator  t
          dashboard-set-init-info  t
          dashboard-banner-logo-title
          (format "Emacs IDE v%s  ·  %s  ·  %d packages"
                  (or (bound-and-true-p emacs-ide-version) "?")
                  (format-time-string "%a %d %b")
                  (condition-case nil
                      (cond
                       ((and (boundp 'straight--recipe-cache)
                             (hash-table-p straight--recipe-cache))
                        (hash-table-count straight--recipe-cache))
                       ((file-directory-p
                         (expand-file-name "straight/build" user-emacs-directory))
                        (length (directory-files
                                 (expand-file-name "straight/build" user-emacs-directory)
                                 nil "^[^.]")))
                       (t 0))
                    (error 0)))
          dashboard-footer-messages
          '("M-x butterfly  ·  C-h C  ·  M-x tetris"))

    :config
    (add-to-list 'dashboard-item-generators
                 '(ide-health    . emacs-ide-dashboard--health-section))
    (add-to-list 'dashboard-item-generators
                 '(ide-workspace . emacs-ide-dashboard--workspace-section))
    (add-to-list 'dashboard-item-generators
                 '(ide-actions   . emacs-ide-dashboard--actions-section))
    (add-to-list 'dashboard-item-shortcuts '(ide-health    . "h"))
    (add-to-list 'dashboard-item-shortcuts '(ide-workspace . "w"))

    ;; === FIXES FOR SCROLL WARNING ===
    (with-eval-after-load 'pixel-scroll
      (setq pixel-scroll-precision-interpolate-page nil))

    ;; Disable dashboard's own resize hook that often causes scroll issues
    (when (fboundp 'dashboard-resize-on-hook)
      (fset 'dashboard-resize-on-hook #'ignore))

    (when (fboundp 'dashboard-setup-startup-hook)
      (dashboard-setup-startup-hook))

    ;; Safer refresh with scroll protection
    (run-with-idle-timer 5 nil
                         (lambda ()
                           (when (get-buffer "*dashboard*")
                             (with-current-buffer "*dashboard*"
                               (let ((pixel-scroll-precision-mode nil))
                                 (when (fboundp 'dashboard-refresh-buffer)
                                   (dashboard-refresh-buffer)))))))

    ;; Add kill hook safely
    (unless (memq #'emacs-ide-dashboard--kill-on-file-open find-file-hook)
      (add-hook 'find-file-hook #'emacs-ide-dashboard--kill-on-file-open t)) ; t = append

    ;; Set as initial buffer only if session restore is disabled
    (let ((restore-session (and (fboundp 'emacs-ide-config-get)
                                (emacs-ide-config-get 'general 'restore-session nil))))
      (unless restore-session
        (setq initial-buffer-choice
              (lambda ()
                (or (get-buffer (or (bound-and-true-p dashboard-buffer-name)
                                    "*dashboard*"))
                    (get-buffer-create "*dashboard*"))))))))

(provide 'ui-dashboard)
;;; ui-dashboard.el ends here
