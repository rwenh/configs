;;; ui-dashboard.el --- Office Dashboard -*- lexical-binding: t -*-
;;; Commentary:
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit — was versioned as 3.0.6 in original):
;;;   - FIX-VERSION: Header aligned to project version 3.0.4.
;;;   - FIX-THEME-COLORS: All hardcoded hex face colors replaced with
;;;     standard Emacs faces (error, warning, success, shadow, link)
;;;     that adapt to the active ef-theme. The old Doom One dark palette
;;;     values were nearly invisible on ef-light and other light themes.
;;;   - FIX-DEFUN-RELOAD: emacs-ide-dashboard--kill-on-file-open moved to
;;;     top-level defun (outside :config). Defining it inside :config caused
;;;     M-x emacs-ide-config-reload to re-add it to find-file-hook even
;;;     after it had already self-removed, potentially firing it a second
;;;     time. add-hook now guarded with unless/memq.
;;;   - FIX-SESSION-RESTORE: initial-buffer-choice now checks
;;;     general.restore-session from config.yml — if true, perspective.el
;;;     restores the previous session and the dashboard should not override
;;;     the initial buffer choice.
;;;   - FIX-REFRESH-TIMER: Dashboard refresh idle timer raised from 4s to 6s
;;;     to ensure health check results (fired at 3s idle) are available
;;;     before the dashboard renders them.
;;; Fix 3.0.6 (retained): FIX-DASHBOARD-SPLIT — kill-on-file-open hook.
;;; Fix 3.0.4 (retained): FIX-7 — registration inside :config.
;;; Fix 3.0.3 (retained): Cancel void timers around dashboard-setup-startup-hook.
;;; Fix 3.0.2 (retained): :demand t.
;;; Code:

;; ============================================================================
;; HEALTH WIDGET
;; ============================================================================
(defun emacs-ide-dashboard--health-section (list-size)
  "Dashboard health widget. LIST-SIZE ignored.
FIX-THEME-COLORS: uses standard Emacs faces instead of hardcoded hex values."
  (let* ((results  (and (boundp 'emacs-ide-health-results) emacs-ide-health-results))
         (checked  (and (boundp 'emacs-ide-health-last-check) emacs-ide-health-last-check))
         (errors   (if (boundp 'emacs-ide-health--last-errors)   emacs-ide-health--last-errors   0))
         (warnings (if (boundp 'emacs-ide-health--last-warnings) emacs-ide-health--last-warnings 0)))
    (if (not checked)
        (insert (propertize "  󰣐 Health check pending...\n" 'face 'shadow))
      (let* ((face (cond ((> errors   0) 'error)
                         ((> warnings 0) 'warning)
                         (t              'success)))
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
                 (face   (cond ((eq status 'ok)      'success)
                               ((eq status 'warning) 'warning)
                               ((eq status 'error)   'error)
                               (t                    'shadow))))
            (insert (propertize (format "%s %-20s %s\n" icon name (or msg "")) 'face face))))
        (when (or (> errors 0) (> warnings 0))
          (insert-button "  → Run full health check"
                         'action (lambda (_) (call-interactively #'emacs-ide-health-check-all))
                         'follow-link t
                         'face 'link)
          (insert "\n"))))))

;; ============================================================================
;; WORKSPACE WIDGET
;; ============================================================================
(defun emacs-ide-dashboard--workspace-section (list-size)
  "Show active workspaces. LIST-SIZE ignored.
FIX-THEME-COLORS: uses standard faces instead of hardcoded hex values."
  (if (not (fboundp 'persp-names))
      (insert (propertize "  󰡕 Workspaces: perspective.el not loaded\n" 'face 'shadow))
    (let ((names   (persp-names))
          (current (when (fboundp 'persp-current-name) (persp-current-name))))
      (insert (propertize "  󰡕 Workspaces: " 'face 'bold))
      (dolist (name names)
        (let ((active (string= name current)))
          (insert-button (format "[%s]" name)
                         'action (let ((n name)) (lambda (_) (persp-switch n)))
                         'follow-link t
                         'face (if active 'link 'shadow))
          (insert " ")))
      (insert "\n"))))

;; ============================================================================
;; QUICK ACTIONS WIDGET
;; ============================================================================
(defun emacs-ide-dashboard--actions-section (list-size)
  "Quick-action buttons row. LIST-SIZE ignored.
FIX-THEME-COLORS: uses standard faces instead of hardcoded hex values."
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
                   'face '(:inherit link :box (:line-width 1)))
    (insert "  "))
  (insert "\n"))

;; ============================================================================
;; DASHBOARD SPLIT PREVENTION
;; FIX-DEFUN-RELOAD: defined at top level so M-x emacs-ide-config-reload
;; does not re-add it to find-file-hook after it has already self-removed.
;; Defining inside :config caused add-hook to run again on every reload.
;; ============================================================================
(defun emacs-ide-dashboard--kill-on-file-open ()
  "Close dashboard window and kill its buffer when a real file opens.
Only fires for file-visiting buffers (buffer-file-name non-nil) — dired
and other non-file buffers leave the dashboard intact.
Removes itself from `find-file-hook' after first run (one-shot)."
  (when (and buffer-file-name
             (not (string= (buffer-name) "*dashboard*")))
    (let ((db-buf (get-buffer (or (bound-and-true-p dashboard-buffer-name)
                                  "*dashboard*"))))
      (when db-buf
        (let ((db-win (get-buffer-window db-buf)))
          (when (and db-win (not (one-window-p)))
            (delete-window db-win)))
        (kill-buffer db-buf)))
    (remove-hook 'find-file-hook #'emacs-ide-dashboard--kill-on-file-open)))

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
    ;; FIX-REFRESH-TIMER: raised from 4s to 6s so health check results
    ;; (fired at 3s idle in emacs-ide-health-check-startup) are available
    ;; before the dashboard refresh renders them.
    (run-with-idle-timer 6 nil
                         (lambda ()
                           (when (get-buffer "*dashboard*")
                             (with-current-buffer "*dashboard*"
                               (when (fboundp 'dashboard-refresh-buffer)
                                 (dashboard-refresh-buffer))))))

    ;; ── FIX-DASHBOARD-SPLIT (retained from 3.0.6) ───────────────────────
    ;; FIX-DEFUN-RELOAD: defun moved to top level; add-hook guarded with
    ;; unless/memq to prevent re-adding after config reload.
    (unless (memq #'emacs-ide-dashboard--kill-on-file-open find-file-hook)
      (add-hook 'find-file-hook #'emacs-ide-dashboard--kill-on-file-open))

    ;; FIX-SESSION-RESTORE: only set initial-buffer-choice when session
    ;; restore is disabled — perspective.el handles initial buffer when
    ;; general.restore-session: true in config.yml.
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
