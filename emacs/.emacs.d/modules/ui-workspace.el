;;; ui-workspace.el --- Named Workspaces via perspective.el + tab-bar -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

;;;; ── C-c W prefix map ────────────────────────────────────────────────────────

(define-prefix-command 'emacs-ide-workspace-map)
(global-set-key (kbd "C-c W") 'emacs-ide-workspace-map)

;;;; ── perspective.el ─────────────────────────────────────────────────────────

(use-package perspective
  :demand t
  :bind (("C-c W s" . persp-switch)
         ("C-c W n" . persp-new)
         ("C-c W k" . persp-kill)
         ("C-c W r" . persp-rename)
         ("C-c W b" . persp-switch-to-buffer)
         ("C-c W i" . persp-ibuffer)
         ("C-c W m" . persp-merge)
         ("C-c W u" . persp-unmerge)
         ("C-c W l" . persp-state-load)
         ("C-c W w" . persp-state-save))
  :init
  (setq persp-initial-frame-name    "main"
        persp-state-default-file
        (expand-file-name
         (or (and (fboundp 'emacs-ide-config-get)
                  (emacs-ide-config-get 'workspace 'state-file nil))
             "var/persp-state")
         user-emacs-directory)
        persp-suppress-no-prefix-key-warning t
        persp-show-modestring                t
        persp-modestring-short               t
        persp-sort                           'created)
  :config
  (persp-mode 1))

;;;; ── Tab-bar perspective labels ─────────────────────────────────────────────

(with-eval-after-load 'perspective
  (defun emacs-ide-workspace--tab-bar-perspectives ()
    "Return a propertized string of workspace names for the tab-bar."
    (when (fboundp 'persp-names)
      (let ((current (when (fboundp 'persp-current-name)
                       (persp-current-name))))
        (mapconcat
         (lambda (name)
           (propertize (format " %s " name)
                       'face (if (string= name current)
                                 'tab-bar-tab
                               'tab-bar-tab-inactive)))
         (persp-names) " "))))

  (setq tab-bar-format
        '(tab-bar-format-tabs
          tab-bar-separator
          (:eval (emacs-ide-workspace--tab-bar-perspectives))
          tab-bar-format-align-right
          tab-bar-format-global)))

;;;; ── Default workspace setup ────────────────────────────────────────────────

(defun emacs-ide-workspace-setup-defaults ()
  "Create default workspaces from config.yml workspace.defaults.
Run once after init via `after-init-hook'."
  (when (fboundp 'persp-switch)
    (let* ((cfg-defaults
            (and (fboundp 'emacs-ide-config-get)
                 (emacs-ide-config-get 'workspace 'defaults nil)))
           ;; workspace.defaults is now a properly parsed list (Session 1 #1A fix)
           (defaults
            (if (and (listp cfg-defaults) cfg-defaults)
                (mapcar (lambda (d)
                          (if (symbolp d) (symbol-name d) d))
                        cfg-defaults)
              '("main" "debug" "scratch"))))
      ;; Create each configured workspace if it doesn't exist
      (dolist (ws defaults)
        (unless (and (fboundp 'persp-get-by-name)
                     (persp-get-by-name ws))
          (persp-new ws)))
      ;; Always ensure debug and scratch exist as fallback workspaces
      (dolist (ws '("debug" "scratch"))
        (unless (and (fboundp 'persp-get-by-name)
                     (persp-get-by-name ws))
          (persp-new ws)))
      ;; Switch to main unless restore-session is active
      (let ((restore (and (fboundp 'emacs-ide-config-get)
                          (emacs-ide-config-get 'general 'restore-session nil))))
        (unless restore
          (persp-switch "main"))))))

(add-hook 'after-init-hook #'emacs-ide-workspace-setup-defaults)

;;;; ── Burly (window layout bookmarks) ────────────────────────────────────────

(use-package burly
  :defer t
  :bind (("C-c W S" . burly-bookmark-windows)
         ("C-c W R" . burly-open-bookmark)))

;;;; ── Auto-switch workspace on project change ─────────────────────────────────

(defvar emacs-ide-workspace-auto-switch t
  "When non-nil, switching Projectile projects creates/switches a workspace.")

(defun emacs-ide-workspace--apply-auto-switch-config ()
  "Read workspace.auto-switch from config and update the variable."
  (setq emacs-ide-workspace-auto-switch
        (if (fboundp 'emacs-ide-config-get)
            (emacs-ide-config-get 'workspace 'auto-switch t)
          t)))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-workspace--apply-auto-switch-config))

(add-hook 'emacs-ide-config-reload-hook
          #'emacs-ide-workspace--apply-auto-switch-config)

(defun emacs-ide-workspace-on-project-switch ()
  "Switch to (or create) a workspace named after the Projectile project."
  (when (and emacs-ide-workspace-auto-switch
             (fboundp 'persp-switch)
             (fboundp 'projectile-project-name))
    (let ((proj (projectile-project-name)))
      (when (and proj (not (string= proj "-")))
        (persp-switch proj)))))

(add-hook 'projectile-after-switch-project-hook
          #'emacs-ide-workspace-on-project-switch)

;;;; ── Consult workspace buffer source ────────────────────────────────────────

(with-eval-after-load 'consult
  (with-eval-after-load 'perspective
    (defvar emacs-ide-workspace--consult-source
      `(:name     "Workspace buffers"
        :narrow   ?w
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :default  t
        :items    ,(lambda ()
                     (when (fboundp 'persp-current-buffers)
                       (mapcar #'buffer-name
                               (persp-current-buffers)))))
      "Consult source listing buffers in the current perspective workspace.")
    (unless (member 'emacs-ide-workspace--consult-source
                    consult-buffer-sources)
      (add-to-list 'consult-buffer-sources
                   'emacs-ide-workspace--consult-source))))

;;;; ── doom-modeline workspace segment ────────────────────────────────────────

(with-eval-after-load 'doom-modeline
  (with-eval-after-load 'perspective
    (doom-modeline-def-segment emacs-ide-workspace
      "Current workspace name shown in the modeline."
      (when (and (eq (selected-window) (get-buffer-window))
                 (fboundp 'persp-current-name))
        (propertize (format " [%s]" (persp-current-name))
                    'face      'doom-modeline-buffer-major-mode
                    'help-echo "Current workspace — click C-c W s to switch")))))

;;;; ── Save on exit ────────────────────────────────────────────────────────────

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (fboundp 'persp-state-save)
                       (bound-and-true-p persp-mode)
                       (or (not (fboundp 'emacs-ide-config-get))
                           (emacs-ide-config-get 'workspace 'save-on-exit t)))
              (ignore-errors (persp-state-save)))))

;;;; ── Switch by index ─────────────────────────────────────────────────────────

(defun emacs-ide-workspace-switch-by-index (n)
  "Switch to the N-th workspace (1-based).
Displays an error if fewer than N workspaces exist."
  (interactive "nWorkspace index: ")
  (if (not (bound-and-true-p persp-mode))
      (message "workspace: persp-mode is not active")
    (when (fboundp 'persp-names)
      (let ((names (persp-names)))
        (if (> n (length names))
            (message "workspace: only %d workspace%s exist"
                     (length names)
                     (if (= (length names) 1) "" "s"))
          (persp-switch (nth (1- n) names)))))))

(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key
     (kbd (format "C-c W %d" n))
     (lambda ()
       (interactive)
       (emacs-ide-workspace-switch-by-index n)))))

;;;; ── Status ──────────────────────────────────────────────────────────────────

(defun emacs-ide-workspace-status ()
  "Display a buffer listing all workspaces and their buffer counts."
  (interactive)
  (with-output-to-temp-buffer "*Workspace Status*"
    (princ "=== WORKSPACE STATUS ===\n\n")
    (princ (format "%-3s %-22s %s\n" "#" "Name" "Buffers"))
    (princ (make-string 42 ?─))
    (princ "\n")
    (if (not (fboundp 'persp-names))
        (princ "  persp-mode not active\n")
      (let ((current (when (fboundp 'persp-current-name)
                       (persp-current-name)))
            (idx 1))
        (dolist (name (persp-names))
          (let* ((active (string= name current))
                 (persp  (when (fboundp 'persp-get-by-name)
                           (persp-get-by-name name)))
                 (bufs   (if (and persp (fboundp 'persp-buffers))
                             (length (persp-buffers persp))
                           0)))
            (princ (format "%s%d  %-22s %d buffer%s\n"
                           (if active "→ " "  ")
                           idx
                           name
                           bufs
                           (if (= bufs 1) "" "s")))
            (cl-incf idx)))))
    (princ "\nSwitch: C-c W s  |  By index: C-c W 1..9\n")))

(provide 'ui-workspace)
;;; ui-workspace.el ends here
