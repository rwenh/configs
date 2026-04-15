;;; ui-workspace.el --- Named Workspaces via perspective.el + tab-bar -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(define-prefix-command 'emacs-ide-workspace-map)
(global-set-key (kbd "C-c W") 'emacs-ide-workspace-map)

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
        persp-show-modestring        t
        persp-modestring-short       t
        persp-sort                   'created)
  :config
  (persp-mode 1))

(with-eval-after-load 'perspective
  (defun emacs-ide-workspace--tab-bar-perspectives ()
    (when (fboundp 'persp-names)
      (let ((current (when (fboundp 'persp-current-name) (persp-current-name))))
        (mapconcat
         (lambda (name)
           (let* ((active (string= name current))
                  (face   (if active 'tab-bar-tab 'tab-bar-tab-inactive)))
             (propertize (format " %s " name) 'face face)))
         (persp-names) " "))))

  (setq tab-bar-format
        '(tab-bar-format-tabs
          tab-bar-separator
          (:eval (emacs-ide-workspace--tab-bar-perspectives))
          tab-bar-format-align-right
          tab-bar-format-global)))

(defun emacs-ide-workspace-setup-defaults ()
  (when (fboundp 'persp-switch)
    (let* ((envs (condition-case nil
                     (when (boundp 'emacs-ide-config-data)
                       (mapcar #'car
                               (cdr (assoc 'environments emacs-ide-config-data))))
                   (error nil)))
           (cfg-defaults (and (fboundp 'emacs-ide-config-get)
                              (emacs-ide-config-get 'workspace 'defaults nil)))
           (defaults (or (and (listp cfg-defaults) (mapcar (lambda (d)
                                                             (if (symbolp d)
                                                                 (symbol-name d)
                                                               d))
                                                           cfg-defaults))
                         '("main" "debug" "scratch"))))
      (dolist (env (or envs defaults))
        (let ((name (if (symbolp env) (symbol-name env) env)))
          (unless (and (fboundp 'persp-get-by-name)
                       (persp-get-by-name name))
            (persp-new name))))
      (dolist (ws '("debug" "scratch"))
        (unless (and (fboundp 'persp-get-by-name)
                     (persp-get-by-name ws))
          (persp-new ws)))
      (let ((restore (and (fboundp 'emacs-ide-config-get)
                          (emacs-ide-config-get 'general 'restore-session nil))))
        (unless restore
          (persp-switch "main"))))))

(add-hook 'after-init-hook #'emacs-ide-workspace-setup-defaults)

(use-package burly
  :defer t
  :bind (("C-c W S" . burly-bookmark-windows)
         ("C-c W R" . burly-open-bookmark)))

(defvar emacs-ide-workspace-auto-switch t
  "When non-nil, switching projects creates/switches a matching workspace.")

(defun emacs-ide-workspace--apply-auto-switch-config ()
  (setq emacs-ide-workspace-auto-switch
        (if (fboundp 'emacs-ide-config-get)
            (emacs-ide-config-get 'workspace 'auto-switch t)
          t)))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-workspace--apply-auto-switch-config))
(add-hook 'emacs-ide-config-reload-hook
          #'emacs-ide-workspace--apply-auto-switch-config)

(defun emacs-ide-workspace-on-project-switch ()
  (when (and emacs-ide-workspace-auto-switch
             (fboundp 'persp-switch)
             (fboundp 'projectile-project-name))
    (let ((proj (projectile-project-name)))
      (when (and proj (not (string= proj "-")))
        (persp-switch proj)))))

(add-hook 'projectile-after-switch-project-hook
          #'emacs-ide-workspace-on-project-switch)

(with-eval-after-load 'consult
  (with-eval-after-load 'perspective
    (defvar emacs-ide-workspace--consult-source
      `(:name     "Workspace buffers"
        :narrow   ?w
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :default  t
        :items    ,(lambda ()
                     (when (fboundp 'persp-current-buffers)
                       (mapcar #'buffer-name (persp-current-buffers)))))
      "Consult source for current workspace buffers.")
    (unless (member 'emacs-ide-workspace--consult-source consult-buffer-sources)
      (add-to-list 'consult-buffer-sources
                   'emacs-ide-workspace--consult-source))))

(with-eval-after-load 'doom-modeline
  (with-eval-after-load 'perspective
    (doom-modeline-def-segment emacs-ide-workspace
      "Current workspace name."
      (when (and (eq (selected-window) (get-buffer-window))
                 (fboundp 'persp-current-name))
        (propertize (format " [%s]" (persp-current-name))
                    'face 'doom-modeline-buffer-major-mode
                    'help-echo "Current workspace")))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (fboundp 'persp-state-save)
                       (bound-and-true-p persp-mode)
                       (or (not (fboundp 'emacs-ide-config-get))
                           (emacs-ide-config-get 'workspace 'save-on-exit t)))
              (ignore-errors (persp-state-save)))))

(defun emacs-ide-workspace-switch-by-index (n)
  (if (not (bound-and-true-p persp-mode))
      (message "workspace: persp-mode is not active")
    (when (fboundp 'persp-names)
      (let ((names (persp-names)))
        (if (> n (length names))
            (message "workspace: only %d workspaces exist" (length names))
          (persp-switch (nth (1- n) names)))))))

(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key
     (kbd (format "M-%d" n))
     (lambda () (interactive) (emacs-ide-workspace-switch-by-index n)))))

(defun emacs-ide-workspace-status ()
  (interactive)
  (with-output-to-temp-buffer "*Workspace Status*"
    (princ "=== WORKSPACE STATUS ===\n\n")
    (when (fboundp 'persp-names)
      (dolist (name (persp-names))
        (let* ((current  (string= name (when (fboundp 'persp-current-name)
                                         (persp-current-name))))
               (persp    (when (fboundp 'persp-get-by-name) (persp-get-by-name name)))
               (bufs     (when (fboundp 'persp-buffers) (length (persp-buffers persp)))))
          (princ (format "%s %-20s %d buffer%s\n"
                         (if current "→" " ")
                         name
                         (or bufs 0)
                         (if (= (or bufs 0) 1) "" "s"))))))))

(provide 'ui-workspace)
;;; ui-workspace.el ends here
