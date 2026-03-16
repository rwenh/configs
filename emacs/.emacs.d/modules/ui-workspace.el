;;; ui-workspace.el --- Named Workspaces via perspective.el + tab-bar -*- lexical-binding: t -*-
;;; Commentary:
;;; Workspace management: each workspace is a named perspective (isolated
;;; buffer list + window layout).  Tab-bar shows workspaces as tabs.
;;; Workspaces auto-created from config.yml environments section.
;;;
;;; Default workspaces: work · personal · debug · scratch
;;; Switch:   C-c W s   or   M-1..9 (tab-bar)
;;; Create:   C-c W n
;;; Kill:     C-c W k
;;; Rename:   C-c W r
;;; Save:     C-c W S   (burly bookmark)
;;; Restore:  C-c W R   (burly bookmark)
;;;
;;; Version: 1.0.0
;;; Code:

;; ============================================================================
;; PERSPECTIVE.EL — ISOLATED BUFFER NAMESPACES
;; ============================================================================

;; Define C-c W as a prefix BEFORE perspective loads so :bind entries work
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
        persp-state-default-file    (expand-file-name "var/persp-state" user-emacs-directory)
        persp-suppress-no-prefix-key-warning t
        persp-show-modestring        t
        persp-modestring-short       t
        persp-sort                   'created)
  :config
  (persp-mode 1))

;; ============================================================================
;; TAB-BAR INTEGRATION — workspaces as visual tabs
;; ============================================================================

(with-eval-after-load 'perspective
  ;; Show perspective name in tab-bar format
  (defun emacs-ide-workspace--tab-bar-perspectives ()
    "Format all perspective names for tab-bar display."
    (when (fboundp 'persp-names)
      (let ((current (when (fboundp 'persp-current-name) (persp-current-name))))
        (mapconcat
         (lambda (name)
           (let* ((active (string= name current))
                  (face   (if active 'tab-bar-tab 'tab-bar-tab-inactive)))
             (propertize (format " %s " name) 'face face)))
         (persp-names) " "))))

  ;; Override tab-bar format to show perspectives
  (setq tab-bar-format
        '(tab-bar-format-tabs
          tab-bar-separator
          (:eval (emacs-ide-workspace--tab-bar-perspectives))
          tab-bar-format-align-right
          tab-bar-format-global)))

;; ============================================================================
;; DEFAULT WORKSPACES — auto-created from config.yml environments
;; ============================================================================

(defun emacs-ide-workspace-setup-defaults ()
  "Create default workspaces based on config.yml environments section."
  (when (fboundp 'persp-switch)
    (let ((envs (condition-case nil
                    (when (boundp 'emacs-ide-config-data)
                      (mapcar #'car
                              (cdr (assoc "environments" emacs-ide-config-data))))
                  (error nil)))
          (defaults '("main" "debug" "scratch")))
      ;; Create environment workspaces from config
      (dolist (env (or envs defaults))
        (unless (and (fboundp 'persp-get-by-name)
                     (persp-get-by-name env))
          (persp-new env)))
      ;; Always ensure debug and scratch exist
      (dolist (ws '("debug" "scratch"))
        (unless (and (fboundp 'persp-get-by-name)
                     (persp-get-by-name ws))
          (persp-new ws)))
      ;; Return to main
      (persp-switch "main"))))

(add-hook 'after-init-hook #'emacs-ide-workspace-setup-defaults)

;; ============================================================================
;; BURLY — WINDOW LAYOUT SAVE/RESTORE PER WORKSPACE
;; ============================================================================

(use-package burly
  :defer t
  :bind (("C-c W S" . burly-bookmark-windows)
         ("C-c W R" . burly-open-bookmark)))

;; ============================================================================
;; PROJECT → WORKSPACE AUTO-ASSOCIATION
;; When switching Projectile projects, optionally switch to matching workspace
;; ============================================================================

(defvar emacs-ide-workspace-auto-switch t
  "When non-nil, switching projects creates/switches a matching workspace.")

(defun emacs-ide-workspace-on-project-switch ()
  "Auto-create or switch to a workspace named after the project."
  (when (and emacs-ide-workspace-auto-switch
             (fboundp 'persp-switch)
             (fboundp 'projectile-project-name))
    (let ((proj (projectile-project-name)))
      (when (and proj (not (string= proj "-")))
        (persp-switch proj)))))

(add-hook 'projectile-after-switch-project-hook
          #'emacs-ide-workspace-on-project-switch)

;; ============================================================================
;; WORKSPACE-AWARE BUFFER SWITCHING
;; consult-buffer scoped to current perspective
;; ============================================================================

(with-eval-after-load 'consult
  (with-eval-after-load 'perspective
    ;; Add perspective-filtered source to consult-buffer
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
    (add-to-list 'consult-buffer-sources
                 'emacs-ide-workspace--consult-source)))

;; ============================================================================
;; MODELINE — show workspace name
;; ============================================================================

(with-eval-after-load 'doom-modeline
  (with-eval-after-load 'perspective
    (doom-modeline-def-segment emacs-ide-workspace
      "Current workspace name."
      (when (and (doom-modeline--active)
                 (fboundp 'persp-current-name))
        (propertize (format " [%s]" (persp-current-name))
                    'face 'doom-modeline-buffer-major-mode
                    'help-echo "Current workspace")))))

;; ============================================================================
;; QUICK WORKSPACE SWITCHER (M-1 through M-9)
;; ============================================================================

(defun emacs-ide-workspace-switch-by-index (n)
  "Switch to the Nth workspace (1-indexed)."
  (when (fboundp 'persp-names)
    (let ((names (persp-names)))
      (if (> n (length names))
          (message "workspace: only %d workspaces exist" (length names))
        (persp-switch (nth (1- n) names))))))

(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key
     (kbd (format "M-%d" n))
     (lambda () (interactive) (emacs-ide-workspace-switch-by-index n)))))

;; ============================================================================
;; STATUS
;; ============================================================================

(defun emacs-ide-workspace-status ()
  "Show all workspaces and their buffer counts."
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
