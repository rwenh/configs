;;; tools-terminal.el --- Terminal Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; VTerm, Eshell, and terminal utilities.
;;; Version: 2.2.2
;;; Fixes:
;;;   - dired-listing-switches used --group-directories-first which is a
;;;     GNU coreutils-only flag; it errors on macOS/BSD ls. Now guarded
;;;     behind a system-type check, falling back to safe portable flags.
;;;   - emacs-ide-colorize-compilation-buffer NOT redefined here (canonical
;;;     definition lives in ui-core.el).
;;; Code:

;; ============================================================================
;; VTERM
;; ============================================================================
(defun emacs-ide-detect-shell ()
  "Detect available shell with fallback chain."
  (let ((shell (getenv "SHELL")))
    (cond
     ((and shell (file-executable-p shell)) shell)
     ((file-executable-p "/bin/bash") "/bin/bash")
     ((file-executable-p "/bin/zsh")  "/bin/zsh")
     ((file-executable-p "/bin/sh")   "/bin/sh")
     (t "sh"))))

(use-package vterm
  :bind (("C-c t" . emacs-ide-vterm-here)
         ("C-c T" . vterm-other-window))
  :init
  (setq vterm-max-scrollback            100000
        vterm-shell                     (emacs-ide-detect-shell)
        vterm-kill-buffer-on-exit       t
        vterm-term-environment-variable "xterm-256color"
        vterm-timer-delay               0.01
        vterm-clear-scrollback-when-clearing t)
  :config
  (add-to-list 'vterm-eval-cmds
               '("update-pwd" (lambda (path)
                                (setq default-directory path)))))

;; ============================================================================
;; MULTI-VTERM
;; ============================================================================
(use-package multi-vterm
  :after vterm
  :bind (("C-c M-t" . multi-vterm)
         ("C-c M-n" . multi-vterm-next)
         ("C-c M-p" . multi-vterm-prev))
  :init
  (setq multi-vterm-dedicated-window-height-percent 30))

;; ============================================================================
;; VTERM UTILITIES
;; ============================================================================
(defun emacs-ide-vterm-here ()
  "Open vterm in the current buffer's directory."
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory
              (or (when (buffer-file-name)
                    (file-name-directory (buffer-file-name)))
                  default-directory)))
        (if (get-buffer-window "*vterm*")
            (select-window (get-buffer-window "*vterm*"))
          (vterm)))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-project ()
  "Open vterm in the project root."
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory
              (or (and (fboundp 'projectile-project-root)
                       (projectile-project-root))
                  default-directory)))
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-toggle ()
  "Toggle vterm window visibility."
  (interactive)
  (if (fboundp 'vterm)
      (if-let ((w (get-buffer-window "*vterm*")))
          (delete-window w)
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-send-string (string)
  "Send STRING to the vterm buffer."
  (let ((buf (get-buffer "*vterm*")))
    (if (and buf (fboundp 'vterm-send-string))
        (with-current-buffer buf
          (vterm-send-string string)
          (vterm-send-return))
      (message "⚠️  No vterm buffer found"))))

(defun emacs-ide-vterm-run-command (command)
  "Open vterm and run COMMAND."
  (interactive "sCommand: ")
  (emacs-ide-vterm-here)
  (emacs-ide-vterm-send-string command))

(defun emacs-ide-vterm-run-file ()
  "Run current buffer's file in vterm with appropriate interpreter."
  (interactive)
  (when buffer-file-name
    (let* ((file   (buffer-file-name))
           (ext    (file-name-extension file))
           (interp (cond
                    ((string= ext "py") (and (executable-find "python3") "python3"))
                    ((string= ext "js") (and (executable-find "node") "node"))
                    ((string= ext "go") (and (executable-find "go") "go run"))
                    ((string= ext "rs") (and (executable-find "cargo") "cargo run"))
                    ((string= ext "rb") (and (executable-find "ruby") "ruby"))
                    ((string= ext "sh") (and (executable-find "bash") "bash"))
                    (t nil))))
      (if interp
          (emacs-ide-vterm-run-command (format "%s %s" interp file))
        (message "⚠️  No interpreter found for .%s files" ext)))))

;; ============================================================================
;; ESHELL
;; ============================================================================
(use-package eshell
  :straight nil
  :bind (("C-c e" . emacs-ide-eshell-here)
         ("C-c E" . eshell))
  :init
  (setq eshell-scroll-to-bottom-on-input      'all
        eshell-scroll-to-bottom-on-output     'all
        eshell-kill-processes-on-exit         t
        eshell-hist-ignoredups                t
        eshell-history-size                   10000
        eshell-destroy-buffer-when-process-dies t)
  :config
  (dolist (cmd '("ssh" "tail" "top" "htop" "less" "more"))
    (add-to-list 'eshell-visual-commands cmd)))

(defun emacs-ide-eshell-here ()
  "Open eshell in current buffer's directory."
  (interactive)
  (let ((default-directory
          (or (when (buffer-file-name)
                (file-name-directory (buffer-file-name)))
              default-directory)))
    (eshell 'N)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t)) (erase-buffer)))

(defun eshell/e (file)
  "Open FILE in Emacs from eshell."
  (find-file file))

;; ============================================================================
;; COMPILATION BUFFER
;; NOTE: emacs-ide-colorize-compilation-buffer is defined in ui-core.el only.
;;       Do NOT redefine it here.
;; ============================================================================
(use-package compile
  :straight nil
  :init
  (setq compilation-scroll-output  'first-error
        compilation-window-height  20
        compilation-ask-about-save nil
        compilation-always-kill    t
        compilation-skip-threshold 2))

;; ============================================================================
;; COMINT
;; ============================================================================
(use-package comint
  :straight nil
  :init
  (setq comint-prompt-read-only           t
        comint-scroll-to-bottom-on-input  t
        comint-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output t
        comint-input-ignoredups           t
        comint-completion-addsuffix       t
        comint-buffer-maximum-size        10000
        comint-move-point-for-output      t))

;; ============================================================================
;; DIRED
;; FIX: --group-directories-first is GNU coreutils only; errors on macOS/BSD.
;;      Guard behind system-type; macOS uses portable flags instead.
;; ============================================================================
(use-package dired
  :straight nil
  :init
  (setq dired-listing-switches
        (if (eq system-type 'darwin)
            "-alGh"                          ; macOS BSD ls — no GNU-only flags
          "-alGh --group-directories-first") ; GNU/Linux coreutils
        dired-dwim-target                   t
        dired-recursive-copies              'always
        dired-recursive-deletes             'always
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer            t)
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("l" . dired-find-alternate-file)
              ("C-c C-p" . wdired-change-to-wdired-mode)))

;; ============================================================================
;; DOCKER
;; ============================================================================
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package docker
  :if (executable-find "docker")
  ;; C-c D o: docker lives here; debug-core.el owns C-c D s/r/q
  :bind ("C-c D o" . docker))

(defun emacs-ide-docker-build ()
  "Build docker image in project root."
  (interactive)
  (if (executable-find "docker")
      (let ((default-directory
              (or (and (fboundp 'projectile-project-root)
                       (projectile-project-root))
                  default-directory)))
        (emacs-ide-vterm-run-command "docker build -t myapp ."))
    (message "⚠️  Docker not found")))

(defun emacs-ide-docker-compose-up ()
  "Run docker-compose up in project root."
  (interactive)
  (if (executable-find "docker-compose")
      (let ((default-directory
              (or (and (fboundp 'projectile-project-root)
                       (projectile-project-root))
                  default-directory)))
        (emacs-ide-vterm-run-command "docker-compose up"))
    (message "⚠️  docker-compose not found")))

(provide 'tools-terminal)
;;; tools-terminal.el ends here
