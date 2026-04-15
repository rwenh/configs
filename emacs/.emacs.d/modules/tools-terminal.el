;;; tools-terminal.el --- Terminal Integration -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(defun emacs-ide-terminal--cfg (key default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'terminal key default)
    default))

(defun emacs-ide-detect-shell ()
  (let ((shell (getenv "SHELL")))
    (cond
     ((and shell (file-executable-p shell)) shell)
     ((file-executable-p "/bin/bash") "/bin/bash")
     ((file-executable-p "/bin/zsh")  "/bin/zsh")
     ((file-executable-p "/bin/sh")   "/bin/sh")
     (t "sh"))))

(defun emacs-ide-terminal--resolve-shell ()
  (let ((cfg-shell (emacs-ide-terminal--cfg 'shell nil)))
    (if (and cfg-shell
             (stringp cfg-shell)
             (not (string-empty-p cfg-shell))
             (file-executable-p cfg-shell))
        cfg-shell
      (emacs-ide-detect-shell))))

(use-package vterm
  :bind (("C-c t" . emacs-ide-vterm-here)
         ("C-c T" . vterm-other-window))
  :init
  (setq vterm-max-scrollback
        (emacs-ide-terminal--cfg 'max-scrollback 100000)
        vterm-shell
        (emacs-ide-terminal--resolve-shell)
        vterm-kill-buffer-on-exit
        (emacs-ide-terminal--cfg 'kill-buffer-on-exit t)
        vterm-term-environment-variable "xterm-256color"
        vterm-timer-delay
        (emacs-ide-terminal--cfg 'timer-delay 0.01)
        vterm-clear-scrollback-when-clearing t)
  :config
  (add-to-list 'vterm-eval-cmds
               '("update-pwd" (lambda (path)
                                (setq default-directory path)))))

(use-package multi-vterm
  :after vterm
  :bind (("C-c M-t" . multi-vterm)
         ("C-c M-n" . multi-vterm-next)
         ("C-c M-p" . multi-vterm-prev))
  :init
  (setq multi-vterm-dedicated-window-height-percent 30))

(defun emacs-ide-vterm-here ()
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory
              (or (when (buffer-file-name)
                    (file-name-directory (buffer-file-name)))
                  default-directory)))
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-project ()
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory
              (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  default-directory)))
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-toggle ()
  (interactive)
  (if (fboundp 'vterm)
      (if-let ((w (get-buffer-window "*vterm*")))
          (delete-window w)
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-send-string (string)
  (let ((buf (get-buffer "*vterm*")))
    (if (and buf (fboundp 'vterm-send-string))
        (with-current-buffer buf
          (vterm-send-string string)
          (vterm-send-return))
      (message "⚠️  No vterm buffer found"))))

(defun emacs-ide-vterm-run-command (command)
  (interactive "sCommand: ")
  (emacs-ide-vterm-here)
  (emacs-ide-vterm-send-string command))

(defun emacs-ide-vterm-run-file ()
  (interactive)
  (when buffer-file-name
    (let* ((file   (buffer-file-name))
           (ext    (file-name-extension file))
           (interp (cond
                    ((string= ext "py") (and (executable-find "python3") "python3"))
                    ((string= ext "js") (and (executable-find "node")    "node"))
                    ((string= ext "go") (and (executable-find "go")      "go run"))
                    ((string= ext "rs") (and (executable-find "cargo")   "cargo run"))
                    ((string= ext "rb") (and (executable-find "ruby")    "ruby"))
                    ((string= ext "sh") (and (executable-find "bash")    "bash"))
                    (t nil))))
      (if interp
          (emacs-ide-vterm-run-command
           (format "%s %s" interp (shell-quote-argument file)))
        (message "⚠️  No interpreter found for .%s files" ext)))))

(use-package eshell
  :straight nil
  :bind (("C-c e" . emacs-ide-eshell-here)
         ("C-c E" . eshell))
  :init
  (setq eshell-scroll-to-bottom-on-input       'all
        eshell-scroll-to-bottom-on-output      'all
        eshell-kill-processes-on-exit          t
        eshell-hist-ignoredups                 t
        eshell-history-size                    10000
        eshell-destroy-buffer-when-process-dies t)
  :config
  (dolist (cmd '("ssh" "tail" "top" "htop" "less" "more"))
    (cl-pushnew cmd eshell-visual-commands :test #'equal)))

(defun emacs-ide-eshell-here ()
  (interactive)
  (let ((default-directory
          (or (when (buffer-file-name)
                (file-name-directory (buffer-file-name)))
              default-directory)))
    (eshell 'N)))

(with-eval-after-load 'eshell
  (unless (fboundp 'eshell/e)
    (defun eshell/e (file)
      (find-file file))))

(defun eshell/clear ()
  (let ((inhibit-read-only t)) (erase-buffer)))

(unless (memq 'emacs-ide-terminal--ansi-compile compilation-filter-hook)
  (defun emacs-ide-terminal--ansi-compile ()
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'emacs-ide-terminal--ansi-compile))

(use-package compile
  :straight nil
  :init
  (setq compilation-scroll-output  'first-error
        compilation-window-height  20
        compilation-ask-about-save nil
        compilation-always-kill    t
        compilation-skip-threshold 2))

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

(use-package dired
  :straight nil
  :init
  (setq dired-listing-switches
        (if (eq system-type 'darwin)
            "-alGh"
          "-alGh --group-directories-first")
        dired-dwim-target                    t
        dired-recursive-copies               'always
        dired-recursive-deletes              'always
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer             t)
  :bind (:map dired-mode-map
              ("h"     . dired-up-directory)
              ("l"     . dired-find-alternate-file)
              ("C-c C-p" . wdired-change-to-wdired-mode)))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package docker
  :if (executable-find "docker")
  :bind ("C-c D o" . docker))

(defun emacs-ide-docker-build ()
  (interactive)
  (if (executable-find "docker")
      (let* ((root (or (and (fboundp 'projectile-project-root)
                            (ignore-errors (projectile-project-root)))
                       default-directory))
             (name (or (and (fboundp 'projectile-project-name)
                            (ignore-errors (projectile-project-name)))
                       (file-name-nondirectory
                        (directory-file-name root))))
             (default-directory root))
        (emacs-ide-vterm-run-command
         (format "docker build -t %s ." (shell-quote-argument name))))
    (message "⚠️  Docker not found")))

(defun emacs-ide-docker-compose-up ()
  (interactive)
  (if (executable-find "docker-compose")
      (let ((default-directory
              (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  default-directory)))
        (emacs-ide-vterm-run-command "docker-compose up"))
    (message "⚠️  docker-compose not found")))

(provide 'tools-terminal)
;;; tools-terminal.el ends here
