;;; tools-terminal.el --- Terminal Integration (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; VTerm, Eshell, and terminal utilities with proper validation
;;; Code:

;; ============================================================================
;; VTERM - BEST TERMINAL EMULATOR
;; ============================================================================
(use-package vterm
  :bind (("C-c t" . emacs-ide-vterm-here)
         ("C-c T" . vterm-other-window))
  :init
  ;; Detect shell safely
  (defun emacs-ide-detect-shell ()
    "Detect available shell with fallback."
    (let ((shell (getenv "SHELL")))
      (cond
       ((and shell (file-executable-p shell)) shell)
       ((file-executable-p "/bin/bash") "/bin/bash")
       ((file-executable-p "/bin/sh") "/bin/sh")
       ((file-executable-p "/bin/zsh") "/bin/zsh")
       (t "sh"))))
  
  (setq vterm-max-scrollback 100000
        vterm-shell (emacs-ide-detect-shell)
        vterm-kill-buffer-on-exit t
        vterm-term-environment-variable "xterm-256color"
        vterm-timer-delay 0.01
        vterm-clear-scrollback-when-clearing t)
  :config
  ;; Make sure vterm can find programs
  (when (fboundp 'vterm-send-string)
    (add-to-list 'vterm-eval-cmds
                 '("update-pwd" (lambda (path) (setq default-directory path))))))

;; ============================================================================
;; MULTI-VTERM - MULTIPLE TERMINALS
;; ============================================================================
(use-package multi-vterm
  :after vterm
  :bind (("C-c M-t" . multi-vterm)
         ("C-c M-n" . multi-vterm-next)
         ("C-c M-p" . multi-vterm-prev))
  :init
  (setq multi-vterm-dedicated-window-height-percent 30))

;; ============================================================================
;; VTERM UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-vterm-here ()
  "Open vterm in current directory."
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory (or (when (buffer-file-name)
                                     (file-name-directory (buffer-file-name)))
                                  default-directory)))
        (if (get-buffer-window "*vterm*")
            (select-window (get-buffer-window "*vterm*"))
          (vterm)))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-project ()
  "Open vterm in project root."
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory)))
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-toggle ()
  "Toggle vterm visibility."
  (interactive)
  (if (fboundp 'vterm)
      (if-let ((vterm-window (get-buffer-window "*vterm*")))
          (delete-window vterm-window)
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-send-string (string)
  "Send STRING to vterm safely."
  (let ((vterm-buffer (get-buffer "*vterm*")))
    (if (and vterm-buffer (fboundp 'vterm-send-string))
        (with-current-buffer vterm-buffer
          (vterm-send-string string)
          (vterm-send-return))
      (message "⚠️  No vterm buffer found"))))

(defun emacs-ide-vterm-run-command (command)
  "Run COMMAND in vterm safely."
  (interactive "sCommand: ")
  (emacs-ide-vterm-here)
  (emacs-ide-vterm-send-string command))

(defun emacs-ide-vterm-run-file ()
  "Run current file in vterm with appropriate interpreter."
  (interactive)
  (when buffer-file-name
    (let* ((file (buffer-file-name))
           (ext (file-name-extension file))
           (interpreter (cond
                        ((string= ext "py") (and (executable-find "python3") "python3"))
                        ((string= ext "js") (and (executable-find "node") "node"))
                        ((string= ext "go") (and (executable-find "go") "go run"))
                        ((string= ext "rs") (and (executable-find "cargo") "cargo run"))
                        ((string= ext "rb") (and (executable-find "ruby") "ruby"))
                        ((string= ext "sh") (and (executable-find "bash") "bash"))
                        (t nil)))
           (command (when interpreter
                     (format "%s %s" interpreter file))))
      (if command
          (emacs-ide-vterm-run-command command)
        (message "⚠️  No interpreter found for .%s files" ext)))))

;; ============================================================================
;; ESHELL - EMACS SHELL
;; ============================================================================
(use-package eshell
  :straight nil
  :bind (("C-c e" . emacs-ide-eshell-here)
         ("C-c E" . eshell))
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-history-size 10000
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)
  :config
  ;; Visual commands
  (dolist (cmd '("ssh" "tail" "top" "htop" "less" "more"))
    (add-to-list 'eshell-visual-commands cmd)))

(defun emacs-ide-eshell-here ()
  "Open eshell in current directory."
  (interactive)
  (let ((default-directory (or (when (buffer-file-name)
                                 (file-name-directory (buffer-file-name)))
                              default-directory)))
    (eshell 'N)))

;; ============================================================================
;; ESHELL ALIASES & FUNCTIONS
;; ============================================================================
(defun eshell/clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/e (file)
  "Open FILE in Emacs."
  (find-file file))

(defun eshell/d (&rest args)
  "Dired current directory."
  (dired "."))

;; ============================================================================
;; SHELL - TERM MODE
;; ============================================================================
(use-package shell
  :straight nil
  :bind ("C-c s" . shell)
  :init
  (setq shell-file-name (emacs-ide-detect-shell)))

;; ============================================================================
;; TERM MODE IMPROVEMENTS
;; ============================================================================
(use-package term
  :straight nil
  :config
  (defun emacs-ide-term-mode-hook ()
    "Term mode customization."
    (setq term-buffer-maximum-size 10000)
    (setq term-scroll-to-bottom-on-output t))
  
  (add-hook 'term-mode-hook 'emacs-ide-term-mode-hook))

;; ============================================================================
;; COMPILATION BUFFER IMPROVEMENTS
;; ============================================================================
(use-package compile
  :straight nil
  :init
  (setq compilation-scroll-output 'first-error
        compilation-window-height 20
        compilation-ask-about-save nil
        compilation-always-kill t
        compilation-skip-threshold 2)
  :config
  ;; ANSI color support
  (require 'ansi-color)
  (defun emacs-ide-colorize-compilation-buffer ()
    "Colorize compilation output."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  
  (add-hook 'compilation-filter-hook 'emacs-ide-colorize-compilation-buffer))

;; ============================================================================
;; COMINT MODE (SHELL BUFFERS)
;; ============================================================================
(use-package comint
  :straight nil
  :init
  (setq comint-prompt-read-only t
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-scroll-show-maximum-output t
        comint-input-ignoredups t
        comint-completion-addsuffix t
        comint-buffer-maximum-size 10000
        comint-move-point-for-output t))

;; ============================================================================
;; TERMINAL UTILITY COMMANDS
;; ============================================================================
(defun emacs-ide-terminal-run-in-background (command)
  "Run COMMAND in background terminal safely."
  (interactive "sCommand: ")
  (condition-case err
      (start-process-shell-command "emacs-ide-bg" nil command)
    (error (message "⚠️  Failed to run command: %s" (error-message-string err))))
  (message "Running in background: %s" command))

(defun emacs-ide-terminal-run-and-show (command)
  "Run COMMAND and show output safely."
  (interactive "sCommand: ")
  (condition-case err
      (async-shell-command command "*Terminal Output*")
    (error (message "⚠️  Failed to run command: %s" (error-message-string err)))))

(defun emacs-ide-terminal-compile-file ()
  "Compile current file intelligently with validation."
  (interactive)
  (when buffer-file-name
    (let* ((file (buffer-file-name))
           (ext (file-name-extension file))
           (base (file-name-sans-extension file))
           (command (cond
                    ((string= ext "c") 
                     (when (executable-find "gcc")
                       (format "gcc -Wall -O2 -o %s %s && ./%s" base file base)))
                    ((string= ext "cpp")
                     (when (executable-find "g++")
                       (format "g++ -Wall -O2 -o %s %s && ./%s" base file base)))
                    ((string= ext "rs")
                     (when (executable-find "cargo")
                       "cargo build && cargo run"))
                    ((string= ext "go")
                     (when (executable-find "go")
                       (format "go build %s && ./%s" file base)))
                    ((string= ext "java")
                     (when (executable-find "javac")
                       (format "javac %s && java %s" file (file-name-base file))))
                    (t nil))))
      (if command
          (emacs-ide-vterm-run-command command)
        (message "⚠️  No compiler found for .%s files" ext)))))

;; ============================================================================
;; DOCKER INTEGRATION (WITH VALIDATION)
;; ============================================================================
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package docker
  :if (executable-find "docker")
  :bind ("C-c D" . docker))

(defun emacs-ide-docker-build ()
  "Build docker image (if docker exists)."
  (interactive)
  (if (executable-find "docker")
      (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory)))
        (emacs-ide-vterm-run-command "docker build -t myapp ."))
    (message "⚠️  Docker not found")))

(defun emacs-ide-docker-run ()
  "Run docker container (if docker exists)."
  (interactive)
  (if (executable-find "docker")
      (emacs-ide-vterm-run-command "docker run -it myapp")
    (message "⚠️  Docker not found")))

(defun emacs-ide-docker-compose-up ()
  "Docker compose up (if docker-compose exists)."
  (interactive)
  (if (executable-find "docker-compose")
      (let ((default-directory (or (and (fboundp 'projectile-project-root)
                                       (projectile-project-root))
                                  default-directory)))
        (emacs-ide-vterm-run-command "docker-compose up"))
    (message "⚠️  docker-compose not found")))

(provide 'tools-terminal)
;;; tools-terminal.el ends here