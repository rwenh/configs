;;; tools-terminal.el --- Terminal Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; VTerm, Eshell, and terminal utilities
;;; Code:

;; ============================================================================
;; VTERM - BEST TERMINAL EMULATOR
;; ============================================================================
(use-package vterm
  :bind (("C-c t" . emacs-ide-vterm-here)
         ("C-c T" . vterm-other-window)
         ("C-c M-t" . emacs-ide-vterm-project))
  :init
  (setq vterm-max-scrollback 100000
        vterm-shell (or (getenv "SHELL") "/bin/bash")
        vterm-kill-buffer-on-exit t
        vterm-term-environment-variable "xterm-256color"
        vterm-timer-delay 0.01
        vterm-clear-scrollback-when-clearing t)
  :config
  ;; Make sure vterm can find programs
  (add-to-list 'vterm-eval-cmds
               '("update-pwd" (lambda (path) (setq default-directory path)))))

;; ============================================================================
;; MULTI-VTERM - MULTIPLE TERMINALS
;; ============================================================================
(use-package multi-vterm
  :after vterm
  :bind (("C-c M-t" . multi-vterm)
         ("C-c M-n" . multi-vterm-next)
         ("C-c M-p" . multi-vterm-prev)
         ("C-c M-r" . multi-vterm-rename-buffer))
  :init
  (setq multi-vterm-dedicated-window-height-percent 30))

;; ============================================================================
;; VTERM UTILITY FUNCTIONS
;; ============================================================================
(defun emacs-ide-vterm-here ()
  "Open vterm in current directory."
  (interactive)
  (let ((default-directory (or (when (buffer-file-name)
                                  (file-name-directory (buffer-file-name)))
                               default-directory)))
    (if (get-buffer-window "*vterm*")
        (select-window (get-buffer-window "*vterm*"))
      (vterm))))

(defun emacs-ide-vterm-project ()
  "Open vterm in project root."
  (interactive)
  (let ((default-directory (or (projectile-project-root)
                               default-directory)))
    (vterm)))

(defun emacs-ide-vterm-toggle ()
  "Toggle vterm visibility."
  (interactive)
  (if-let ((vterm-window (get-buffer-window "*vterm*")))
      (delete-window vterm-window)
    (vterm)))

(defun emacs-ide-vterm-send-string (string)
  "Send STRING to vterm."
  (let ((vterm-buffer (get-buffer "*vterm*")))
    (if vterm-buffer
        (with-current-buffer vterm-buffer
          (vterm-send-string string)
          (vterm-send-return))
      (message "No vterm buffer found"))))

(defun emacs-ide-vterm-run-command (command)
  "Run COMMAND in vterm."
  (interactive "sCommand: ")
  (emacs-ide-vterm-here)
  (emacs-ide-vterm-send-string command))

(defun emacs-ide-vterm-run-file ()
  "Run current file in vterm."
  (interactive)
  (when buffer-file-name
    (let* ((file (buffer-file-name))
           (ext (file-name-extension file))
           (command (pcase ext
                     ("py" (format "python3 %s" file))
                     ("js" (format "node %s" file))
                     ("go" (format "go run %s" file))
                     ("rs" "cargo run")
                     ("rb" (format "ruby %s" file))
                     ("sh" (format "bash %s" file))
                     (_ (format "./%s" file)))))
      (emacs-ide-vterm-run-command command))))

;; ============================================================================
;; ESHELL - EMACS SHELL
;; ============================================================================
(use-package eshell
  :ensure nil
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
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "tail")
  (add-to-list 'eshell-visual-commands "top")
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "less")
  (add-to-list 'eshell-visual-commands "more"))

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

(defun eshell/g (&rest args)
  "Git command shortcut."
  (apply 'eshell-exec-visual "git" args))

(defun eshell/l (&rest args)
  "Better ls."
  (apply 'eshell-exec-visual "ls" "-lah" args))

(defun eshell/ll (&rest args)
  "Long listing."
  (apply 'eshell-exec-visual "ls" "-lh" args))

;; ============================================================================
;; SHELL - TERM MODE
;; ============================================================================
(use-package shell
  :ensure nil
  :bind ("C-c s" . shell)
  :init
  (setq shell-file-name (or (getenv "SHELL") "/bin/bash")))

;; ============================================================================
;; TERM MODE IMPROVEMENTS
;; ============================================================================
(use-package term
  :ensure nil
  :config
  ;; Better keybindings in term-mode
  (defun emacs-ide-term-mode-hook ()
    "Term mode customization."
    (setq term-buffer-maximum-size 10000)
    (setq term-scroll-to-bottom-on-output t))
  
  (add-hook 'term-mode-hook 'emacs-ide-term-mode-hook))

;; ============================================================================
;; COMPILATION BUFFER IMPROVEMENTS
;; ============================================================================
(use-package compile
  :ensure nil
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
  :ensure nil
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
  "Run COMMAND in background terminal."
  (interactive "sCommand: ")
  (start-process-shell-command "emacs-ide-bg" nil command)
  (message "Running in background: %s" command))

(defun emacs-ide-terminal-run-and-show (command)
  "Run COMMAND and show output."
  (interactive "sCommand: ")
  (async-shell-command command "*Terminal Output*"))

(defun emacs-ide-terminal-cd-to-buffer-dir ()
  "CD to current buffer directory in terminal."
  (interactive)
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (emacs-ide-vterm-send-string (format "cd %s" dir)))))

(defun emacs-ide-terminal-compile-file ()
  "Compile current file intelligently."
  (interactive)
  (when buffer-file-name
    (let* ((file (buffer-file-name))
           (ext (file-name-extension file))
           (base (file-name-sans-extension file))
           (command (pcase ext
                     ("c" (format "gcc -Wall -O2 -o %s %s && ./%s" base file base))
                     ("cpp" (format "g++ -Wall -O2 -o %s %s && ./%s" base file base))
                     ("rs" "cargo build && cargo run")
                     ("go" (format "go build %s && ./%s" file base))
                     ("java" (format "javac %s && java %s" file (file-name-base file)))
                     (_ nil))))
      (if command
          (emacs-ide-vterm-run-command command)
        (message "No compile command for .%s files" ext)))))

;; ============================================================================
;; DOCKER INTEGRATION
;; ============================================================================
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :mode "docker-compose.*\\.ya?ml\\'")

(use-package docker
  :bind ("C-c D" . docker))

(defun emacs-ide-docker-build ()
  "Build docker image in current directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (emacs-ide-vterm-run-command "docker build -t myapp .")))

(defun emacs-ide-docker-run ()
  "Run docker container."
  (interactive)
  (emacs-ide-vterm-run-command "docker run -it myapp"))

(defun emacs-ide-docker-compose-up ()
  "Docker compose up."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (emacs-ide-vterm-run-command "docker-compose up")))

;; ============================================================================
;; KUBERNETES INTEGRATION
;; ============================================================================
(use-package kubernetes
  :commands (kubernetes-overview)
  :bind ("C-c k" . kubernetes-overview))

;; ============================================================================
;; TERMINAL KEYBINDINGS
;; ============================================================================
(global-set-key (kbd "C-c t t") 'emacs-ide-vterm-here)
(global-set-key (kbd "C-c t p") 'emacs-ide-vterm-project)
(global-set-key (kbd "C-c t r") 'emacs-ide-vterm-run-file)
(global-set-key (kbd "C-c t c") 'emacs-ide-terminal-compile-file)
(global-set-key (kbd "C-c t d") 'emacs-ide-terminal-cd-to-buffer-dir)
(global-set-key (kbd "C-c t b") 'emacs-ide-terminal-run-in-background)
(global-set-key (kbd "C-c t s") 'emacs-ide-terminal-run-and-show)

(provide 'tools-terminal)
;;; tools-terminal.el ends here
