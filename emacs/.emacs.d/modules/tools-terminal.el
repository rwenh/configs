;;; tools-terminal.el --- Terminal Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; VTerm, Eshell, and terminal utilities.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 2.2.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.4 to 3.0.4.
;;;   - FIX-VTERM-CONFIG-READ: vterm-max-scrollback, vterm-kill-buffer-on-exit,
;;;     and vterm-timer-delay were hardcoded. Now read from config.yml
;;;     terminal.max-scrollback, terminal.kill-buffer-on-exit, and
;;;     terminal.timer-delay respectively, with the old values as defaults.
;;;   - FIX-VTERM-SHELL-CONFIG: vterm-shell called emacs-ide-detect-shell at
;;;     :init time, ignoring terminal.shell from config.yml. Shell is now
;;;     resolved via emacs-ide-terminal--resolve-shell which checks config
;;;     first and falls back to auto-detection.
;;;   - FIX-VTERM-HERE-EXISTING: emacs-ide-vterm-here checked only for a
;;;     buffer named exactly "*vterm*", missing "*vterm*<2>" etc. Simplified
;;;     to call (vterm) directly which handles buffer reuse internally.
;;;   - FIX-VTERM-PROJECT-ROOT: emacs-ide-vterm-project called
;;;     projectile-project-root without ignore-errors — crashes when not in
;;;     a project. Wrapped with ignore-errors.
;;;   - FIX-RUN-FILE-QUOTE: emacs-ide-vterm-run-file interpolated buffer
;;;     file name into a shell command without shell-quote-argument — paths
;;;     with spaces produced broken commands. Fixed.
;;;   - FIX-ESHELL-VISUAL-RELOAD: (add-to-list 'eshell-visual-commands cmd)
;;;     in :config accumulated duplicates on every config reload. Changed to
;;;     cl-pushnew with :test #'equal to make it idempotent.
;;;   - FIX-ESHELL-E-GUARD: (unless (fboundp 'eshell/e) ...) ran at top level
;;;     before eshell loaded — eshell/e is not defined until eshell loads so
;;;     fboundp always returned nil and the custom definition always won,
;;;     defeating the guard. Moved inside (with-eval-after-load 'eshell ...).
;;;   - FIX-DOCKER-IMAGE-NAME: emacs-ide-docker-build hardcoded "-t myapp".
;;;     Now uses the projectile project name when available, falling back to
;;;     the directory basename.
;;; Fixes vs 2.2.3 (retained):
;;;   - FIX-CCC-D: C-c D o (docker) — keybindings.el owns C-c D prefix.
;;; Fixes vs 2.2.2 (retained):
;;;   - M-22: eshell/e guard (now moved into with-eval-after-load 'eshell).
;;; Code:

;; ============================================================================
;; CONFIG HELPERS
;; ============================================================================
(defun emacs-ide-terminal--cfg (key default)
  "Read KEY from terminal section of config.yml, falling back to DEFAULT."
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'terminal key default)
    default))

;; ============================================================================
;; SHELL DETECTION
;; FIX-VTERM-SHELL-CONFIG: check config.yml terminal.shell first, then
;; fall back to $SHELL environment variable and binary detection.
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

(defun emacs-ide-terminal--resolve-shell ()
  "Return the shell to use for vterm.
FIX-VTERM-SHELL-CONFIG: reads terminal.shell from config.yml first.
Falls back to emacs-ide-detect-shell when config value is nil or empty."
  (let ((cfg-shell (emacs-ide-terminal--cfg 'shell nil)))
    (if (and cfg-shell
             (stringp cfg-shell)
             (not (string-empty-p cfg-shell))
             (file-executable-p cfg-shell))
        cfg-shell
      (emacs-ide-detect-shell))))

;; ============================================================================
;; VTERM
;; FIX-VTERM-CONFIG-READ: terminal.* config values now wired to vterm vars.
;; ============================================================================
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
  "Open vterm in the current buffer's directory.
FIX-VTERM-HERE-EXISTING: simplified to call (vterm) directly — vterm
handles buffer reuse internally and correctly finds existing vterm buffers
regardless of their exact name (*vterm*, *vterm*<2>, etc.)."
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory
              (or (when (buffer-file-name)
                    (file-name-directory (buffer-file-name)))
                  default-directory)))
        (vterm))
    (message "⚠️  vterm not available")))

(defun emacs-ide-vterm-project ()
  "Open vterm in the project root.
FIX-VTERM-PROJECT-ROOT: projectile-project-root wrapped with ignore-errors."
  (interactive)
  (if (fboundp 'vterm)
      (let ((default-directory
              (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
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
  "Run current buffer's file in vterm with appropriate interpreter.
FIX-RUN-FILE-QUOTE: file path now quoted with shell-quote-argument."
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
          ;; FIX-RUN-FILE-QUOTE: shell-quote-argument prevents space-in-path issues
          (emacs-ide-vterm-run-command
           (format "%s %s" interp (shell-quote-argument file)))
        (message "⚠️  No interpreter found for .%s files" ext)))))

;; ============================================================================
;; ESHELL
;; ============================================================================
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
  ;; FIX-ESHELL-VISUAL-RELOAD: cl-pushnew prevents duplicate entries on reload
  (dolist (cmd '("ssh" "tail" "top" "htop" "less" "more"))
    (cl-pushnew cmd eshell-visual-commands :test #'equal)))

(defun emacs-ide-eshell-here ()
  "Open eshell in current buffer's directory."
  (interactive)
  (let ((default-directory
          (or (when (buffer-file-name)
                (file-name-directory (buffer-file-name)))
              default-directory)))
    (eshell 'N)))

;; FIX-ESHELL-E-GUARD: moved inside with-eval-after-load 'eshell so that
;; fboundp check runs AFTER eshell defines its own eshell/e. At top level
;; eshell/e is never bound yet — the guard was always nil and the custom
;; definition always won, defeating the intent.
(with-eval-after-load 'eshell
  (unless (fboundp 'eshell/e)
    (defun eshell/e (file)
      "Open FILE in Emacs from eshell."
      (find-file file))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t)) (erase-buffer)))

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
;; FIX (retained): --group-directories-first is GNU coreutils only.
;; ============================================================================
(use-package dired
  :straight nil
  :init
  (setq dired-listing-switches
        (if (eq system-type 'darwin)
            "-alGh"                           ; macOS BSD ls — no GNU-only flags
          "-alGh --group-directories-first")  ; GNU/Linux coreutils
        dired-dwim-target                    t
        dired-recursive-copies               'always
        dired-recursive-deletes              'always
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer             t)
  :bind (:map dired-mode-map
              ("h"     . dired-up-directory)
              ("l"     . dired-find-alternate-file)
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
  "Build docker image in project root.
FIX-DOCKER-IMAGE-NAME: uses project name instead of hardcoded 'myapp'."
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
  "Run docker-compose up in project root."
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
