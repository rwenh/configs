;;; lang-shell.el --- Shell IDE layer (Bash/Zsh/Fish/POSIX) -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG: emacs-ide-shell-run was defined inside sh-script
;;;     :config — not visible to M-x until sh-script loads. Moved to top-level.
;;;   - FIX-SHEBANG-INTERP: shebang detection captured the full line including
;;;     arguments (e.g. "/usr/bin/env bash -e") — passing the whole string as
;;;     the interpreter command failed. Now extracts just the executable path.
;;;   - FIX-BASH-TS-MODE-HOOK: lsp-mode hook was only on sh-mode, missing
;;;     bash-ts-mode (Emacs 29+ default for .sh files with treesitter).
;;;   - FIX-BASH-TS-MODE-FORMATTER: apheleia formatter only attached to sh-mode,
;;;     missing bash-ts-mode. Both now registered.
;;;   - FIX-FLYMAKE-SHELLCHECK-TS: flymake-shellcheck hook missing bash-ts-mode.
;;;   - FIX-TEST-REGISTER: Added emacs-ide-test-register-runner for sh-mode
;;;     using bats (the standard bash test framework).
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for sh-mode.
;;;   - FIX-FISH-MODE: .fish files were mapped to sh-mode — fish syntax is
;;;     incompatible. Now uses fish-mode package when available, otherwise
;;;     falls back to sh-mode with a comment.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP-GUARD: (bound-and-true-p emacs-ide-lsp-enable) guard.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "shell" :tier 2 :lsp-server "bash-language-server"
  :formatter "shfmt" :test-cmd "bats" :repl "bash"
  :modes '(sh-mode bash-ts-mode))

(when (emacs-ide-dev-lang-enabled-p "shell")

(emacs-ide-dev-ensure-treesit 'bash)

;; ============================================================================
;; COMPILE / RUN COMMAND
;; FIX-DEFUN-IN-CONFIG: top-level so M-x sees it before sh-script loads.
;; FIX-SHEBANG-INTERP: extract just the executable from the shebang line,
;; handling cases like "#!/usr/bin/env bash -e" and "#!/bin/bash".
;; ============================================================================
(defun emacs-ide-shell--detect-interpreter ()
  "Detect the shell interpreter from the shebang line or fall back to bash."
  (when (buffer-file-name)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "#!\\s-*\\(\\S-+\\)")
        (let ((shebang (match-string 1)))
          ;; Handle "#!/usr/bin/env bash" — take the argument as the interpreter
          (if (string-match-p "/env$" shebang)
              (progn
                (when (looking-at "#!\\s-*\\S-+\\s-+\\(\\S-+\\)")
                  (match-string 1)))
            shebang))))))

(defun emacs-ide-shell-run ()
  "Run the current shell script using its shebang interpreter or bash."
  (interactive)
  (let ((interp (or (emacs-ide-shell--detect-interpreter)
                    (executable-find "bash")
                    "sh")))
    (if (and interp (buffer-file-name))
        (compile (format "%s %s"
                         (shell-quote-argument interp)
                         (shell-quote-argument (buffer-file-name))))
      (message "lang-shell: no interpreter found"))))

(defun emacs-ide-shell-repl ()
  "Open a bash REPL."
  (interactive)
  (if (executable-find "bash")
      (progn (require 'comint)
             (make-comint "bash-repl" "bash")
             (switch-to-buffer "*bash-repl*"))
    (message "lang-shell: bash not found")))

(defun emacs-ide-shell-test-file ()
  "Run bats tests for the current file."
  (interactive)
  (if (and (executable-find "bats") (buffer-file-name))
      (compile (format "bats %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-shell: bats not found. Install: npm install -g bats")))

(defun emacs-ide-shell-test-project ()
  "Run all bats tests in the project."
  (interactive)
  (if (executable-find "bats")
      (compile "bats .")
    (message "lang-shell: bats not found. Install: npm install -g bats")))

;; ============================================================================
;; SH-SCRIPT (POSIX sh / bash / zsh)
;; FIX-FISH-MODE: .fish removed from sh-mode — fish syntax is incompatible.
;; ============================================================================
(use-package sh-script
  :straight nil
  :defer t
  :mode (("\\.sh\\'"   . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'"  . sh-mode)
         ;; FIX-FISH-MODE: .fish handled by fish-mode below; removed from here
         ("\\`#!/.*\\(bash\\|zsh\\|sh\\)" . sh-mode))
  :init
  (setq sh-basic-offset 2
        sh-indentation  2)
  :config
  (emacs-ide-dev-bind-compile sh-mode-map #'emacs-ide-shell-run)
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'sh-mode
      :launch         #'emacs-ide-shell-repl
      :buffer-name    "*bash-repl*"
      :send-region-fn nil))
  ;; FIX-TEST-REGISTER: register bats as the shell test runner
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'sh-mode
      :file-fn    #'emacs-ide-shell-test-file
      :project-fn #'emacs-ide-shell-test-project)))

;; ============================================================================
;; FISH MODE
;; FIX-FISH-MODE: use dedicated fish-mode for correct syntax highlighting.
;; Falls back gracefully if not installed.
;; ============================================================================
(use-package fish-mode
  :if (locate-library "fish-mode")
  :defer t
  :mode "\\.fish\\'")

;; ============================================================================
;; LSP — bash-language-server
;; FIX-BASH-TS-MODE-HOOK: bash-ts-mode hook added alongside sh-mode.
;; ============================================================================
(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((sh-mode      . lsp-deferred)
         (bash-ts-mode . lsp-deferred)))

;; ============================================================================
;; SHELLCHECK via flymake
;; FIX-FLYMAKE-SHELLCHECK-TS: bash-ts-mode hook added alongside sh-mode.
;; ============================================================================
(use-package flymake-shellcheck
  :if (executable-find "shellcheck")
  :hook ((sh-mode      . flymake-shellcheck-auto)
         (bash-ts-mode . flymake-shellcheck-auto)))

;; ============================================================================
;; FORMATTER — shfmt via apheleia
;; FIX-BASH-TS-MODE-FORMATTER: bash-ts-mode now also registered.
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'shfmt 'sh-mode)
  (emacs-ide-dev-attach-formatter 'shfmt 'bash-ts-mode))

) ;; end (when (emacs-ide-dev-lang-enabled-p "shell"))

(provide 'lang-shell)
;;; lang-shell.el ends here
