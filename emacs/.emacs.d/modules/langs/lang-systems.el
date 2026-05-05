;;; lang-systems.el --- Systems Languages IDE layer (Zig / Nix / D / V) -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "zig"
  :tier 3 :lsp-server "zls"
  :formatter "zig fmt" :test-cmd "zig test" :repl nil
  :modes '(zig-mode))

(emacs-ide-dev-register "nix"
  :tier 3 :lsp-server "nil"
  :formatter "nixpkgs-fmt" :test-cmd nil :repl "nix repl"
  :modes '(nix-mode))

(emacs-ide-dev-register "d"
  :tier 3 :lsp-server nil
  :formatter nil :test-cmd "dub test" :repl nil
  :modes '(d-mode))

(emacs-ide-dev-register "v"
  :tier 3 :lsp-server nil
  :formatter "v fmt" :test-cmd "v test" :repl nil
  :modes '(v-mode))

(when (or (emacs-ide-dev-lang-enabled-p "zig")
          (emacs-ide-dev-lang-enabled-p "nix")
          (emacs-ide-dev-lang-enabled-p "d")
          (emacs-ide-dev-lang-enabled-p "v"))

;;;; ── Zig run / test helpers ──────────────────────────────────────────────────

(defun emacs-ide-zig-build ()
  "Build the Zig project via zig build."
  (interactive)
  (if (executable-find "zig")
      (compile "zig build")
    (message "lang-systems: zig not found on PATH")))

(defun emacs-ide-zig-test-project ()
  "Run Zig project tests via zig build test."
  (interactive)
  (if (executable-find "zig")
      (compile "zig build test")
    (message "lang-systems: zig not found on PATH")))

(defun emacs-ide-zig-test-file ()
  "Run zig test on the current file."
  (interactive)
  (if (and (executable-find "zig") (buffer-file-name))
      (compile (format "zig test %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-systems: zig not found or no file")))

;;;; ── zig-mode ────────────────────────────────────────────────────────────────

(use-package zig-mode
  :if (and (emacs-ide-dev-lang-enabled-p "zig")
           (executable-find "zig"))
  :defer t
  :mode "\\.zig\\'"
  :init
  (setq zig-format-on-save nil)   ;; apheleia handles formatting
  :config
  (emacs-ide-dev-bind-compile zig-mode-map #'emacs-ide-zig-build)
  )

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "zig")
           (executable-find "zls"))
  :hook (zig-mode . lsp-deferred))

(with-eval-after-load 'apheleia
  (when (and (emacs-ide-dev-lang-enabled-p "zig")
             (executable-find "zig"))
    (emacs-ide-dev-attach-formatter 'zigfmt 'zig-mode)))

(with-eval-after-load 'dap-mode
  (when (emacs-ide-dev-lang-enabled-p "zig")
    (emacs-ide-dev-attach-dap "Zig :: LLDB" 'dap-lldb)))

(with-eval-after-load 'tools-test-runner-registry
  (when (and (fboundp 'emacs-ide-test-register-runner)
             (emacs-ide-dev-lang-enabled-p "zig"))
    (emacs-ide-test-register-runner 'zig-mode
      :file-fn    #'emacs-ide-zig-test-file
      :project-fn #'emacs-ide-zig-test-project)))

;;;; ── Nix ─────────────────────────────────────────────────────────────────────

(defun emacs-ide-nix-repl ()
  "Open a nix repl."
  (interactive)
  (if (executable-find "nix")
      (progn
        (require 'comint)
        (make-comint "nix-repl" "nix" nil "repl")
        (switch-to-buffer "*nix-repl*"))
    (message "lang-systems: nix not found on PATH")))

(use-package nix-mode
  :if (emacs-ide-dev-lang-enabled-p "nix")
  :defer t
  :mode "\\.nix\\'"
  :config
  (emacs-ide-dev-attach-repl nix-mode-map #'emacs-ide-nix-repl
                              (kbd "C-c x r"))
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'nix-mode
      :launch         #'emacs-ide-nix-repl
      :buffer-name    "*nix-repl*"
      :send-region-fn nil)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "nix")
           (executable-find "nil"))
  :hook (nix-mode . lsp-deferred))

(with-eval-after-load 'apheleia
  (when (and (emacs-ide-dev-lang-enabled-p "nix")
             (executable-find "nixpkgs-fmt"))
    (emacs-ide-dev-attach-formatter 'nixpkgs-fmt 'nix-mode)))

;;;; ── D ───────────────────────────────────────────────────────────────────────

(use-package d-mode
  :if (and (emacs-ide-dev-lang-enabled-p "d")
           (executable-find "dmd"))
  :defer t
  :mode "\\.d\\'")

;;;; ── V ───────────────────────────────────────────────────────────────────────

(use-package v-mode
  :if (and (emacs-ide-dev-lang-enabled-p "v")
           (executable-find "v"))
  :defer t
  :mode "\\.v\\'")

) ;; end systems-enabled

(provide 'lang-systems)
;;; lang-systems.el ends here
