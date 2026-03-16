;;; lang-systems.el --- Systems Languages IDE layer (Zig / Nix / D / V) -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "zig" :tier 3 :lsp-server "zls" :formatter "zig fmt"
  :test-cmd "zig test" :repl nil :modes '(zig-mode))
(emacs-ide-dev-register "nix" :tier 3 :lsp-server "nil" :formatter "nixpkgs-fmt"
  :test-cmd nil :repl "nix repl" :modes '(nix-mode))

(when (or (emacs-ide-dev-lang-enabled-p "zig")
          (emacs-ide-dev-lang-enabled-p "nix")
          (emacs-ide-dev-lang-enabled-p "d"))

;; ── Zig ───────────────────────────────────────────────────────────────────
(use-package zig-mode
  :if (and (emacs-ide-dev-lang-enabled-p "zig") (executable-find "zig"))
  :defer t :mode "\\.zig\\'"
  :init (setq zig-format-on-save t)
  :config
  (emacs-ide-dev-bind-compile zig-mode-map
    (lambda () (interactive) (compile "zig build"))))
(use-package lsp-mode :if (and (emacs-ide-dev-lang-enabled-p "zig") (executable-find "zls"))
  :hook (zig-mode . lsp-deferred))
(with-eval-after-load 'dap-mode
  (when (emacs-ide-dev-lang-enabled-p "zig")
    (emacs-ide-dev-attach-dap "Zig :: LLDB" 'dap-lldb)))

;; ── Nix ───────────────────────────────────────────────────────────────────
(use-package nix-mode
  :if (emacs-ide-dev-lang-enabled-p "nix")
  :defer t :mode "\\.nix\\'"
  :config
  (defun emacs-ide-nix-repl ()
    (interactive)
    (if (executable-find "nix") (progn (require 'comint) (make-comint "nix-repl" "nix" nil "repl") (switch-to-buffer "*nix-repl*"))
      (message "lang-systems: nix not found")))
  (emacs-ide-dev-attach-repl nix-mode-map #'emacs-ide-nix-repl))
(use-package lsp-mode :if (and (emacs-ide-dev-lang-enabled-p "nix") (executable-find "nil"))
  :hook (nix-mode . lsp-deferred))
(with-eval-after-load 'apheleia
  (when (and (emacs-ide-dev-lang-enabled-p "nix") (executable-find "nixpkgs-fmt"))
    (emacs-ide-dev-attach-formatter 'nixpkgs-fmt 'nix-mode)))

;; ── D ─────────────────────────────────────────────────────────────────────
(use-package d-mode
  :if (and (emacs-ide-dev-lang-enabled-p "d") (executable-find "dmd"))
  :defer t :mode "\\.d\\'")

;; ── V ─────────────────────────────────────────────────────────────────────
(use-package v-mode
  :if (and (emacs-ide-dev-lang-enabled-p "v") (executable-find "v"))
  :defer t :mode "\\.v\\'")

) (provide 'lang-systems)
;;; lang-systems.el ends here
