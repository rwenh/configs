;;; lang-data.el --- Data Science IDE layer (R / Julia / Notebooks) -*- lexical-binding: t -*-
;;; Version: 1.0.1
;;; Fixes vs 1.0.0:
;;;   - FIX-LSP-GUARD: ess-r-mode and julia-mode lsp-deferred hooks were
;;;     unguarded — fired even when emacs-ide-lsp-enable is nil in config.yml.
;;;     Added (bound-and-true-p emacs-ide-lsp-enable) :if guard to both.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "r" :tier 2 :lsp-server "r-languageserver"
  :formatter "styler" :test-cmd "Rscript" :repl "R" :modes '(ess-r-mode))
(emacs-ide-dev-register "julia" :tier 2 :lsp-server "julia-lsp"
  :formatter "JuliaFormatter" :test-cmd "julia" :repl "julia" :modes '(julia-mode))

(when (or (emacs-ide-dev-lang-enabled-p "r") (emacs-ide-dev-lang-enabled-p "julia"))

;; ── R via ESS ─────────────────────────────────────────────────────────────
(use-package ess
  :if (and (emacs-ide-dev-lang-enabled-p "r") (executable-find "R"))
  :defer t :mode (("\\.R\\'" . ess-r-mode) ("\\.r\\'" . ess-r-mode))
  :init (setq ess-use-flymake nil  ; LSP owns diagnostics
              ess-r-flymake-linters '()
              ess-indent-level 2
              ess-fancy-comments nil)
  :config
  (defun emacs-ide-r-run ()
    (interactive)
    (if (executable-find "Rscript")
        (compile (format "Rscript %s" (shell-quote-argument (buffer-file-name))))
      (message "lang-data: Rscript not found")))
  (emacs-ide-dev-bind-compile ess-r-mode-map #'emacs-ide-r-run))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (emacs-ide-dev-lang-enabled-p "r"))
  :hook (ess-r-mode . lsp-deferred))

;; ── Julia ─────────────────────────────────────────────────────────────────
(use-package julia-mode
  :if (and (emacs-ide-dev-lang-enabled-p "julia") (executable-find "julia"))
  :defer t :mode "\\.jl\\'"
  :config
  (defun emacs-ide-julia-run ()
    (interactive)
    (if (executable-find "julia")
        (compile (format "julia %s" (shell-quote-argument (buffer-file-name))))
      (message "lang-data: julia not found")))
  (emacs-ide-dev-bind-compile julia-mode-map #'emacs-ide-julia-run))

(use-package julia-repl
  :if (and (emacs-ide-dev-lang-enabled-p "julia") (executable-find "julia"))
  :after julia-mode
  :hook (julia-mode . julia-repl-mode)
  :bind (:map julia-mode-map ("C-c r" . julia-repl)))

(use-package lsp-julia
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (emacs-ide-dev-lang-enabled-p "julia"))
  :after julia-mode :hook (julia-mode . lsp-deferred))

;; ── Jupyter notebooks ─────────────────────────────────────────────────────
(use-package jupyter
  :if (and (executable-find "jupyter")
           (emacs-ide-dev-lang-enabled-p "python-jupyter"))
  :defer t :commands (jupyter-run-repl jupyter-connect-repl jupyter-org-interaction-mode))

) (provide 'lang-data)
;;; lang-data.el ends here
