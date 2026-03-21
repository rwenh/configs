;;; lang-functional.el --- Functional Languages IDE layer -*- lexical-binding: t -*-
;;; Haskell · Clojure · Elixir · Erlang · OCaml
;;; Version: 1.0.1
;;; Fixes vs 1.0.0:
;;;   - FIX-LSP-GUARD: All four lsp-deferred hooks (haskell, clojure, elixir,
;;;     OCaml) were unguarded — fired even when emacs-ide-lsp-enable is nil.
;;;     Added (bound-and-true-p emacs-ide-lsp-enable) :if guard to each.
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "haskell" :tier 3 :lsp-server "haskell-language-server"
  :formatter "ormolu" :test-cmd "cabal test" :repl "ghci" :modes '(haskell-mode))
(emacs-ide-dev-register "clojure" :tier 3 :lsp-server "clojure-lsp"
  :formatter "cljfmt" :test-cmd "clj -M:test" :repl "cider" :modes '(clojure-mode clojurescript-mode))

(when (or (emacs-ide-dev-lang-enabled-p "haskell")
          (emacs-ide-dev-lang-enabled-p "clojure")
          (emacs-ide-dev-lang-enabled-p "elixir")
          (emacs-ide-dev-lang-enabled-p "ocaml"))

;; ── Haskell ───────────────────────────────────────────────────────────────
(use-package haskell-mode
  :if (and (emacs-ide-dev-lang-enabled-p "haskell") (executable-find "runhaskell"))
  :defer t :mode "\\.hs\\'" :interpreter "runhaskell"
  :init (setq haskell-process-type 'cabal-repl haskell-interactive-popup-errors nil)
  :config
  (emacs-ide-dev-bind-compile haskell-mode-map
    (lambda () (interactive) (compile (format "runhaskell %s" (shell-quote-argument (buffer-file-name)))))))
(use-package lsp-haskell
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (emacs-ide-dev-lang-enabled-p "haskell"))
  :hook (haskell-mode . lsp-deferred)
  :init (setq lsp-haskell-formatting-provider "ormolu"))
(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "haskell")
    (emacs-ide-dev-attach-formatter 'ormolu 'haskell-mode)))

;; ── Clojure + CIDER ───────────────────────────────────────────────────────
(use-package clojure-mode
  :if (emacs-ide-dev-lang-enabled-p "clojure")
  :defer t :mode (("\\.clj\\'" . clojure-mode) ("\\.cljc\\'" . clojure-mode)
                  ("\\.cljs\\'" . clojurescript-mode) ("\\.edn\\'" . clojure-mode)))
(use-package cider
  :if (emacs-ide-dev-lang-enabled-p "clojure")
  :after clojure-mode
  :bind (:map clojure-mode-map
              ("C-c r"   . cider-jack-in)
              ("C-c C-c" . cider-eval-defun-at-point)
              ("C-c C-b" . cider-eval-buffer)
              ("C-c C-t" . cider-test-run-tests))
  :init (setq cider-repl-display-help-banner nil cider-repl-use-pretty-printing t))
(use-package clj-refactor :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode))
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (executable-find "clojure-lsp"))
  :hook (clojure-mode . lsp-deferred))

;; ── Elixir ────────────────────────────────────────────────────────────────
(use-package elixir-mode
  :if (and (emacs-ide-dev-lang-enabled-p "elixir") (executable-find "elixir"))
  :defer t :mode (("\\.ex\\'" . elixir-mode) ("\\.exs\\'" . elixir-mode)))
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (emacs-ide-dev-lang-enabled-p "elixir")
           (executable-find "elixir-ls"))
  :hook (elixir-mode . lsp-deferred))
(use-package alchemist :if (emacs-ide-dev-lang-enabled-p "elixir") :after elixir-mode)

;; ── OCaml ─────────────────────────────────────────────────────────────────
(use-package tuareg
  :if (and (emacs-ide-dev-lang-enabled-p "ocaml") (executable-find "ocaml"))
  :defer t :mode (("\\.ml\\'" . tuareg-mode) ("\\.mli\\'" . tuareg-mode)))
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)  ; FIX-LSP-GUARD
           (emacs-ide-dev-lang-enabled-p "ocaml")
           (executable-find "ocamllsp"))
  :hook (tuareg-mode . lsp-deferred))

;; ── Erlang ────────────────────────────────────────────────────────────────
(use-package erlang
  :if (and (emacs-ide-dev-lang-enabled-p "erlang") (executable-find "erl"))
  :defer t :mode "\\.erl\\'")

) (provide 'lang-functional)
;;; lang-functional.el ends here
