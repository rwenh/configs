;;; lang-functional.el --- Functional Languages IDE layer -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "haskell"
  :tier 3 :lsp-server "haskell-language-server"
  :formatter "ormolu" :test-cmd "cabal test" :repl "ghci"
  :modes '(haskell-mode))

(emacs-ide-dev-register "clojure"
  :tier 3 :lsp-server "clojure-lsp"
  :formatter "cljfmt" :test-cmd "clj -M:test" :repl "cider"
  :modes '(clojure-mode clojurescript-mode))

(emacs-ide-dev-register "erlang"
  :tier 3 :lsp-server nil
  :formatter nil :test-cmd "rebar3 eunit" :repl "erl"
  :modes '(erlang-mode))

(when (or (emacs-ide-dev-lang-enabled-p "haskell")
          (emacs-ide-dev-lang-enabled-p "clojure")
          (emacs-ide-dev-lang-enabled-p "elixir")
          (emacs-ide-dev-lang-enabled-p "ocaml")
          (emacs-ide-dev-lang-enabled-p "erlang"))

;;;; ── Haskell ─────────────────────────────────────────────────────────────────

(use-package haskell-mode
  :if (and (emacs-ide-dev-lang-enabled-p "haskell")
           (executable-find "runhaskell"))
  :defer t
  :mode "\\.hs\\'"
  :interpreter "runhaskell"
  :init
  (setq haskell-process-type             'cabal-repl
        haskell-interactive-popup-errors  nil)
  :config
  (emacs-ide-dev-bind-compile haskell-mode-map
    (lambda ()
      (interactive)
      (compile (format "runhaskell %s"
                       (shell-quote-argument (buffer-file-name))))))
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'haskell-mode
      :launch (lambda ()
                (when (fboundp 'haskell-interactive-switch)
                  (haskell-interactive-switch)))
      :buffer-name    "*haskell*"
      :send-region-fn (lambda (beg end)
                        (cond
                         ((fboundp 'haskell-process-load-region)
                          (haskell-process-load-region beg end))
                         ((fboundp 'haskell-process-load-file)
                          (haskell-process-load-file))))))
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'haskell-mode
      :project-fn (lambda ()
                    (interactive)
                    (cond
                     ((executable-find "cabal") (compile "cabal test"))
                     ((executable-find "stack") (compile "stack test"))
                     (t (message "lang-functional: no Haskell test runner found")))))))

(use-package lsp-haskell
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "haskell"))
  :hook (haskell-mode . lsp-deferred)
  :init (setq lsp-haskell-formatting-provider "ormolu"))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "haskell")
    (emacs-ide-dev-attach-formatter 'ormolu 'haskell-mode)))

;;;; ── Clojure ─────────────────────────────────────────────────────────────────

(use-package clojure-mode
  :if (emacs-ide-dev-lang-enabled-p "clojure")
  :defer t
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'"  . clojure-mode)))

(use-package cider
  :if (emacs-ide-dev-lang-enabled-p "clojure")
  :after clojure-mode
  :bind (:map clojure-mode-map
              ("C-c C-c" . cider-eval-defun-at-point)
              ("C-c C-b" . cider-eval-buffer))
  :init
  (setq cider-repl-display-help-banner nil
        cider-repl-use-pretty-printing  t)
  :config
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'clojure-mode
      :launch         (lambda ()
                        (when (fboundp 'cider-jack-in) (cider-jack-in nil)))
      :buffer-name    "*cider-repl*"
      :send-region-fn (lambda (beg end)
                        (when (fboundp 'cider-eval-region)
                          (cider-eval-region beg end)))))
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'clojure-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "clj")
                        (compile "clj -M:test")
                      (message "lang-functional: clj not found on PATH")))
      :file-fn    (lambda ()
                    (interactive)
                    (when (fboundp 'cider-test-run-ns-tests)
                      (cider-test-run-ns-tests nil)))
      :point-fn   (lambda ()
                    (interactive)
                    (when (fboundp 'cider-test-run-test)
                      (cider-test-run-test))))))

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "clojure")
           (executable-find "clojure-lsp"))
  :hook (clojure-mode . lsp-deferred))

;;;; ── Elixir ──────────────────────────────────────────────────────────────────

(use-package elixir-mode
  :if (and (emacs-ide-dev-lang-enabled-p "elixir")
           (executable-find "elixir"))
  :defer t
  :mode (("\\.ex\\'"  . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :config
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'elixir-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "mix")
                        (compile "mix test")
                      (message "lang-functional: mix not found on PATH")))
      :file-fn    (lambda ()
                    (interactive)
                    (if (and (executable-find "mix") (buffer-file-name))
                        (compile (format "mix test %s"
                                         (shell-quote-argument (buffer-file-name))))
                      (message "lang-functional: mix not found or no file"))))))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "elixir")
           (executable-find "elixir-ls"))
  :hook (elixir-mode . lsp-deferred))

(use-package alchemist
  :if (and (emacs-ide-dev-lang-enabled-p "elixir")
           (executable-find "elixir"))
  :after elixir-mode)

;;;; ── OCaml ───────────────────────────────────────────────────────────────────

(use-package tuareg
  :if (and (emacs-ide-dev-lang-enabled-p "ocaml")
           (executable-find "ocaml"))
  :defer t
  :mode (("\\.ml\\'"  . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "ocaml")
           (executable-find "ocamllsp"))
  :hook (tuareg-mode . lsp-deferred))

;;;; ── Erlang ──────────────────────────────────────────────────────────────────

(use-package erlang
  :if (and (emacs-ide-dev-lang-enabled-p "erlang")
           (executable-find "erl"))
  :defer t
  :mode "\\.erl\\'")

) ;; end functional-enabled

(provide 'lang-functional)
;;; lang-functional.el ends here
