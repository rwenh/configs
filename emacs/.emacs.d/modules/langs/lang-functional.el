;;; lang-functional.el --- Functional Languages IDE layer -*- lexical-binding: t -*-
;;; Haskell · Clojure · Elixir · Erlang · OCaml
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-CIDER-REPL-KEY: ("C-c r" . cider-jack-in) in cider :bind collided
;;;     with the recovery prefix map (emacs-ide-recovery-map). Changed to
;;;     C-c x r (unified REPL dispatch key).
;;;   - FIX-CIDER-CCC-T: ("C-c C-t" . cider-test-run-tests) in cider :bind
;;;     shadowed the global emacs-ide-test-run binding set in tools-test.el.
;;;     Removed from the global map; test dispatch goes through the registry.
;;;   - FIX-CLOJURE-LANG-ENABLED: lsp-mode for clojure only checked
;;;     (executable-find "clojure-lsp") but not (emacs-ide-dev-lang-enabled-p
;;;     "clojure") — LSP would hook even when clojure is disabled in config.
;;;     Added the lang-enabled check.
;;;   - FIX-REPL-REGISTER-HASKELL: Added explicit emacs-ide-repl-register for
;;;     haskell-mode inside haskell-mode :config for reliable C-c x r dispatch.
;;;   - FIX-REPL-REGISTER-CLOJURE: Added explicit emacs-ide-repl-register for
;;;     clojure-mode inside cider :config for reliable C-c x r dispatch.
;;;   - FIX-TEST-REGISTER-HASKELL: Added emacs-ide-test-register-runner for
;;;     haskell-mode (cabal test / stack test).
;;;   - FIX-TEST-REGISTER-CLOJURE: Added emacs-ide-test-register-runner for
;;;     clojure-mode (clj -M:test).
;;;   - FIX-TEST-REGISTER-ELIXIR: Added emacs-ide-test-register-runner for
;;;     elixir-mode (mix test).
;;;   - FIX-ERLANG-GUARD: erlang was not registered with emacs-ide-dev-register
;;;     so emacs-ide-dev-lang-enabled-p always returned t. Added register call.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP-GUARD: (bound-and-true-p emacs-ide-lsp-enable) guards on all
;;;     lsp-mode use-package blocks.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "haskell" :tier 3 :lsp-server "haskell-language-server"
  :formatter "ormolu" :test-cmd "cabal test" :repl "ghci" :modes '(haskell-mode))
(emacs-ide-dev-register "clojure" :tier 3 :lsp-server "clojure-lsp"
  :formatter "cljfmt" :test-cmd "clj -M:test" :repl "cider"
  :modes '(clojure-mode clojurescript-mode))
;; FIX-ERLANG-GUARD: register erlang so lang-enabled-p returns the correct value
(emacs-ide-dev-register "erlang" :tier 3 :lsp-server nil
  :formatter nil :test-cmd "rebar3 eunit" :repl "erl" :modes '(erlang-mode))

(when (or (emacs-ide-dev-lang-enabled-p "haskell")
          (emacs-ide-dev-lang-enabled-p "clojure")
          (emacs-ide-dev-lang-enabled-p "elixir")
          (emacs-ide-dev-lang-enabled-p "ocaml")
          (emacs-ide-dev-lang-enabled-p "erlang"))

;; ============================================================================
;; Haskell
;; ============================================================================
(use-package haskell-mode
  :if (and (emacs-ide-dev-lang-enabled-p "haskell") (executable-find "runhaskell"))
  :defer t
  :mode "\\.hs\\'"
  :interpreter "runhaskell"
  :init
  (setq haskell-process-type            'cabal-repl
        haskell-interactive-popup-errors nil)
  :config
  (emacs-ide-dev-bind-compile haskell-mode-map
    (lambda () (interactive)
      (compile (format "runhaskell %s"
                       (shell-quote-argument (buffer-file-name))))))
  ;; FIX-REPL-REGISTER-HASKELL: explicit registration for C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'haskell-mode
      :launch         (lambda ()
                        (when (fboundp 'haskell-interactive-switch)
                          (haskell-interactive-switch)))
      :buffer-name    "*haskell*"
      :send-region-fn (lambda (beg end)
                        (cond
                         ((fboundp 'haskell-process-load-region)
                          (haskell-process-load-region beg end))
                         ((fboundp 'haskell-process-load-file)
                          (haskell-process-load-file))))))
  ;; FIX-TEST-REGISTER-HASKELL: register with test runner registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'haskell-mode
      :project-fn (lambda ()
                    (interactive)
                    (cond
                     ((executable-find "cabal") (compile "cabal test"))
                     ((executable-find "stack") (compile "stack test"))
                     (t (message "lang-functional: no haskell test runner found")))))))

(use-package lsp-haskell
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "haskell"))
  :hook (haskell-mode . lsp-deferred)
  :init
  (setq lsp-haskell-formatting-provider "ormolu"))

(with-eval-after-load 'apheleia
  (when (emacs-ide-dev-lang-enabled-p "haskell")
    (emacs-ide-dev-attach-formatter 'ormolu 'haskell-mode)))

;; ============================================================================
;; Clojure + CIDER
;; ============================================================================
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
              ;; FIX-CIDER-REPL-KEY: was "C-c r" (recovery prefix). Now C-c x r.
              ("C-c x r" . cider-jack-in)
              ("C-c C-c" . cider-eval-defun-at-point)
              ("C-c C-b" . cider-eval-buffer))
              ;; FIX-CIDER-CCC-T: removed "C-c C-t" → cider-test-run-tests;
              ;; C-c C-t is the global emacs-ide-test-run. Test dispatch goes
              ;; through the registry (registered below in :config).
  :init
  (setq cider-repl-display-help-banner nil
        cider-repl-use-pretty-printing  t)
  :config
  ;; FIX-REPL-REGISTER-CLOJURE: explicit registration for C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'clojure-mode
      :launch         (lambda ()
                        (when (fboundp 'cider-jack-in) (cider-jack-in nil)))
      :buffer-name    "*cider-repl*"
      :send-region-fn (lambda (beg end)
                        (when (fboundp 'cider-eval-region)
                          (cider-eval-region beg end)))))
  ;; FIX-TEST-REGISTER-CLOJURE: register with test runner registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'clojure-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "clj")
                        (compile "clj -M:test")
                      (message "lang-functional: clj not found")))
      :file-fn (lambda ()
                 (interactive)
                 (when (fboundp 'cider-test-run-ns-tests)
                   (cider-test-run-ns-tests nil)))
      :point-fn (lambda ()
                  (interactive)
                  (when (fboundp 'cider-test-run-test)
                    (cider-test-run-test))))))

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode))

(use-package lsp-mode
  ;; FIX-CLOJURE-LANG-ENABLED: added lang-enabled check alongside executable check
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "clojure")
           (executable-find "clojure-lsp"))
  :hook (clojure-mode . lsp-deferred))

;; ============================================================================
;; Elixir
;; ============================================================================
(use-package elixir-mode
  :if (and (emacs-ide-dev-lang-enabled-p "elixir") (executable-find "elixir"))
  :defer t
  :mode (("\\.ex\\'"  . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :config
  ;; FIX-TEST-REGISTER-ELIXIR: register with test runner registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'elixir-mode
      :project-fn (lambda ()
                    (interactive)
                    (if (executable-find "mix")
                        (compile "mix test")
                      (message "lang-functional: mix not found")))
      :file-fn (lambda ()
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
  :if (emacs-ide-dev-lang-enabled-p "elixir")
  :after elixir-mode)

;; ============================================================================
;; OCaml
;; ============================================================================
(use-package tuareg
  :if (and (emacs-ide-dev-lang-enabled-p "ocaml") (executable-find "ocaml"))
  :defer t
  :mode (("\\.ml\\'"  . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "ocaml")
           (executable-find "ocamllsp"))
  :hook (tuareg-mode . lsp-deferred))

;; ============================================================================
;; Erlang
;; ============================================================================
(use-package erlang
  :if (and (emacs-ide-dev-lang-enabled-p "erlang") (executable-find "erl"))
  :defer t
  :mode "\\.erl\\'")

) ;; end (when any functional lang enabled)

(provide 'lang-functional)
;;; lang-functional.el ends here
