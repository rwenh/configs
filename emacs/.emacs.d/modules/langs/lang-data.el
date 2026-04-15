;;; lang-data.el --- Data Science IDE layer (R / Julia / Notebooks) -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "r" :tier 2 :lsp-server "r-languageserver"
  :formatter "styler" :test-cmd "Rscript" :repl "R" :modes '(ess-r-mode))
(emacs-ide-dev-register "julia" :tier 2 :lsp-server "julia-lsp"
  :formatter "JuliaFormatter" :test-cmd "julia" :repl "julia" :modes '(julia-mode))

(when (or (emacs-ide-dev-lang-enabled-p "r")
          (emacs-ide-dev-lang-enabled-p "julia"))

(defun emacs-ide-r-run ()
  "Run current R file via Rscript."
  (interactive)
  (if (executable-find "Rscript")
      (compile (format "Rscript %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-data: Rscript not found")))

(defun emacs-ide-r-test-file ()
  "Run testthat tests for the current R file via Rscript."
  (interactive)
  (if (and (executable-find "Rscript") (buffer-file-name))
      (compile (format "Rscript -e 'testthat::test_file(\"%s\")'"
                       (buffer-file-name)))
    (message "lang-data: Rscript not found or no file")))

(defun emacs-ide-r-test-project ()
  "Run R project tests via devtools::test() or Rscript."
  (interactive)
  (cond
   ((and (executable-find "Rscript")
         (file-exists-p "DESCRIPTION"))
    (compile "Rscript -e 'devtools::test()'"))
   ((executable-find "Rscript")
    (compile "Rscript -e 'testthat::test_dir(\"tests\")'"))
   (t (message "lang-data: Rscript not found"))))

(use-package ess
  :if (and (emacs-ide-dev-lang-enabled-p "r") (executable-find "R"))
  :defer t
  :mode (("\\.R\\'" . ess-r-mode) ("\\.r\\'" . ess-r-mode))
  :init
  (setq ess-use-flymake       nil
        ess-r-flymake-linters '()
        ess-indent-level       2
        ess-fancy-comments     nil)
  :config
  (when (boundp 'ess-r-mode-map)
    (emacs-ide-dev-bind-compile ess-r-mode-map #'emacs-ide-r-run))
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'ess-r-mode
      :launch         (lambda () (when (fboundp 'R) (R)))
      :buffer-name    "*R*"
      :send-region-fn (lambda (beg end)
                        (when (fboundp 'ess-eval-region)
                          (ess-eval-region beg end nil)))))
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'ess-r-mode
      :project-fn #'emacs-ide-r-test-project
      :file-fn    #'emacs-ide-r-test-file)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "r"))
  :hook (ess-r-mode . lsp-deferred))

(defun emacs-ide-julia-run ()
  "Run current Julia file."
  (interactive)
  (if (executable-find "julia")
      (compile (format "julia %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-data: julia not found")))

(defun emacs-ide-julia-test-project ()
  "Run Julia project tests via Pkg.test()."
  (interactive)
  (if (executable-find "julia")
      (compile "julia --project -e 'using Pkg; Pkg.test()'")
    (message "lang-data: julia not found")))

(use-package julia-mode
  :if (and (emacs-ide-dev-lang-enabled-p "julia") (executable-find "julia"))
  :defer t
  :mode "\\.jl\\'"
  :config
  (emacs-ide-dev-bind-compile julia-mode-map #'emacs-ide-julia-run)
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'julia-mode
      :launch         (lambda () (when (fboundp 'julia-repl) (julia-repl)))
      :buffer-name    "*julia*"
      :send-region-fn nil))
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'julia-mode
      :project-fn #'emacs-ide-julia-test-project
      :file-fn    #'emacs-ide-julia-run)))

(use-package julia-repl
  :if (and (emacs-ide-dev-lang-enabled-p "julia") (executable-find "julia"))
  :after julia-mode
  :hook (julia-mode . julia-repl-mode)
  :bind (:map julia-mode-map
              ("C-c x r" . julia-repl)))

(use-package lsp-julia
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "julia"))
  :after julia-mode
  :hook (julia-mode . lsp-deferred))

(use-package jupyter
  :if (and (executable-find "jupyter")
           (emacs-ide-dev-lang-enabled-p "python-jupyter"))
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-org-interaction-mode))

)

(provide 'lang-data)
;;; lang-data.el ends here
