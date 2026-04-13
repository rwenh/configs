;;; lang-data.el --- Data Science IDE layer (R / Julia / Notebooks) -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (post-audit calibration):
;;;   - FIX-R-FILE-FN: emacs-ide-r-run was registered as :file-fn for the R test
;;;     runner. emacs-ide-r-run is a run-the-script command (Rscript file.R), not
;;;     a test command. Added emacs-ide-r-test-file which calls
;;;     testthat::test_file() on the current buffer. Registered as :file-fn.
;;; Fixes vs 1.0.1 (audit, retained):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG-R: emacs-ide-r-run was defined inside ess :config
;;;     — not visible to M-x until ESS loads. Moved to top-level defun.
;;;   - FIX-DEFUN-IN-CONFIG-JULIA: emacs-ide-julia-run was defined inside
;;;     julia-mode :config — same issue. Moved to top-level defun.
;;;   - FIX-JULIA-REPL-KEY: ("C-c r" . julia-repl) in julia-repl :bind
;;;     collides with the recovery prefix map (emacs-ide-recovery-map).
;;;     Changed to C-c x r (unified REPL dispatch key) consistent with
;;;     tools-repl.el and keybindings.el.
;;;   - FIX-REPL-REGISTER: R and Julia now explicitly register with
;;;     emacs-ide-repl-register after their modes load, making the
;;;     C-c x r REPL dispatch reliable rather than depending on
;;;     tools-repl.el's with-eval-after-load order.
;;;   - FIX-TEST-REGISTER-R: Added emacs-ide-test-register-runner for
;;;     ess-r-mode using testthat (devtools::test()) or Rscript fallback.
;;;   - FIX-TEST-REGISTER-JULIA: Added emacs-ide-test-register-runner for
;;;     julia-mode using julia --project -e "using Pkg; Pkg.test()".
;;;   - FIX-ESS-MODE-MAP: emacs-ide-dev-bind-compile now guarded with
;;;     (boundp 'ess-r-mode-map) before use.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP-GUARD: (bound-and-true-p emacs-ide-lsp-enable) guards on all
;;;     lsp-mode use-package blocks.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "r" :tier 2 :lsp-server "r-languageserver"
  :formatter "styler" :test-cmd "Rscript" :repl "R" :modes '(ess-r-mode))
(emacs-ide-dev-register "julia" :tier 2 :lsp-server "julia-lsp"
  :formatter "JuliaFormatter" :test-cmd "julia" :repl "julia" :modes '(julia-mode))

(when (or (emacs-ide-dev-lang-enabled-p "r")
          (emacs-ide-dev-lang-enabled-p "julia"))

;; ============================================================================
;; R via ESS
;; ============================================================================

;; FIX-DEFUN-IN-CONFIG-R: top-level so M-x sees it before ESS loads
(defun emacs-ide-r-run ()
  "Run current R file via Rscript."
  (interactive)
  (if (executable-find "Rscript")
      (compile (format "Rscript %s" (shell-quote-argument (buffer-file-name))))
    (message "lang-data: Rscript not found")))

(defun emacs-ide-r-test-file ()
  "Run testthat tests for the current R file via Rscript.
FIX-R-FILE-FN: uses testthat::test_file() so this is actually a test
command rather than a plain run command."
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
  (setq ess-use-flymake       nil  ; LSP owns diagnostics
        ess-r-flymake-linters '()
        ess-indent-level       2
        ess-fancy-comments     nil)
  :config
  ;; FIX-ESS-MODE-MAP: guard with boundp before binding
  (when (boundp 'ess-r-mode-map)
    (emacs-ide-dev-bind-compile ess-r-mode-map #'emacs-ide-r-run))
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'ess-r-mode
      :launch         (lambda () (when (fboundp 'R) (R)))
      :buffer-name    "*R*"
      :send-region-fn (lambda (beg end)
                        (when (fboundp 'ess-eval-region)
                          (ess-eval-region beg end nil)))))
  ;; FIX-TEST-REGISTER-R: register with test runner registry
  ;; FIX-R-FILE-FN: :file-fn now uses emacs-ide-r-test-file (testthat::test_file)
  ;; instead of emacs-ide-r-run (plain Rscript execution).
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'ess-r-mode
      :project-fn #'emacs-ide-r-test-project
      :file-fn    #'emacs-ide-r-test-file)))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "r"))
  :hook (ess-r-mode . lsp-deferred))

;; ============================================================================
;; Julia
;; ============================================================================

;; FIX-DEFUN-IN-CONFIG-JULIA: top-level so M-x sees it before julia-mode loads
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
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'julia-mode
      :launch         (lambda () (when (fboundp 'julia-repl) (julia-repl)))
      :buffer-name    "*julia*"
      :send-region-fn nil))
  ;; FIX-TEST-REGISTER-JULIA: register with test runner registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner 'julia-mode
      :project-fn #'emacs-ide-julia-test-project
      :file-fn    #'emacs-ide-julia-run)))

(use-package julia-repl
  :if (and (emacs-ide-dev-lang-enabled-p "julia") (executable-find "julia"))
  :after julia-mode
  :hook (julia-mode . julia-repl-mode)
  :bind (:map julia-mode-map
              ;; FIX-JULIA-REPL-KEY: was "C-c r" which collides with the
              ;; recovery prefix map. Changed to C-c x r (REPL dispatch key).
              ("C-c x r" . julia-repl)))

(use-package lsp-julia
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (emacs-ide-dev-lang-enabled-p "julia"))
  :after julia-mode
  :hook (julia-mode . lsp-deferred))

;; ============================================================================
;; Jupyter notebooks
;; ============================================================================
(use-package jupyter
  :if (and (executable-find "jupyter")
           (emacs-ide-dev-lang-enabled-p "python-jupyter"))
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl jupyter-org-interaction-mode))

) ;; end (when r or julia enabled)

(provide 'lang-data)
;;; lang-data.el ends here
