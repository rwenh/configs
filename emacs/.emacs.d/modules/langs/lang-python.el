;;; lang-python.el --- Python IDE layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Canonical lang module template.  Every other lang-*.el follows this
;;; exact section order and uses the same core-dev.el API calls.
;;;
;;; TEMPLATE SECTIONS (replicate verbatim, fill in lang-specific values):
;;;   1. Registration       → emacs-ide-dev-register
;;;   2. Config guard       → emacs-ide-dev-lang-enabled-p
;;;   3. Treesitter         → emacs-ide-dev-ensure-treesit
;;;   4. Major mode         → use-package :mode/:interpreter/:hook
;;;   5. LSP server         → use-package + emacs-ide-dev-lsp-hook
;;;   6. Formatter          → emacs-ide-dev-attach-formatter
;;;   7. REPL               → emacs-ide-dev-attach-repl
;;;   8. Test runner        → use-package or defun wired to tools-test
;;;   9. Debugger           → emacs-ide-dev-attach-dap
;;;  10. Project mgmt       → optional (poetry/pipenv/etc.)
;;;  11. Extras             → refactoring, docstring, notebooks
;;;
;;; Lazy loading: nothing here runs at startup.  The major-mode :mode
;;; entry triggers on first .py file open; every subsequent use-package
;;; uses :after python or :hook python-mode.
;;;
;;; Version: 1.0.1
;;; Fixes vs 1.0.0:
;;;   - FIX-4: Removed :hook from lsp-pyright use-package. tools-lsp.el already
;;;     hooks python-mode to lsp-deferred and requires lsp-pyright eagerly.
;;;     The extra hook caused LSP to start twice on every Python file.
;;; Code:

(require 'core-dev)

;; ============================================================================
;; 1. REGISTRATION
;; ============================================================================
(emacs-ide-dev-register "python"
  :tier        1
  :lsp-server  "pyright"
  :formatter   "black"
  :test-cmd    "pytest"
  :repl        "ipython"
  :modes       '(python-mode python-ts-mode))

;; ============================================================================
;; 2. CONFIG GUARD — skip entire module if disabled in config.yml
;; ============================================================================
(unless (emacs-ide-dev-lang-enabled-p "python")
  (provide 'lang-python)
  (message "lang-python: disabled in config.yml"))

(when (emacs-ide-dev-lang-enabled-p "python")

;; ============================================================================
;; 3. TREESITTER
;; ============================================================================
(emacs-ide-dev-ensure-treesit 'python)

;; ============================================================================
;; 4. MAJOR MODE
;; Lazy: fires only when a .py file opens.
;; ============================================================================
(use-package python
  :straight nil
  :defer t
  :mode (("\\.py\\'" . python-mode)
         ("\\.pyw\\'" . python-mode))
  :interpreter (("python"  . python-mode)
                ("python3" . python-mode)
                ("ipython" . python-mode))
  :init
  (setq python-indent-offset                    4
        python-shell-interpreter                (or (executable-find "ipython")
                                                    (executable-find "python3")
                                                    "python")
        python-shell-interpreter-args           (if (executable-find "ipython")
                                                    "--simple-prompt --no-color-info"
                                                  "")
        python-shell-completion-native-enable   nil
        python-indent-guess-indent-offset       t
        python-indent-guess-indent-offset-verbose nil)
  :config
  ;; Compile/run
  (defun emacs-ide-python-run ()
    "Run current Python file via python3."
    (interactive)
    (if (executable-find "python3")
        (compile (format "python3 %s"
                         (shell-quote-argument (buffer-file-name))))
      (message "lang-python: python3 not found on PATH")))

  (emacs-ide-dev-bind-compile python-mode-map #'emacs-ide-python-run))

;; ============================================================================
;; 5. LSP — PYRIGHT (preferred) or PYLSP (fallback)
;; FIX-4: :hook removed. tools-lsp.el hooks python-mode → lsp-deferred AND
;;   requires lsp-pyright eagerly in its :config, registering the pyright
;;   server before any Python buffer opens. A second hook here caused LSP
;;   to start twice. The :init vars are kept — they configure pyright before
;;   the server is selected by lsp-mode.
;; ============================================================================
(use-package lsp-pyright
  :if (or (executable-find "pyright")
          (executable-find "pyright-langserver"))
  :after python
  :init
  (setq lsp-pyright-use-library-code-for-types  t
        lsp-pyright-auto-import-completions      t
        lsp-pyright-stub-path                    ""
        lsp-pyright-venv-path                    (expand-file-name "~/.virtualenvs")
        lsp-pyright-multi-root                   nil))

;; Fallback to pylsp when pyright is absent
(with-eval-after-load 'python
  (unless (or (executable-find "pyright")
              (executable-find "pyright-langserver"))
    (when (and (executable-find "pylsp")
               (bound-and-true-p emacs-ide-lsp-enable)
               (fboundp 'lsp-deferred))
      (add-hook 'python-mode-hook #'lsp-deferred)
      (add-hook 'python-ts-mode-hook #'lsp-deferred))))

;; ============================================================================
;; 6. FORMATTER — black (async via apheleia)
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'black 'python-mode)
  (emacs-ide-dev-attach-formatter 'black 'python-ts-mode)
  ;; isort for import sorting (runs after black via apheleia pipeline)
  (when (executable-find "isort")
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(black isort))))

;; ============================================================================
;; 7. REPL — ipython or python-shell
;; ============================================================================
(defun emacs-ide-python-repl ()
  "Open Python REPL (ipython if available, else python3)."
  (interactive)
  (let ((interp (or (executable-find "ipython")
                    (executable-find "python3")
                    "python")))
    (if (fboundp 'run-python)
        (run-python interp nil t)
      (message "lang-python: run-python not available"))))

(with-eval-after-load 'python
  (emacs-ide-dev-attach-repl python-mode-map #'emacs-ide-python-repl)
  (define-key python-mode-map (kbd "C-c C-z") #'emacs-ide-python-repl)
  ;; Send region/buffer to REPL (built-in python.el bindings)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer))

;; ============================================================================
;; 8. TEST RUNNER — pytest
;; ============================================================================

;; pythonic gives us virtualenv-aware process execution
(use-package pythonic
  :after python
  :defer t)

(defun emacs-ide-python-test-file ()
  "Run pytest on the current file."
  (interactive)
  (if (executable-find "pytest")
      (compile (format "pytest -v %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-python: pytest not found. pip install pytest")))

(defun emacs-ide-python-test-project ()
  "Run pytest on the whole project."
  (interactive)
  (if (executable-find "pytest")
      (compile "pytest -v")
    (message "lang-python: pytest not found. pip install pytest")))

(defun emacs-ide-python-test-at-point ()
  "Run the test function at point via pytest -k."
  (interactive)
  (let ((fn (which-function)))
    (if (and fn (executable-find "pytest"))
        (compile (format "pytest -v -k %s" (shell-quote-argument fn)))
      (message "lang-python: cannot determine test name at point"))))

;; Register with tools-test.el dispatcher
(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (emacs-ide-test-register-runner
     'python-mode
     :file-fn    #'emacs-ide-python-test-file
     :project-fn #'emacs-ide-python-test-project
     :point-fn   #'emacs-ide-python-test-at-point)))

;; ============================================================================
;; 9. DEBUGGER — dap-python (debugpy)
;; ============================================================================
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Python :: debugpy" 'dap-python)
  (when (and (fboundp 'dap-register-debug-template)
             (executable-find "python3"))
    (dap-register-debug-template
     "Python :: debugpy :: file"
     (list :type         "python"
           :request      "launch"
           :name         "Python file"
           :program      "${file}"
           :console      "integratedTerminal"
           :justMyCode   t))
    (dap-register-debug-template
     "Python :: debugpy :: pytest"
     (list :type         "python"
           :request      "launch"
           :name         "pytest"
           :module       "pytest"
           :args         (list "-v" "${file}")
           :justMyCode   nil))))

;; ============================================================================
;; 10. PROJECT MANAGEMENT — poetry / pipenv / virtualenv
;; ============================================================================
(use-package poetry
  :if (executable-find "poetry")
  :after python
  :hook (python-mode . poetry-tracking-mode)
  :bind (:map python-mode-map
              ("C-c p p" . poetry-run)
              ("C-c p i" . poetry-add)
              ("C-c p s" . poetry-shell)))

(use-package pipenv
  :if (and (not (executable-find "poetry"))
           (executable-find "pipenv"))
  :after python
  :hook (python-mode . pipenv-mode)
  :bind (:map python-mode-map
              ("C-c p p" . pipenv-run)
              ("C-c p s" . pipenv-shell)))

;; Auto-activate virtualenv when entering a Python project
(use-package auto-virtualenv
  :after python
  :hook ((python-mode . auto-virtualenv-set-virtualenv)
         (projectile-after-switch-project . auto-virtualenv-set-virtualenv)))

;; ============================================================================
;; 11. EXTRAS
;; ============================================================================

;; Docstring generation (Google / NumPy / Sphinx styles)
(use-package sphinx-doc
  :if (executable-find "python3")
  :after python
  :hook (python-mode . sphinx-doc-mode)
  :bind (:map python-mode-map
              ("C-c d" . sphinx-doc)))

;; Rope-based refactoring (rename, extract, inline)
(use-package python-pytest
  :after python
  :defer t)

;; Jupyter notebooks via emacs-jupyter (optional, heavy)
(use-package jupyter
  :if (and (executable-find "jupyter")
           (emacs-ide-dev-lang-enabled-p "python-jupyter"))
  :after python
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl))

;; Live type annotations overlay (requires python-lsp-server + pylsp-mypy)
(use-package lsp-python-ms
  :disabled  ; disabled — pyright is preferred; enable manually if needed
  :after python)

) ;; end (when (emacs-ide-dev-lang-enabled-p "python") ...)

(provide 'lang-python)
;;; lang-python.el ends here
