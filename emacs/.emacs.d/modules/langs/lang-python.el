;;; lang-python.el --- Python IDE layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Canonical lang module template. Every other lang-*.el follows this
;;; exact section order and uses the same core-dev.el API calls.
;;;
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.2 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.2 to 3.0.4.
;;;   - FIX-DEFUN-IN-CONFIG: emacs-ide-python-run was defined inside python
;;;     :config — not visible to M-x until python mode loads. Moved to
;;;     top-level defun.
;;;   - FIX-REPL-ATTACH-ORDER: emacs-ide-dev-attach-repl and key bindings
;;;     were in with-eval-after-load 'python instead of python :config.
;;;     Moved inside python :config for cleaner, reliable ordering.
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for
;;;     python-mode and python-ts-mode inside python :config.
;;;   - FIX-DAP-PROGRAM-PLACEHOLDER: "${file}" in DAP templates is a VS Code
;;;     variable not expanded by dap-mode. Replaced with lambdas reading
;;;     buffer-file-name at launch time.
;;;   - FIX-POETRY-BINDINGS: poetry bound C-c p p/i/s in python-mode-map —
;;;     C-c p is the projectile prefix (tools-project.el). Remapped to
;;;     C-c P p/i/s (uppercase P) to avoid the collision.
;;;   - FIX-PIPENV-BINDINGS: pipenv had the same C-c p collision. Remapped
;;;     to C-c P p/s.
;;;   - FIX-ISORT-CHAIN: isort chaining only set python-mode in apheleia-mode-
;;;     alist, missing python-ts-mode. Both modes now chained.
;;;   - FIX-PYTHON-TS-MODE-TEST: Test runners were only registered for
;;;     python-mode, not python-ts-mode. Both now registered.
;;; Fixes vs 1.0.1 (retained):
;;;   - FIX-DOUBLE-PANE-1: lsp-pyright owned by tools-lsp.el.
;;;   - FIX-DOUBLE-PANE-2: sphinx-doc-mode hook removed.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-4: lsp-pyright hook removed from this file.
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
;; 2. CONFIG GUARD
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
;; 4. COMPILE / RUN COMMAND
;; FIX-DEFUN-IN-CONFIG: top-level so M-x sees it before python mode loads.
;; ============================================================================
(defun emacs-ide-python-run ()
  "Run current Python file via python3."
  (interactive)
  (if (executable-find "python3")
      (compile (format "python3 %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-python: python3 not found on PATH")))

;; ============================================================================
;; 4. MAJOR MODE
;; ============================================================================
(use-package python
  :straight nil
  :defer t
  :mode (("\\.py\\'"  . python-mode)
         ("\\.pyw\\'" . python-mode))
  :interpreter (("python"  . python-mode)
                ("python3" . python-mode)
                ("ipython" . python-mode))
  :init
  (setq python-indent-offset                     4
        python-shell-interpreter                 (or (executable-find "ipython")
                                                     (executable-find "python3")
                                                     "python")
        python-shell-interpreter-args            (if (executable-find "ipython")
                                                     "--simple-prompt --no-color-info"
                                                   "")
        python-shell-completion-native-enable    nil
        python-indent-guess-indent-offset        t
        python-indent-guess-indent-offset-verbose nil)
  :config
  ;; FIX-DEFUN-IN-CONFIG: now top-level; just bind here
  (emacs-ide-dev-bind-compile python-mode-map #'emacs-ide-python-run)
  ;; FIX-REPL-ATTACH-ORDER: moved here from with-eval-after-load
  (emacs-ide-dev-attach-repl python-mode-map #'emacs-ide-python-repl
                              (kbd "C-c x r"))
  (define-key python-mode-map (kbd "C-c C-z") #'emacs-ide-python-repl)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer)
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode '(python-mode python-ts-mode))
      (emacs-ide-repl-register mode
        :launch         #'emacs-ide-python-repl
        :buffer-name    "*Python*"
        :send-region-fn #'python-shell-send-region)))
  ;; FIX-PYTHON-TS-MODE-TEST: register both modes
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(python-mode python-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-python-test-file
        :project-fn #'emacs-ide-python-test-project
        :point-fn   #'emacs-ide-python-test-at-point))))

;; ============================================================================
;; 5. LSP — PYRIGHT (owned by tools-lsp.el) or PYLSP fallback
;; ============================================================================
;; Fallback to pylsp when pyright is absent
(with-eval-after-load 'python
  (unless (or (executable-find "pyright")
              (executable-find "pyright-langserver"))
    (when (and (executable-find "pylsp")
               (bound-and-true-p emacs-ide-lsp-enable)
               (fboundp 'lsp-deferred))
      (add-hook 'python-mode-hook    #'lsp-deferred)
      (add-hook 'python-ts-mode-hook #'lsp-deferred))))

;; ============================================================================
;; 6. FORMATTER — black (async via apheleia)
;; FIX-ISORT-CHAIN: python-ts-mode now also chained with isort.
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'black 'python-mode)
  (emacs-ide-dev-attach-formatter 'black 'python-ts-mode)
  (when (executable-find "isort")
    (setf (alist-get 'python-mode    apheleia-mode-alist) '(black isort))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black isort))))

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

;; ============================================================================
;; 8. TEST RUNNER — pytest
;; ============================================================================
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

;; Register with tools-test.el dispatcher (also done in python :config above)
(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(python-mode python-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-python-test-file
        :project-fn #'emacs-ide-python-test-project
        :point-fn   #'emacs-ide-python-test-at-point))))

;; ============================================================================
;; 9. DEBUGGER — dap-python (debugpy)
;; FIX-DAP-PROGRAM-PLACEHOLDER: "${file}" replaced with lambdas.
;; ============================================================================
(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Python :: debugpy" 'dap-python)
  (when (and (fboundp 'dap-register-debug-template)
             (executable-find "python3"))
    (dap-register-debug-template
     "Python :: debugpy :: file"
     (list :type       "python"
           :request    "launch"
           :name       "Python file"
           ;; FIX-DAP-PROGRAM-PLACEHOLDER: lambda reads buffer at launch time
           :program    (lambda () (buffer-file-name))
           :console    "integratedTerminal"
           :justMyCode t))
    (dap-register-debug-template
     "Python :: debugpy :: pytest"
     (list :type       "python"
           :request    "launch"
           :name       "pytest"
           :module     "pytest"
           ;; FIX-DAP-PROGRAM-PLACEHOLDER: lambda reads buffer at launch time
           :args       (lambda () (list "-v" (buffer-file-name)))
           :justMyCode nil))))

;; ============================================================================
;; 10. PROJECT MANAGEMENT — poetry / pipenv / virtualenv
;; FIX-POETRY-BINDINGS: remapped from C-c p (projectile prefix) to C-c P.
;; FIX-PIPENV-BINDINGS: same remap.
;; ============================================================================
(use-package poetry
  :if (executable-find "poetry")
  :after python
  :hook (python-mode . poetry-tracking-mode)
  :bind (:map python-mode-map
              ;; FIX-POETRY-BINDINGS: C-c P (uppercase) avoids projectile prefix
              ("C-c P p" . poetry-run)
              ("C-c P i" . poetry-add)
              ("C-c P s" . poetry-shell)))

(use-package pipenv
  :if (and (not (executable-find "poetry"))
           (executable-find "pipenv"))
  :after python
  :hook (python-mode . pipenv-mode)
  :bind (:map python-mode-map
              ;; FIX-PIPENV-BINDINGS: C-c P (uppercase) avoids projectile prefix
              ("C-c P p" . pipenv-run)
              ("C-c P s" . pipenv-shell)))

(use-package auto-virtualenv
  :after python
  :hook ((python-mode . auto-virtualenv-set-virtualenv)
         (projectile-after-switch-project . auto-virtualenv-set-virtualenv)))

;; ============================================================================
;; 11. EXTRAS
;; ============================================================================
(use-package sphinx-doc
  :if (executable-find "python3")
  :after python
  :defer t
  :bind (:map python-mode-map
              ("C-c d" . sphinx-doc)))

(use-package python-pytest
  :after python
  :defer t)

(use-package jupyter
  :if (and (executable-find "jupyter")
           (emacs-ide-dev-lang-enabled-p "python-jupyter"))
  :after python
  :defer t
  :commands (jupyter-run-repl jupyter-connect-repl))

(use-package lsp-python-ms
  :disabled  ; disabled — pyright is preferred; enable manually if needed
  :after python)

) ;; end (when (emacs-ide-dev-lang-enabled-p "python"))

(provide 'lang-python)
;;; lang-python.el ends here
