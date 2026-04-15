;;; lang-python.el --- Python IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "python"
  :tier        1
  :lsp-server  "pyright"
  :formatter   "black"
  :test-cmd    "pytest"
  :repl        "ipython"
  :modes       '(python-mode python-ts-mode))

(unless (emacs-ide-dev-lang-enabled-p "python")
  (provide 'lang-python)
  (message "lang-python: disabled in config.yml"))

(when (emacs-ide-dev-lang-enabled-p "python")

(emacs-ide-dev-ensure-treesit 'python)

(defun emacs-ide-python-run ()
  "Run current Python file via python3."
  (interactive)
  (if (executable-find "python3")
      (compile (format "python3 %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-python: python3 not found on PATH")))

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
  (emacs-ide-dev-bind-compile python-mode-map #'emacs-ide-python-run)
  (emacs-ide-dev-attach-repl python-mode-map #'emacs-ide-python-repl
                              (kbd "C-c x r"))
  (define-key python-mode-map (kbd "C-c C-z") #'emacs-ide-python-repl)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer)
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode '(python-mode python-ts-mode))
      (emacs-ide-repl-register mode
        :launch         #'emacs-ide-python-repl
        :buffer-name    "*Python*"
        :send-region-fn #'python-shell-send-region)))
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(python-mode python-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-python-test-file
        :project-fn #'emacs-ide-python-test-project
        :point-fn   #'emacs-ide-python-test-at-point))))

(with-eval-after-load 'python
  (unless (or (executable-find "pyright")
              (executable-find "pyright-langserver"))
    (when (and (executable-find "pylsp")
               (bound-and-true-p emacs-ide-lsp-enable)
               (fboundp 'lsp-deferred))
      (add-hook 'python-mode-hook    #'lsp-deferred)
      (add-hook 'python-ts-mode-hook #'lsp-deferred))))

(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'black 'python-mode)
  (emacs-ide-dev-attach-formatter 'black 'python-ts-mode)
  (when (executable-find "isort")
    (setf (alist-get 'python-mode    apheleia-mode-alist) '(black isort))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black isort))))

(defun emacs-ide-python-repl ()
  "Open Python REPL (ipython if available, else python3)."
  (interactive)
  (let ((interp (or (executable-find "ipython")
                    (executable-find "python3")
                    "python")))
    (if (fboundp 'run-python)
        (run-python interp nil t)
      (message "lang-python: run-python not available"))))

(use-package pythonic
  :after python
  :defer t)

(defun emacs-ide-python-test-file ()
  "Run pytest on the current file."
  (interactive)
  (if (executable-find "pytest")
      (compile (format "pytest -v %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-python: pytest not found")))

(defun emacs-ide-python-test-project ()
  "Run pytest on the whole project."
  (interactive)
  (if (executable-find "pytest")
      (compile "pytest -v")
    (message "lang-python: pytest not found")))

(defun emacs-ide-python-test-at-point ()
  "Run the test function at point via pytest -k."
  (interactive)
  (let ((fn (which-function)))
    (if (and fn (executable-find "pytest"))
        (compile (format "pytest -v -k %s" (shell-quote-argument fn)))
      (message "lang-python: cannot determine test name at point"))))

(with-eval-after-load 'tools-test
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(python-mode python-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-python-test-file
        :project-fn #'emacs-ide-python-test-project
        :point-fn   #'emacs-ide-python-test-at-point))))

(with-eval-after-load 'dap-mode
  (emacs-ide-dev-attach-dap "Python :: debugpy" 'dap-python)
  (when (and (fboundp 'dap-register-debug-template)
             (executable-find "python3"))
    (dap-register-debug-template
     "Python :: debugpy :: file"
     (list :type       "python"
           :request    "launch"
           :name       "Python file"
           :program    (lambda () (buffer-file-name))
           :console    "integratedTerminal"
           :justMyCode t))
    (dap-register-debug-template
     "Python :: debugpy :: pytest"
     (list :type       "python"
           :request    "launch"
           :name       "pytest"
           :module     "pytest"
           :args       (lambda () (list "-v" (buffer-file-name)))
           :justMyCode nil))))

(use-package poetry
  :if (executable-find "poetry")
  :after python
  :hook (python-mode . poetry-tracking-mode)
  :bind (:map python-mode-map
              ("C-c P p" . poetry-run)
              ("C-c P i" . poetry-add)
              ("C-c P s" . poetry-shell)))

(use-package pipenv
  :if (and (not (executable-find "poetry"))
           (executable-find "pipenv"))
  :after python
  :hook (python-mode . pipenv-mode)
  :bind (:map python-mode-map
              ("C-c P p" . pipenv-run)
              ("C-c P s" . pipenv-shell)))

(use-package auto-virtualenv
  :after python
  :hook ((python-mode . auto-virtualenv-set-virtualenv)
         (projectile-after-switch-project . auto-virtualenv-set-virtualenv)))

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
  :disabled
  :after python)

)

(provide 'lang-python)
;;; lang-python.el ends here
