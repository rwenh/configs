;;; lang-csharp.el --- C# / .NET IDE layer -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "csharp"
  :tier 2
  :lsp-server "omnisharp"
  :formatter  "csharpier"
  :test-cmd   "dotnet test"
  :repl       "dotnet-script"
  :modes      '(csharp-mode csharp-ts-mode))

(unless (emacs-ide-dev-lang-enabled-p "csharp")
  (provide 'lang-csharp)
  (message "lang-csharp: disabled in config.yml"))

(when (emacs-ide-dev-lang-enabled-p "csharp")

;;;; ── Tree-sitter ─────────────────────────────────────────────────────────────

(emacs-ide-dev-ensure-treesit 'c-sharp)

;;;; ── Run helpers ─────────────────────────────────────────────────────────────

(defun emacs-ide-csharp-run ()
  "Run the .NET project via dotnet run."
  (interactive)
  (if (executable-find "dotnet")
      (let ((root (or (and (fboundp 'projectile-project-root)
                           (ignore-errors (projectile-project-root)))
                      default-directory)))
        (if (directory-files root nil "\\.csproj$\\|\\.sln$")
            (let ((default-directory root))
              (compile "dotnet run"))
          (compile (format "dotnet script %s"
                           (shell-quote-argument (buffer-file-name))))))
    (message "lang-csharp: dotnet not found on PATH")))

(defun emacs-ide-csharp-build ()
  "Build the .NET solution or project."
  (interactive)
  (if (executable-find "dotnet")
      (compile "dotnet build")
    (message "lang-csharp: dotnet not found on PATH")))

;;;; ── Test helpers ────────────────────────────────────────────────────────────

(defun emacs-ide-csharp-test-project ()
  "Run all .NET tests via dotnet test."
  (interactive)
  (if (executable-find "dotnet")
      (compile "dotnet test --verbosity normal")
    (message "lang-csharp: dotnet not found on PATH")))

(defun emacs-ide-csharp-test-file ()
  "Run tests in the current file's class (dotnet test --filter)."
  (interactive)
  (if (and (executable-find "dotnet") (buffer-file-name))
      (let* ((class (file-name-base (buffer-file-name))))
        (compile (format "dotnet test --filter FullyQualifiedName~%s"
                         (shell-quote-argument class))))
    (message "lang-csharp: dotnet not found or no file")))

;;;; ── REPL (dotnet-script / csi) ─────────────────────────────────────────────

(defun emacs-ide-csharp-repl ()
  "Open a C# REPL (dotnet-script preferred, csi fallback)."
  (interactive)
  (cond
   ((executable-find "dotnet-script")
    (require 'comint)
    (make-comint "csharp-repl" "dotnet-script" nil "repl")
    (switch-to-buffer "*csharp-repl*"))
   ((executable-find "csi")
    (require 'comint)
    (make-comint "csharp-repl" "csi")
    (switch-to-buffer "*csharp-repl*"))
   ((executable-find "dotnet")
    ;; dotnet-repl via dotnet tool
    (require 'comint)
    (make-comint "csharp-repl" "dotnet" nil "repl")
    (switch-to-buffer "*csharp-repl*"))
   (t (message "lang-csharp: no C# REPL found (install dotnet-script: dotnet tool install -g dotnet-script)"))))

;;;; ── csharp-mode (built-in Emacs 29+) ──────────────────────────────────────

(use-package csharp-mode
  :straight nil
  :defer t
  :mode (("\\.cs\\'"   . csharp-mode)
         ("\\.csx\\'"  . csharp-mode))
  :init
  (setq csharp-mode-indent-offset 4)
  :config
  (emacs-ide-dev-bind-compile csharp-mode-map #'emacs-ide-csharp-run)
  (define-key csharp-mode-map (kbd "C-c C-b")
    (lambda () (interactive) (compile "dotnet build")))
  (define-key csharp-mode-map (kbd "C-c C-z") #'emacs-ide-csharp-repl)
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode '(csharp-mode csharp-ts-mode))
      (emacs-ide-repl-register mode
        :launch         #'emacs-ide-csharp-repl
        :buffer-name    "*csharp-repl*"
        :send-region-fn nil))))

;; Wire ts-mode keybindings when available
(with-eval-after-load 'csharp-ts-mode
  (when (boundp 'csharp-ts-mode-map)
    (emacs-ide-dev-bind-compile csharp-ts-mode-map #'emacs-ide-csharp-run)
    (define-key csharp-ts-mode-map (kbd "C-c C-b")
      (lambda () (interactive) (compile "dotnet build")))
    (define-key csharp-ts-mode-map (kbd "C-c C-z") #'emacs-ide-csharp-repl)))

;;;; ── LSP (OmniSharp or csharp-ls) ──────────────────────────────────────────

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((csharp-mode csharp-ts-mode) . lsp-deferred)
  :config
  ;; OmniSharp: install via `dotnet tool install -g OmniSharp`
  ;; csharp-ls: install via `dotnet tool install -g csharp-ls` (lighter weight)
  ;; lsp-mode auto-detects whichever is available.
  (when (boundp 'lsp-csharp-server-path)
    (let ((omnisharp (executable-find "omnisharp"))
          (csharpls  (executable-find "csharp-ls")))
      (when (or omnisharp csharpls)
        (setq lsp-csharp-server-path (or omnisharp csharpls))))))

;;;; ── Formatter (csharpier) ──────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  (when (executable-find "dotnet-csharpier")
    (unless (assq 'csharpier apheleia-formatters)
      (push '(csharpier "dotnet-csharpier" "--write-stdout")
            apheleia-formatters))
    (setf (alist-get 'csharp-mode    apheleia-mode-alist) 'csharpier)
    (setf (alist-get 'csharp-ts-mode apheleia-mode-alist) 'csharpier)))

;;;; ── dotnet helpers ─────────────────────────────────────────────────────────

(defun emacs-ide-csharp-add-package (package)
  "Add a NuGet PACKAGE to the project via dotnet add package."
  (interactive "sNuGet package: ")
  (unless (string-empty-p package)
    (compile (format "dotnet add package %s"
                     (shell-quote-argument package)))))

(defun emacs-ide-csharp-restore ()
  "Restore NuGet packages via dotnet restore."
  (interactive)
  (compile "dotnet restore"))

(defun emacs-ide-csharp-publish ()
  "Publish the .NET project."
  (interactive)
  (compile "dotnet publish -c Release"))

;;;; ── Unity project detection helper ─────────────────────────────────────────

(defun emacs-ide-csharp--unity-p ()
  "Return non-nil if the current project is a Unity project."
  (when-let ((root (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))))
    (or (file-directory-p (expand-file-name "Assets" root))
        (file-directory-p (expand-file-name "ProjectSettings" root)))))

;; When in a Unity project, set the Unity-compatible LSP server if available
(add-hook 'csharp-mode-hook
          (lambda ()
            (when (emacs-ide-csharp--unity-p)
              (message "Unity project detected — using OmniSharp for C#"))))

;;;; ── Test runner registration ────────────────────────────────────────────────

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(csharp-mode csharp-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-csharp-test-file
        :project-fn #'emacs-ide-csharp-test-project))))

;;;; ── DAP (NetCoreDbg) ────────────────────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (require 'dap-netcore nil t)
  (when (fboundp 'dap-register-debug-template)
    (dap-register-debug-template "C# :: dotnet :: launch"
      (list :type            "coreclr"
            :request         "launch"
            :name            "C# dotnet run"
            :program         "dotnet"
            :args            ["run"]
            :cwd             (lambda ()
                               (or (and (fboundp 'projectile-project-root)
                                        (ignore-errors (projectile-project-root)))
                                   default-directory))
            :stopAtEntry     nil
            :serverReadyAction
            (list :action    "openExternally"
                  :pattern   "\\bNow listening on:\\s+(https?://\\S+)")))
    (dap-register-debug-template "C# :: dotnet :: test"
      (list :type    "coreclr"
            :request "launch"
            :name    "C# dotnet test"
            :program "dotnet"
            :args    ["test"]
            :cwd     (lambda ()
                       (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))))))

) ;; end csharp-enabled

(provide 'lang-csharp)
;;; lang-csharp.el ends here
