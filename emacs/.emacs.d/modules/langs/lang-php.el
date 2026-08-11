;;; lang-php.el --- PHP IDE layer -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;;
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "php"
  :tier 2
  :lsp-server "intelephense"
  :formatter  "php-cs-fixer"
  :test-cmd   "vendor/bin/phpunit"
  :repl       "psysh"
  :modes      '(php-mode php-ts-mode))

(unless (emacs-ide-dev-lang-enabled-p "php")
  (provide 'lang-php)
  (message "lang-php: disabled in config.yml"))

(when (emacs-ide-dev-lang-enabled-p "php")

;;;; ── Run helpers ─────────────────────────────────────────────────────────────

(defun emacs-ide-php-run ()
  "Run the current PHP file."
  (interactive)
  (if (executable-find "php")
      (compile (format "php %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-php: php not found on PATH")))

;;;; ── Test helpers ────────────────────────────────────────────────────────────

(defun emacs-ide-php-test-file ()
  "Run PHPUnit or Pest on the current file."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   default-directory))
         (phpunit (expand-file-name "vendor/bin/phpunit" root))
         (pest    (expand-file-name "vendor/bin/pest"    root)))
    (cond
     ((and (file-executable-p pest) (buffer-file-name))
      (compile (format "./vendor/bin/pest %s"
                       (shell-quote-argument (buffer-file-name)))))
     ((and (file-executable-p phpunit) (buffer-file-name))
      (compile (format "./vendor/bin/phpunit %s"
                       (shell-quote-argument (buffer-file-name)))))
     (t (message "lang-php: phpunit/pest not found in vendor/bin/")))))

(defun emacs-ide-php-test-project ()
  "Run all PHP project tests."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root)
                        (ignore-errors (projectile-project-root)))
                   default-directory))
         (pest    (expand-file-name "vendor/bin/pest"    root))
         (phpunit (expand-file-name "vendor/bin/phpunit" root)))
    (cond
     ((file-executable-p pest)    (compile "./vendor/bin/pest"))
     ((file-executable-p phpunit) (compile "./vendor/bin/phpunit"))
     (t (message "lang-php: no test runner found in vendor/bin/")))))

;;;; ── REPL (PsySH) ───────────────────────────────────────────────────────────

(defun emacs-ide-php-repl ()
  "Open a PHP REPL using PsySH."
  (interactive)
  (cond
   ((executable-find "psysh")
    (require 'comint)
    (make-comint "php-repl" "psysh")
    (switch-to-buffer "*php-repl*"))
   ((executable-find "php")
    (require 'comint)
    (make-comint "php-repl" "php" nil "-a")
    (switch-to-buffer "*php-repl*"))
   (t (message "lang-php: psysh/php not found on PATH"))))

;;;; ── php-mode ────────────────────────────────────────────────────────────────

(use-package php-mode
  :if (executable-find "php")
  :defer t
  :mode (("\\.php\\'"    . php-mode)
         ("\\.phtml\\'"  . php-mode)
         ("\\.php[34]\\'" . php-mode))
  :init
  (setq php-mode-coding-style 'psr2)
  :config
  (emacs-ide-dev-bind-compile php-mode-map #'emacs-ide-php-run)
  (define-key php-mode-map (kbd "C-c C-z") #'emacs-ide-php-repl)
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode '(php-mode php-ts-mode))
      (emacs-ide-repl-register mode
        :launch         #'emacs-ide-php-repl
        :buffer-name    "*php-repl*"
        :send-region-fn nil))))

;;;; ── LSP (intelephense or phpactor) ─────────────────────────────────────────

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((php-mode php-ts-mode) . lsp-deferred)
  :config
  (when (boundp 'lsp-intelephense-licence-key)
    nil))

;;;; ── Formatter ───────────────────────────────────────────────────────────────

(with-eval-after-load 'apheleia
  (cond
   ((executable-find "php-cs-fixer")
    (unless (assq 'php-cs-fixer apheleia-formatters)
      (push '(php-cs-fixer "php-cs-fixer" "fix" "--quiet" filepath)
            apheleia-formatters))
    (setf (alist-get 'php-mode    apheleia-mode-alist) 'php-cs-fixer)
    (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'php-cs-fixer))
   ((executable-find "phpcbf")
    (setf (alist-get 'php-mode    apheleia-mode-alist) 'phpcbf)
    (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'phpcbf))))

;;;; ── Composer integration ────────────────────────────────────────────────────

(use-package composer
  :after php-mode
  :defer t
  :commands (composer-install composer-update composer-require)
  :bind (:map php-mode-map
              ("C-c p i" . composer-install)
              ("C-c p u" . composer-update)
              ("C-c p r" . composer-require)))

;;;; ── Test runner registration ────────────────────────────────────────────────

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(php-mode php-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-php-test-file
        :project-fn #'emacs-ide-php-test-project))))

;;;; ── DAP ─────────────────────────────────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (when (executable-find "php")
    (require 'dap-php nil t)
    (when (fboundp 'dap-register-debug-template)
      (dap-register-debug-template "PHP :: XDebug"
        (list :type    "php"
              :request "launch"
              :name    "PHP file"
              :program (lambda () (buffer-file-name))
              :cwd     (lambda ()
                         (or (and (fboundp 'projectile-project-root)
                                  (ignore-errors (projectile-project-root)))
                             default-directory)))))))

) ;; end php-enabled

(provide 'lang-php)
;;; lang-php.el ends here
