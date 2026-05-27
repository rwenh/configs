;;; lang-ruby.el --- Ruby / Rails IDE layer -*- lexical-binding: t -*-
;;; Version: 3.4.0
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "ruby"
  :tier 2
  :lsp-server "solargraph"
  :formatter  "rubocop"
  :test-cmd   "bundle exec rspec"
  :repl       "irb"
  :modes      '(ruby-mode ruby-ts-mode))

(unless (emacs-ide-dev-lang-enabled-p "ruby")
  (provide 'lang-ruby)
  (message "lang-ruby: disabled in config.yml"))

(when (emacs-ide-dev-lang-enabled-p "ruby")

;;;; ── Tree-sitter ─────────────────────────────────────────────────────────────

(emacs-ide-dev-ensure-treesit 'ruby)

;;;; ── Run helpers ─────────────────────────────────────────────────────────────

(defun emacs-ide-ruby-run ()
  "Run the current Ruby file."
  (interactive)
  (if (executable-find "ruby")
      (compile (format "ruby %s"
                       (shell-quote-argument (buffer-file-name))))
    (message "lang-ruby: ruby not found on PATH")))

;;;; ── Test helpers ────────────────────────────────────────────────────────────

(defun emacs-ide-ruby-test-file ()
  "Run tests for the current Ruby file (rspec preferred, then minitest)."
  (interactive)
  (cond
   ((and (executable-find "rspec") (buffer-file-name))
    (compile (format "bundle exec rspec %s --format progress"
                     (shell-quote-argument (buffer-file-name)))))
   ((and (executable-find "ruby") (buffer-file-name))
    (compile (format "ruby -Itest %s"
                     (shell-quote-argument (buffer-file-name)))))
   (t (message "lang-ruby: no test runner found"))))

(defun emacs-ide-ruby-test-project ()
  "Run all Ruby project tests."
  (interactive)
  (let ((root (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  default-directory)))
    (cond
     ((file-exists-p (expand-file-name "Gemfile" root))
      (if (executable-find "rspec")
          (compile "bundle exec rspec --format progress")
        (compile "bundle exec rake test")))
     ((executable-find "rake")
      (compile "rake test"))
     (t (message "lang-ruby: no project test runner found")))))

(defun emacs-ide-ruby-test-at-point ()
  "Run the rspec example at point."
  (interactive)
  (if (and (executable-find "rspec") (buffer-file-name))
      (let ((line (line-number-at-pos)))
        (compile (format "bundle exec rspec %s:%d"
                         (shell-quote-argument (buffer-file-name))
                         line)))
    (message "lang-ruby: rspec not found or no file")))

;;;; ── REPL (irb / pry) ────────────────────────────────────────────────────────

(defun emacs-ide-ruby-repl ()
  "Open a Ruby REPL (pry preferred, irb fallback)."
  (interactive)
  (let ((interp (or (executable-find "pry")
                    (executable-find "irb"))))
    (if interp
        (progn
          (require 'comint)
          (make-comint "ruby-repl" interp)
          (switch-to-buffer "*ruby-repl*"))
      (message "lang-ruby: irb/pry not found on PATH"))))

;;;; ── ruby-mode (built-in) ───────────────────────────────────────────────────

(use-package ruby-mode
  :straight nil
  :defer t
  :mode (("\\.rb\\'"       . ruby-mode)
         ("\\.rake\\'"     . ruby-mode)
         ("\\.gemspec\\'"  . ruby-mode)
         ("Rakefile\\'"    . ruby-mode)
         ("Gemfile\\'"     . ruby-mode)
         ("Guardfile\\'"   . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("Capfile\\'"     . ruby-mode)
         ("Fastfile\\'"    . ruby-mode))
  :init
  (setq ruby-indent-level  2
        ruby-insert-encoding-magic-comment nil)
  :config
  (emacs-ide-dev-bind-compile ruby-mode-map #'emacs-ide-ruby-run)
  (when (fboundp 'emacs-ide-repl-register)
    (dolist (mode '(ruby-mode ruby-ts-mode))
      (emacs-ide-repl-register mode
        :launch         #'emacs-ide-ruby-repl
        :buffer-name    "*ruby-repl*"
        :send-region-fn (lambda (beg end)
                          (let ((buf (get-buffer "*ruby-repl*")))
                            (if buf
                                (progn
                                  (comint-send-region buf beg end)
                                  (with-current-buffer buf
                                    (comint-send-input)))
                              (message "lang-ruby: start REPL first with C-c x r"))))))))

;;;; ── Enhanced ruby-ts-mode (Emacs 29+) ──────────────────────────────────────

(with-eval-after-load 'ruby-ts-mode
  (when (boundp 'ruby-ts-mode-map)
    (emacs-ide-dev-bind-compile ruby-ts-mode-map #'emacs-ide-ruby-run)))

;;;; ── LSP (solargraph or ruby-lsp) ──────────────────────────────────────────

(use-package lsp-mode
  :if (bound-and-true-p emacs-ide-lsp-enable)
  :hook ((ruby-mode ruby-ts-mode) . lsp-deferred)
  :config
  ;; Prefer ruby-lsp if available, fall back to solargraph
  (when (and (boundp 'lsp-disabled-clients)
             (not (executable-find "ruby-lsp"))
             (executable-find "solargraph"))
    (setq lsp-disabled-clients
          (append (bound-and-true-p lsp-disabled-clients)
                  '(ruby-lsp-ls)))))

;;;; ── Formatter (rubocop via apheleia) ───────────────────────────────────────

(with-eval-after-load 'apheleia
  (cond
   ((executable-find "standardrb")
    (setf (alist-get 'ruby-mode    apheleia-mode-alist) 'standardrb)
    (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'standardrb))
   ((executable-find "rubocop")
    (setf (alist-get 'ruby-mode    apheleia-mode-alist) 'rubocop)
    (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop))))

;;;; ── rspec-mode (optional test navigation) ─────────────────────────────────

(use-package rspec-mode
  :if (and (executable-find "rspec")
           (file-exists-p (expand-file-name "spec" default-directory)))
  :after ruby-mode
  :defer t
  :hook ((ruby-mode ruby-ts-mode) . rspec-mode))

;;;; ── bundler ─────────────────────────────────────────────────────────────────

(use-package bundler
  :after ruby-mode
  :defer t
  :commands (bundle-open bundle-install bundle-update bundle-exec bundle-console)
  :bind (:map ruby-mode-map
              ("C-c b i" . bundle-install)
              ("C-c b u" . bundle-update)
              ("C-c b o" . bundle-open)
              ("C-c b c" . bundle-console)))

;;;; ── inf-ruby (fallback REPL integration) ───────────────────────────────────

(use-package inf-ruby
  :after ruby-mode
  :defer t
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

;;;; ── Test runner registration ────────────────────────────────────────────────

(with-eval-after-load 'tools-test-runner-registry
  (when (fboundp 'emacs-ide-test-register-runner)
    (dolist (mode '(ruby-mode ruby-ts-mode))
      (emacs-ide-test-register-runner mode
        :file-fn    #'emacs-ide-ruby-test-file
        :project-fn #'emacs-ide-ruby-test-project
        :point-fn   #'emacs-ide-ruby-test-at-point))))

;;;; ── DAP (rdbg — Ruby debug gem) ───────────────────────────────────────────

(with-eval-after-load 'dap-mode
  (when (executable-find "rdbg")
    (require 'dap-ruby nil t)
    (when (fboundp 'dap-register-debug-template)
      (dap-register-debug-template "Ruby :: rdbg :: file"
        (list :type    "Ruby"
              :request "launch"
              :name    "Ruby file (rdbg)"
              :program (lambda () (buffer-file-name))
              :cwd     (lambda ()
                         (or (and (fboundp 'projectile-project-root)
                                  (ignore-errors (projectile-project-root)))
                             default-directory)))))))

) ;; end ruby-enabled

(provide 'lang-ruby)
;;; lang-ruby.el ends here
