;;; lang-sql.el --- SQL IDE layer -*- lexical-binding: t -*-
;;;
;;; Version: 3.0.4

(require 'core-dev)

(emacs-ide-dev-register "sql" :tier 2 :lsp-server "sqls"
  :formatter "pg_format" :test-cmd nil :repl "sql" :modes '(sql-mode))

(when (emacs-ide-dev-lang-enabled-p "sql")

(defun emacs-ide-sql--dialect ()
  "Return the configured SQL dialect symbol, defaulting to 'postgres.
Reads lang-settings.sql.dialect from config.yml via core-dev."
  (let* ((settings (and (fboundp 'emacs-ide-dev--config-lang-settings)
                        (emacs-ide-dev--config-lang-settings "sql")))
         (raw      (and settings (cdr (assoc 'dialect settings)))))
    (if (and raw (not (eq raw 'postgres)))
        (if (stringp raw) (intern raw) raw)
      'postgres)))

(use-package sql
  :straight nil
  :defer t
  :mode "\\.sql\\'"
  :init
  (setq sql-product (emacs-ide-sql--dialect))
  :config
  (when (fboundp 'emacs-ide-repl-register)
    (emacs-ide-repl-register 'sql-mode
      :launch         (lambda ()
                        (if (fboundp 'sql-connect)
                            (call-interactively #'sql-connect)
                          (sql-product-interactive (emacs-ide-sql--dialect))))
      :buffer-name    "*SQL*"
      :send-region-fn (lambda (beg end)
                        (when (fboundp 'sql-send-region)
                          (sql-send-region beg end))))))

(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "sqls"))
  :hook (sql-mode . lsp-deferred))

(defun emacs-ide-sql--maybe-sqlformat-on-save ()
  "Enable sqlformat-on-save-mode only when apheleia is not active."
  (unless (bound-and-true-p apheleia-global-mode)
    (when (fboundp 'sqlformat-on-save-mode)
      (sqlformat-on-save-mode 1))))

(use-package sqlformat
  :if (executable-find "pg_format")
  :hook (sql-mode . emacs-ide-sql--maybe-sqlformat-on-save)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args    '("-s2" "-g")))

(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'pgformatter 'sql-mode)
  (emacs-ide-dev-attach-formatter 'pgformatter 'sql-ts-mode))

(use-package ejc-sql
  :defer t
  :commands (ejc-connect ejc-quit-connection)
  :bind (:map sql-mode-map
              ("C-c C-e" . ejc-eval-user-sql-at-point)
              ("C-c C-R" . ejc-eval-user-sql-region)))

)

(provide 'lang-sql)
;;; lang-sql.el ends here
