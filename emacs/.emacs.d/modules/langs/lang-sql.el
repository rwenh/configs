;;; lang-sql.el --- SQL IDE layer -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;; Code:
(require 'core-dev)
(emacs-ide-dev-register "sql" :tier 2 :lsp-server "sqls"
  :formatter "pg_format" :test-cmd nil :repl "sql" :modes '(sql-mode))
(when (emacs-ide-dev-lang-enabled-p "sql")
(use-package sql :straight nil :defer t
  :mode "\\.sql\\'"
  :init (setq sql-product 'postgres))
(use-package lsp-mode :if (executable-find "sqls")
  :hook (sql-mode . lsp-deferred))
(use-package sqlformat :if (executable-find "pg_format")
  :hook (sql-mode . sqlformat-on-save-mode)
  :init (setq sqlformat-command 'pgformatter
              sqlformat-args '("-s2" "-g")))
;; ejc-sql for live DB connections
(use-package ejc-sql
  :defer t :commands (ejc-connect ejc-quit-connection)
  :bind (:map sql-mode-map
              ("C-c C-e" . ejc-eval-user-sql-at-point)
              ("C-c C-b" . ejc-eval-user-sql-region)))
) (provide 'lang-sql)
;;; lang-sql.el ends here
