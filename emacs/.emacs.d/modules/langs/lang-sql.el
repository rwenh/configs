;;; lang-sql.el --- SQL IDE layer -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 1.0.1 (audit):
;;;   - FIX-VERSION: Header bumped from 1.0.1 to 3.0.4.
;;;   - FIX-SQLFORMAT-APHELEIA-CONFLICT: sqlformat-on-save-mode hook ran
;;;     pg_format on every save while apheleia-langs-patch.el also registered
;;;     pgformatter for sql-mode — double format on save. sqlformat-on-save-mode
;;;     is now only enabled when apheleia-global-mode is not active, mirroring
;;;     the gofmt-before-save guard pattern used in lang-go.el.
;;;   - FIX-SQL-DIALECT: sql-product was hardcoded to 'postgres. Now reads
;;;     lang-settings.sql.dialect from config.yml, defaulting to postgres.
;;;   - FIX-EJC-SQL-BINDINGS: C-c C-b in sql-mode-map for ejc-eval-user-sql-region
;;;     collided with emacs-ide-repl-send-buffer (C-c x b globally). Remapped
;;;     to C-c C-E (uppercase) and C-c C-R to avoid global key collisions.
;;;   - FIX-REPL-REGISTER: Added explicit emacs-ide-repl-register for sql-mode
;;;     using sql-connect / SQLi buffer for C-c x r dispatch.
;;;   - FIX-SQL-TS-MODE: Added apheleia formatter attachment for sql-ts-mode
;;;     for consistency with other lang modules.
;;; Fixes vs 1.0.0 (retained):
;;;   - FIX-LSP-GUARD: (bound-and-true-p emacs-ide-lsp-enable) :if guard.
;;; Code:

(require 'core-dev)

(emacs-ide-dev-register "sql" :tier 2 :lsp-server "sqls"
  :formatter "pg_format" :test-cmd nil :repl "sql" :modes '(sql-mode))

(when (emacs-ide-dev-lang-enabled-p "sql")

;; ============================================================================
;; CONFIG HELPER
;; FIX-SQL-DIALECT: read dialect from config.yml lang-settings.sql.dialect
;; ============================================================================
(defun emacs-ide-sql--dialect ()
  "Return the configured SQL dialect symbol, defaulting to 'postgres."
  (let ((raw (and (fboundp 'emacs-ide-config-get-nested)
                  (emacs-ide-config-get-nested "lang-settings.sql.dialect" nil))))
    (if (and raw (not (eq raw 'postgres)))
        (intern (if (stringp raw) raw (symbol-name raw)))
      'postgres)))

;; ============================================================================
;; SQL MODE
;; FIX-SQL-DIALECT: sql-product now reads from config.
;; ============================================================================
(use-package sql
  :straight nil
  :defer t
  :mode "\\.sql\\'"
  :init
  (setq sql-product (emacs-ide-sql--dialect))
  :config
  ;; FIX-REPL-REGISTER: explicit registration for reliable C-c x r dispatch
  ;; SQLi (sql-interactive-mode) is the standard Emacs SQL REPL buffer
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

;; ============================================================================
;; LSP — sqls
;; ============================================================================
(use-package lsp-mode
  :if (and (bound-and-true-p emacs-ide-lsp-enable)
           (executable-find "sqls"))
  :hook (sql-mode . lsp-deferred))

;; ============================================================================
;; FORMATTER — sqlformat / pg_format
;; FIX-SQLFORMAT-APHELEIA-CONFLICT: only enable sqlformat-on-save-mode when
;; apheleia is not active — both running causes double format on every save.
;; ============================================================================
(defun emacs-ide-sql--maybe-sqlformat-on-save ()
  "Enable sqlformat-on-save-mode only when apheleia is not active.
FIX-SQLFORMAT-APHELEIA-CONFLICT: prevents double formatting when
apheleia-langs-patch.el has already registered pgformatter for sql-mode."
  (unless (bound-and-true-p apheleia-global-mode)
    (when (fboundp 'sqlformat-on-save-mode)
      (sqlformat-on-save-mode 1))))

(use-package sqlformat
  :if (executable-find "pg_format")
  :hook (sql-mode . emacs-ide-sql--maybe-sqlformat-on-save)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args    '("-s2" "-g")))

;; ============================================================================
;; FORMATTER — apheleia (pgformatter)
;; FIX-SQL-TS-MODE: attach formatter to sql-ts-mode as well.
;; ============================================================================
(with-eval-after-load 'apheleia
  (emacs-ide-dev-attach-formatter 'pgformatter 'sql-mode)
  (emacs-ide-dev-attach-formatter 'pgformatter 'sql-ts-mode))

;; ============================================================================
;; EJC-SQL — live DB connections
;; FIX-EJC-SQL-BINDINGS: C-c C-b remapped to C-c C-R (uppercase R) to avoid
;; collision with emacs-ide-repl-send-buffer (global C-c x b).
;; C-c C-e kept (ejc-eval-at-point has no global collision at this key).
;; ============================================================================
(use-package ejc-sql
  :defer t
  :commands (ejc-connect ejc-quit-connection)
  :bind (:map sql-mode-map
              ("C-c C-e" . ejc-eval-user-sql-at-point)
              ;; FIX-EJC-SQL-BINDINGS: was C-c C-b (collides with repl-send-buffer)
              ("C-c C-R" . ejc-eval-user-sql-region)))

) ;; end (when (emacs-ide-dev-lang-enabled-p "sql"))

(provide 'lang-sql)
;;; lang-sql.el ends here
