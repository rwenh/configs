;;; tools-sql.el --- SQL Integration -*- lexical-binding: t -*-
;;; Commentary:
;;; SQL mode with connection profiles, sqls LSP, and an interactive query runner.
;;; Supports PostgreSQL, MySQL, SQLite, MSSQL via sql-mode connection profiles
;;; stored in config — no passwords in init files (uses auth-source).
;;;
;;; Usage:
;;;   M-x emacs-ide-sql-connect       — pick a named connection profile
;;;   C-c s s                          — send region/buffer to SQL process
;;;   C-c s e                          — explain query at point
;;;   C-c s f                          — format SQL buffer via sqlfluff
;;;
;;; Add "tools-sql" to emacs-ide-feature-modules in init.el (after lang-core).
;;; Version: 1.0.0
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; SQL-MODE (BUILT-IN) — with sane defaults
;; ============================================================================
(use-package sql
  :straight nil
  :mode (("\\.sql\\'" . sql-mode)
         ("\\.psql\\'" . sql-mode))
  :init
  (setq sql-postgres-login-params
        '(user password server database port)
        sql-mysql-login-params
        '(user password server database port)
        sql-sqlite-login-params
        '(database)
        sql-product 'postgres          ; default product
        sql-send-terminator t
        sql-pop-to-buffer-after-send-region nil
        sql-display-sqli-buffer-function #'display-buffer)
  :config
  ;; Pretty-print SQL output in the process buffer
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq-local truncate-lines t)
              (toggle-truncate-lines 1)))
  ;; Highlight SQL keywords in .sql files
  (add-hook 'sql-mode-hook #'sql-highlight-postgres-keywords))

;; ============================================================================
;; SQLS LSP SERVER — intelligent SQL completion and diagnostics
;; ============================================================================
(with-eval-after-load 'lsp-mode
  (when (executable-find "sqls")
    ;; sqls reads connection config from ~/.config/sqls/config.yml
    ;; See: https://github.com/sqls-server/sqls#connection-config
    (add-hook 'sql-mode-hook
              (lambda ()
                (when (and (boundp 'emacs-ide-lsp-enable)
                           emacs-ide-lsp-enable
                           (executable-find "sqls"))
                  (lsp-deferred))))))

;; ============================================================================
;; CONNECTION PROFILES — named connections stored as alist
;; Add your own in config.yml (future: parse from config) or customize here.
;; Passwords come from auth-source (~/.authinfo.gpg), not stored in code.
;; ============================================================================
(defvar emacs-ide-sql-connections
  '(("local-pg"
     (product . postgres)
     (server  . "localhost")
     (port    . 5432)
     (database . "postgres")
     (user    . "postgres"))
    ("local-sqlite"
     (product . sqlite)
     (database . ":memory:")))
  "Named SQL connection profiles.
Passwords are NOT stored here — they come from auth-source (~/.authinfo.gpg).
Format: (NAME (product . PRODUCT) (server . HOST) (port . PORT)
              (database . DB) (user . USER))")

(defun emacs-ide-sql--get-password (user server)
  "Fetch password for USER@SERVER from auth-source, prompting if absent."
  (let ((found (car (auth-source-search :host server :user user :max 1))))
    (if found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret) (funcall secret) secret))
      (read-passwd (format "Password for %s@%s: " user server)))))

(defun emacs-ide-sql-connect (name)
  "Connect to named SQL connection NAME from `emacs-ide-sql-connections'."
  (interactive
   (list (completing-read "Connection: "
                          (mapcar #'car emacs-ide-sql-connections)
                          nil t)))
  (let* ((profile (cdr (assoc name emacs-ide-sql-connections)))
         (product  (or (cdr (assoc 'product profile)) 'postgres))
         (server   (cdr (assoc 'server profile)))
         (port     (cdr (assoc 'port profile)))
         (database (cdr (assoc 'database profile)))
         (user     (cdr (assoc 'user profile)))
         (password (when (and user server)
                     (emacs-ide-sql--get-password user server))))
    (setq sql-product product)
    (when server   (setq sql-server server))
    (when port     (setq sql-port port))
    (when database (setq sql-database database))
    (when user     (setq sql-user user))
    (when password (setq sql-password password))
    (sql-connect name)
    (message "✓ Connected to %s (%s)" name product)))

(defun emacs-ide-sql-connect-add (name product server database user)
  "Add a new connection profile NAME interactively."
  (interactive
   (list (read-string "Connection name: ")
         (intern (completing-read "Product: " '("postgres" "mysql" "sqlite" "mssql") nil t))
         (read-string "Server: " "localhost")
         (read-string "Database: ")
         (read-string "User: ")))
  (add-to-list 'emacs-ide-sql-connections
               (list name
                     (cons 'product product)
                     (cons 'server server)
                     (cons 'database database)
                     (cons 'user user)))
  (message "✓ Added connection profile: %s" name))

;; ============================================================================
;; SQL UTILITIES
;; ============================================================================
(defun emacs-ide-sql-send-buffer ()
  "Send the entire buffer to the SQL process."
  (interactive)
  (if (fboundp 'sql-send-buffer)
      (sql-send-buffer)
    (message "⚠️  No SQL process — use M-x emacs-ide-sql-connect first")))

(defun emacs-ide-sql-send-region-or-paragraph ()
  "Send the active region, or the current paragraph, to the SQL process."
  (interactive)
  (if (region-active-p)
      (if (fboundp 'sql-send-region)
          (sql-send-region (region-beginning) (region-end))
        (message "⚠️  No SQL process"))
    ;; No region: send current statement (from blank line to blank line)
    (save-excursion
      (let ((start (progn (backward-paragraph) (point)))
            (end   (progn (forward-paragraph) (point))))
        (if (fboundp 'sql-send-region)
            (sql-send-region start end)
          (message "⚠️  No SQL process"))))))

(defun emacs-ide-sql-explain ()
  "Prepend EXPLAIN to the region/statement and send to SQL process."
  (interactive)
  (let* ((start (if (region-active-p) (region-beginning)
                  (save-excursion (backward-paragraph) (point))))
         (end   (if (region-active-p) (region-end)
                  (save-excursion (forward-paragraph) (point))))
         (stmt  (buffer-substring-no-properties start end))
         (explained (concat "EXPLAIN ANALYZE\n" stmt)))
    (if (fboundp 'sql-send-string)
        (sql-send-string explained)
      (message "⚠️  No SQL process — use M-x emacs-ide-sql-connect first"))))

(defun emacs-ide-sql-format ()
  "Format SQL buffer using sqlfluff (if available) or sql-indent."
  (interactive)
  (cond
   ((executable-find "sqlfluff")
    (when buffer-file-name
      (shell-command
       (format "sqlfluff fix --dialect %s %s"
               (if (eq sql-product 'postgres) "ansi" (symbol-name sql-product))
               (shell-quote-argument buffer-file-name)))
      (revert-buffer nil t t)
      (message "✓ Formatted with sqlfluff")))
   ((fboundp 'sql-indent-buffer)
    (sql-indent-buffer)
    (message "✓ Formatted with sql-indent"))
   (t
    (message "⚠️  Install sqlfluff for SQL formatting: pip install sqlfluff"))))

;; ============================================================================
;; SQL-INDENT — basic indentation
;; ============================================================================
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================
(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c s s") 'emacs-ide-sql-send-region-or-paragraph)
  (define-key sql-mode-map (kbd "C-c s b") 'emacs-ide-sql-send-buffer)
  (define-key sql-mode-map (kbd "C-c s e") 'emacs-ide-sql-explain)
  (define-key sql-mode-map (kbd "C-c s f") 'emacs-ide-sql-format)
  (define-key sql-mode-map (kbd "C-c s c") 'emacs-ide-sql-connect))

(global-set-key (kbd "C-c s c") 'emacs-ide-sql-connect)

(provide 'tools-sql)
;;; tools-sql.el ends here
