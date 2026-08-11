;;; tools-database.el --- Database Client -*- lexical-binding: t -*-
;;;
;;; Version: 3.5.0
;;; Code:

(require 'cl-lib)
(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory) t))

;;;; ── Config helpers ──────────────────────────────────────────────────────────

(defun emacs-ide-db--cfg (key &optional default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'database key default)
    default))

;;;; ── Connection registry ─────────────────────────────────────────────────────

(defvar emacs-ide-db--connections nil
  "Alist of (NAME . plist) where plist has :type :host :port :dbname :user.")

(defvar emacs-ide-db--active-connection nil
  "Name of the currently active database connection, or nil.")

(defun emacs-ide-db--load-connections-from-config ()
  "Populate `emacs-ide-db--connections' from config.yml database.connections."
  (when (boundp 'emacs-ide-config-data)
    (let ((conns (and (fboundp 'emacs-ide-config-get)
                      (emacs-ide-config-get 'database 'connections nil))))
      (when (and conns (listp conns))
        (setq emacs-ide-db--connections
              (mapcar (lambda (entry)
                        (cons (symbol-name (car entry)) (cdr entry)))
                      conns))))))

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-db--load-connections-from-config))

(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-db--load-connections-from-config)

;;;; ── Query history ───────────────────────────────────────────────────────────

(defvar emacs-ide-db--query-history nil
  "Ring of the last 100 SQL queries run via this interface.")

(defconst emacs-ide-db--history-max 100)

(defun emacs-ide-db--record-query (sql)
  "Add SQL to the query history ring."
  (push (cons (format-time-string "%Y-%m-%d %H:%M:%S") sql)
        emacs-ide-db--query-history)
  (when (> (length emacs-ide-db--query-history) emacs-ide-db--history-max)
    (setq emacs-ide-db--query-history
          (seq-take emacs-ide-db--query-history emacs-ide-db--history-max))))

;;;; ── Query bookmarks ─────────────────────────────────────────────────────────

(defvar emacs-ide-db--bookmarks nil
  "Alist of (NAME . SQL) for named query bookmarks.")

;;;; ── Core connection functions ───────────────────────────────────────────────

(defun emacs-ide-db--connection-names ()
  "Return list of configured connection names, or nil."
  (mapcar #'car emacs-ide-db--connections))

(defun emacs-ide-db--get-connection (name)
  "Return the plist for connection NAME, or nil."
  (cdr (assoc name emacs-ide-db--connections)))

(defun emacs-ide-db-connect (&optional name)
  "Connect to a named database.  Prompts if NAME is not given."
  (interactive)
  (let* ((names (emacs-ide-db--connection-names))
         (name  (or name
                    (and names
                         (completing-read "Database: " names nil t))
                    (read-string "Connection name: "))))
    (setq emacs-ide-db--active-connection name)
    (let ((conn (emacs-ide-db--get-connection name)))
      (if (null conn)
          (message "tools-database: connecting to %s (no config; use sql-connect)" name)
        (let* ((type  (or (plist-get conn :type)  "postgres"))
               (host  (or (plist-get conn :host)  "localhost"))
               (port  (or (plist-get conn :port)  5432))
               (db    (or (plist-get conn :dbname) name))
               (user  (or (plist-get conn :user)   (user-login-name))))
          (cond
           ((string-match-p "postgres\\|pg" type)
            (when (fboundp 'sql-postgres)
              (let ((sql-postgres-login-params nil)
                    (sql-server host)
                    (sql-port (if (numberp port) port (string-to-number (format "%s" port))))
                    (sql-database db)
                    (sql-user user))
                (sql-postgres db))))
           ((string-match-p "mysql\\|mariadb" type)
            (when (fboundp 'sql-mysql)
              (sql-mysql db)))
           ((string-match-p "sqlite" type)
            (when (fboundp 'sql-sqlite)
              (sql-sqlite db)))
           (t (sql-connect (intern name)))))
        (message "Connected to %s" name)))))

(defun emacs-ide-db-disconnect ()
  "Disconnect from the active database session."
  (interactive)
  (if emacs-ide-db--active-connection
      (progn
        (setq emacs-ide-db--active-connection nil)
        (when (and (get-buffer "*SQL*") (fboundp 'sql-send-string))
          (kill-buffer "*SQL*"))
        (message "Disconnected"))
    (message "tools-database: no active connection")))

(defun emacs-ide-db-run-query (sql)
  "Run SQL string and display results."
  (interactive "sSQL: ")
  (when (string-blank-p sql)
    (user-error "Empty query"))
  (emacs-ide-db--record-query sql)
  (if (fboundp 'sql-send-string)
      (sql-send-string sql)
    (message "tools-database: open a SQL buffer first with emacs-ide-db-connect")))

;;;; ── Schema browser ──────────────────────────────────────────────────────────

(defun emacs-ide-db--schema-query (db-type)
  "Return an information_schema query appropriate for DB-TYPE."
  (cond
   ((string-match-p "postgres\\|pg" db-type)
    "SELECT table_schema, table_name, table_type FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog','information_schema') ORDER BY table_schema, table_name;")
   ((string-match-p "mysql\\|mariadb" db-type)
    "SELECT table_schema, table_name, table_type FROM information_schema.tables WHERE table_schema NOT IN ('mysql','information_schema','performance_schema','sys') ORDER BY table_schema, table_name;")
   ((string-match-p "sqlite" db-type)
    "SELECT name, type FROM sqlite_master WHERE type IN ('table','view') ORDER BY name;")
   (t
    "SELECT table_schema, table_name, table_type FROM information_schema.tables ORDER BY table_schema, table_name;")))

(defun emacs-ide-db-browse-schema ()
  "Browse the schema of the active database connection."
  (interactive)
  (if (null emacs-ide-db--active-connection)
      (message "tools-database: connect first with emacs-ide-db-connect")
    (let* ((conn   (emacs-ide-db--get-connection emacs-ide-db--active-connection))
           (dbtype (or (and conn (plist-get conn :type)) "postgres"))
           (sql    (emacs-ide-db--schema-query dbtype)))
      (emacs-ide-db-run-query sql)
      (message "Schema query sent for %s (%s)"
               emacs-ide-db--active-connection dbtype))))

(defun emacs-ide-db-describe-table (table)
  "Describe TABLE columns using information_schema."
  (interactive "sTable name: ")
  (when (string-blank-p table)
    (user-error "Table name cannot be empty"))
  (let* ((conn   (and emacs-ide-db--active-connection
                      (emacs-ide-db--get-connection emacs-ide-db--active-connection)))
         (dbtype (or (and conn (plist-get conn :type)) "postgres"))
         (sql
          (cond
           ((string-match-p "postgres\\|pg" dbtype)
            (format "SELECT column_name, data_type, is_nullable, column_default FROM information_schema.columns WHERE table_name = '%s' ORDER BY ordinal_position;" table))
           ((string-match-p "mysql\\|mariadb" dbtype)
            (format "DESCRIBE %s;" table))
           ((string-match-p "sqlite" dbtype)
            (format "PRAGMA table_info(%s);" table))
           (t
            (format "SELECT column_name, data_type FROM information_schema.columns WHERE table_name = '%s';" table)))))
    (emacs-ide-db-run-query sql)))

;;;; ── Query history UI ────────────────────────────────────────────────────────

(defun emacs-ide-db-query-history ()
  "Browse and re-run queries from history."
  (interactive)
  (if (null emacs-ide-db--query-history)
      (message "No query history yet")
    (let* ((choices (mapcar (lambda (entry)
                               (format "[%s] %s"
                                       (car entry)
                                       (truncate-string-to-width (cdr entry) 60 nil nil "…")))
                             emacs-ide-db--query-history))
           (pick (completing-read "Query history: " choices nil t)))
      (when pick
        (let* ((idx (cl-position pick choices :test #'string=))
               (sql (cdr (nth idx emacs-ide-db--query-history))))
          (emacs-ide-db-run-query sql))))))

;;;; ── Query bookmarks ─────────────────────────────────────────────────────────

(defun emacs-ide-db-run-bookmark ()
  "Select and run a named query bookmark."
  (interactive)
  (if (null emacs-ide-db--bookmarks)
      (message "No bookmarks.  Use M-x emacs-ide-db-add-bookmark to add one.")
    (let* ((name (completing-read "Bookmark: "
                                   (mapcar #'car emacs-ide-db--bookmarks) nil t))
           (sql  (cdr (assoc name emacs-ide-db--bookmarks))))
      (when sql (emacs-ide-db-run-query sql)))))

(defun emacs-ide-db-add-bookmark (name sql)
  "Save SQL as a named query bookmark."
  (interactive "sBookmark name: \nsSQL: ")
  (setf (alist-get name emacs-ide-db--bookmarks nil nil #'string=) sql)
  (message "Bookmark saved: %s" name))

;;;; ── Interactive SELECT builder ─────────────────────────────────────────────

(defun emacs-ide-db-select-builder ()
  "Interactively build and run a SELECT query."
  (interactive)
  (let* ((table  (read-string "Table: "))
         (cols   (read-string "Columns (* for all): " "*"))
         (where  (read-string "WHERE clause (blank = none): "))
         (limit  (read-string "LIMIT (blank = none): " "100"))
         (sql    (concat "SELECT " cols " FROM " table
                          (unless (string-blank-p where) (concat " WHERE " where))
                          (unless (string-blank-p limit) (concat " LIMIT " limit))
                          ";")))
    (when (y-or-n-p (format "Run: %s ? " sql))
      (emacs-ide-db-run-query sql))))

;;;; ── Add connection wizard ───────────────────────────────────────────────────

(defun emacs-ide-db-add-connection (name type host dbname user)
  "Register a new database connection (session-only; add to config.yml to persist)."
  (interactive
   (list (read-string "Connection name: ")
         (completing-read "Type: " '("postgres" "mysql" "sqlite" "mssql") nil t)
         (read-string "Host: " "localhost")
         (read-string "Database name: ")
         (read-string "User: " (user-login-name))))
  (push (cons name (list :type type :host host :dbname dbname :user user))
        emacs-ide-db--connections)
  (message "Connection '%s' registered (session-only; add to config.yml to persist)" name))

;;;; ── Status ──────────────────────────────────────────────────────────────────

(defun emacs-ide-db-status ()
  "Display database connection status."
  (interactive)
  (with-output-to-temp-buffer "*Database Status*"
    (princ "=== DATABASE STATUS ===\n\n")
    (princ (format "Active connection: %s\n"
                   (or emacs-ide-db--active-connection "none")))
    (princ (format "Configured connections: %d\n\n"
                   (length emacs-ide-db--connections)))
    (dolist (conn emacs-ide-db--connections)
      (let* ((name  (car conn))
             (plist (cdr conn))
             (type  (or (plist-get plist :type) "?"))
             (host  (or (plist-get plist :host) "?"))
             (db    (or (plist-get plist :dbname) "?")))
        (princ (format "  %s %-16s  %s@%s/%s\n"
                       (if (equal name emacs-ide-db--active-connection) "→" " ")
                       name type (plist-get plist :user) db))))
    (princ (format "\nQuery history:  %d entries\n" (length emacs-ide-db--query-history)))
    (princ (format "Bookmarks:      %d\n" (length emacs-ide-db--bookmarks)))
    (princ "\nCommands:\n")
    (princ "  M-x emacs-ide-db-connect         C-c d c\n")
    (princ "  M-x emacs-ide-db-browse-schema   C-c d d\n")
    (princ "  M-x emacs-ide-db-query-history   C-c d h\n")
    (princ "  M-x emacs-ide-db-select-builder  C-c d q\n")))

;;;; ── Hydra ───────────────────────────────────────────────────────────────────

(with-eval-after-load 'hydra
  (defhydra hydra-database (:hint nil :color blue)
    "
  database
  ──────────────────────────────────────────────
  _c_ connect       _d_ schema browser   _t_ describe table
  _q_ SELECT builder _h_ query history   _b_ run bookmark
  _a_ add connection _s_ status          _x_ disconnect
  ──────────────────────────────────────────────
  _ESC_ quit
"
    ("c" emacs-ide-db-connect)
    ("d" emacs-ide-db-browse-schema)
    ("t" (call-interactively #'emacs-ide-db-describe-table))
    ("q" emacs-ide-db-select-builder)
    ("h" emacs-ide-db-query-history)
    ("b" emacs-ide-db-run-bookmark)
    ("a" (call-interactively #'emacs-ide-db-add-connection))
    ("s" emacs-ide-db-status)
    ("x" emacs-ide-db-disconnect)
    ("ESC" nil)))

;;;; ── Keybindings ─────────────────────────────────────────────────────────────

(define-prefix-command 'emacs-ide-db-map)
(global-set-key (kbd "C-c d") 'emacs-ide-db-map)
(global-set-key (kbd "C-c d c") #'emacs-ide-db-connect)
(global-set-key (kbd "C-c d s") #'emacs-ide-db-status)
(global-set-key (kbd "C-c d d") #'emacs-ide-db-browse-schema)
(global-set-key (kbd "C-c d t") #'emacs-ide-db-describe-table)
(global-set-key (kbd "C-c d h") #'emacs-ide-db-query-history)
(global-set-key (kbd "C-c d b") #'emacs-ide-db-run-bookmark)
(global-set-key (kbd "C-c d q") #'emacs-ide-db-select-builder)
(global-set-key (kbd "C-c d a") #'emacs-ide-db-add-connection)
(global-set-key (kbd "C-c h D")
  (lambda () (interactive)
    (if (fboundp 'hydra-database/body)
        (hydra-database/body)
      (message "hydra-database not loaded"))))

(provide 'tools-database)
;;; tools-database.el ends here
