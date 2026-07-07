;;; ide-search.el --- Search and occur helpers -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;;
;;; Code:

(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory)))

(defgroup ide-search nil "Search and occur helpers." :group 'convenience :prefix "ide-search-")

(defcustom ide-search-long-line-length 100
  "Threshold for `ide-search-occur-long-lines'."
  :type 'integer :group 'ide-search)

(defconst ide-search-url-regexp "https?://[^][ \t\n\r\"'<>]+"
  "Regexp matching a bare URL.")

(defun ide-search-occur-urls ()
  "Run `occur' for URLs in the current buffer."
  (interactive)
  (occur ide-search-url-regexp))

(defun ide-search-occur-long-lines (&optional length)
  "Run `occur' for lines longer than LENGTH characters."
  (interactive "P")
  (let ((len (or (and (integerp length) length)
                 (if (fboundp 'emacs-ide-config-get)
                     (emacs-ide-config-get 'analytics 'long-line-length
                                            ide-search-long-line-length)
                   ide-search-long-line-length))))
    (occur (format "^.\\{%d,\\}" len))))

(defun ide-search-occur-todo-keywords ()
  "Run `occur' for TODO-style comment keywords."
  (interactive)
  (let* ((keywords (if (bound-and-true-p hl-todo-keyword-faces)
                        (mapcar #'car hl-todo-keyword-faces)
                      '("TODO" "FIXME" "HACK" "NOTE" "XXX" "BUG")))
         (regexp (regexp-opt keywords 'words)))
    (occur regexp)))

(defun ide-search-project-grep-symbol (&optional symbol)
  "Search the project for SYMBOL via consult-ripgrep → projectile-ripgrep → rgrep."
  (interactive)
  (let ((sym (or symbol (ide-common-region-or-symbol))))
    (unless sym (user-error "No symbol at point"))
    (let ((root (ide-common-project-root)))
      (cond
       ((fboundp 'consult-ripgrep)    (consult-ripgrep root sym))
       ((fboundp 'projectile-ripgrep) (projectile-ripgrep sym))
       (t (rgrep sym "*" root))))))

(defun ide-search-isearch-other-end ()
  "Exit isearch at the opposite end of the match."
  (interactive)
  (isearch-exit)
  (when isearch-other-end
    (goto-char isearch-other-end)))

(provide 'ide-search)
;;; ide-search.el ends here
