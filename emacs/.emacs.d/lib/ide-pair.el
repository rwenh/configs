;;; ide-pair.el --- Context-aware wrap-or-insert -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;;
;;; Code:

(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory)))

(defun ide-pair-insert-or-wrap (delim)
  "Insert DELIM, or wrap the active region in DELIM on both sides."
  (if (use-region-p)
      (let ((beg (region-beginning)) (end (region-end)))
        (goto-char end)
        (insert delim)
        (goto-char beg)
        (insert delim)
        (goto-char (+ end (* 2 (length delim)))))
    (insert delim)))

(defun ide-pair-wrap-star ()
  "Self-insert \"*\" or wrap region in \"*...*\"."
  (interactive) (ide-pair-insert-or-wrap "*"))

(defun ide-pair-wrap-underscore ()
  "Self-insert \"_\" or wrap region in \"_..._\"."
  (interactive) (ide-pair-insert-or-wrap "_"))

(defun ide-pair-wrap-backtick ()
  "Self-insert backtick or wrap region in backtick spans."
  (interactive) (ide-pair-insert-or-wrap "`"))

(defun ide-pair-wrap-equals ()
  "Self-insert \"=\" or wrap region in \"=...=\" (Org verbatim)."
  (interactive) (ide-pair-insert-or-wrap "="))

(defun ide-pair-wrap-tilde ()
  "Self-insert \"~\" or wrap region in \"~...~\" (Org code)."
  (interactive) (ide-pair-insert-or-wrap "~"))

(defun ide-pair-setup-markdown-bindings ()
  "Bind wrap commands in `markdown-mode-map'."
  (when (boundp 'markdown-mode-map)
    (define-key markdown-mode-map (kbd "*") #'ide-pair-wrap-star)
    (define-key markdown-mode-map (kbd "_") #'ide-pair-wrap-underscore)
    (define-key markdown-mode-map (kbd "`") #'ide-pair-wrap-backtick)))

(defun ide-pair-setup-org-bindings ()
  "Bind wrap commands in `org-mode-map' (= and ~ left for smartparens)."
  (when (boundp 'org-mode-map)
    (define-key org-mode-map (kbd "*") #'ide-pair-wrap-star)
    (define-key org-mode-map (kbd "_") #'ide-pair-wrap-underscore)))

(provide 'ide-pair)
;;; ide-pair.el ends here
