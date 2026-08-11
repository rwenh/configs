;;; completion-snippets.el --- Snippet configuration -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;;
;;; Code:

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :bind (("C-c y e" . yas-expand)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         ("C-c y i" . yas-insert-snippet))
  :init
  (setq yas-verbosity 1
        yas-snippet-dirs        (list (expand-file-name "snippets" user-emacs-directory))
        yas-triggers-in-field   t
        yas-wrap-around-region  t)
  :config
  (when (fboundp 'yas-reload-all)
    (yas-reload-all)))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (when (and (fboundp 'yas-reload-all)
             (boundp 'yasnippet-snippets-snippets-dir))
    (add-to-list 'yas-snippet-dirs yasnippet-snippets-snippets-dir t)
    (yas-reload-all)))

(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (defun emacs-ide--yasnippet-capf-setup ()
    (when (fboundp 'yasnippet-capf)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'emacs-ide--yasnippet-capf-setup)))

(provide 'completion-snippets)
;;; completion-snippets.el ends here
