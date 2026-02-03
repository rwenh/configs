;;; completion-snippets.el --- Snippet configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; YASnippet configuration for code snippets
;;; Code:

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :bind (("C-c y e" . yas-expand)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         ("C-c y i" . yas-insert-snippet))
  :init
  (setq yas-verbosity 1
        yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
        yas-triggers-in-field t
        yas-wrap-around-region t)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'completion-snippets)
;;; completion-snippets.el ends here
