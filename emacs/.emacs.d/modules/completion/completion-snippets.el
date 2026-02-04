;;; completion-snippets.el --- Snippet configuration (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; YASnippet configuration for code snippets with config integration
;;; Code:

;; ============================================================================
;; YASNIPPET - SNIPPET ENGINE (DEFERRED)
;; ============================================================================
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
  (when (fboundp 'yas-reload-all)
    (yas-reload-all)))

;; ============================================================================
;; YASNIPPET-SNIPPETS - COMMUNITY SNIPPETS (DEFERRED)
;; ============================================================================
(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================================
;; YASNIPPET-CAPF - COMPLETION AT POINT INTEGRATION (DEFERRED)
;; ============================================================================
(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (when (fboundp 'yasnippet-capf)
    (add-to-list 'completion-at-point-functions #'yasnippet-capf)))

;; ============================================================================
;; SNIPPET HELPER FUNCTIONS
;; ============================================================================
(defun emacs-ide-snippet-new ()
  "Create new snippet safely."
  (interactive)
  (if (fboundp 'yas-new-snippet)
      (yas-new-snippet)
    (message "⚠️  YASnippet not available")))

(defun emacs-ide-snippet-visit ()
  "Visit snippet file safely."
  (interactive)
  (if (fboundp 'yas-visit-snippet-file)
      (yas-visit-snippet-file)
    (message "⚠️  YASnippet not available")))

(defun emacs-ide-snippet-expand ()
  "Expand snippet at point safely."
  (interactive)
  (if (fboundp 'yas-expand)
      (yas-expand)
    (message "⚠️  YASnippet not available")))

(defun emacs-ide-snippet-insert ()
  "Insert snippet safely."
  (interactive)
  (if (fboundp 'yas-insert-snippet)
      (yas-insert-snippet)
    (message "⚠️  YASnippet not available")))

(provide 'completion-snippets)
;;; completion-snippets.el ends here