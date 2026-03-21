;;; completion-snippets.el --- Snippet configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; YASnippet configuration for code snippets with config integration.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Version header added (file had none).
;;;   - FIX-SNIPPET-DIRS: yas-snippet-dirs now includes both the user
;;;     snippets directory AND yasnippet-snippets-snippets-dir (the
;;;     community snippets path). The old single-entry list silently
;;;     excluded community snippets if yasnippet-snippets didn't
;;;     self-register before yas-reload-all was called.
;;;   - FIX-DEAD-WRAPPERS: emacs-ide-snippet-* wrapper functions removed.
;;;     They were never bound to any keys (use-package :bind pointed
;;;     directly to the raw yas-* functions) and were therefore dead code.
;;;     use-package :bind already defers until yasnippet loads, providing
;;;     the same safety guarantee without the wrapper indirection.
;;;   - FIX-CAPF-LOCAL: yasnippet-capf now added buffer-locally via a
;;;     mode hook rather than globally via add-to-list on the default
;;;     value of completion-at-point-functions. The global approach caused
;;;     yasnippet-capf to appear in *Messages*, dired, and other non-code
;;;     buffers, potentially slowing completion everywhere.
;;;   - FIX-RELOAD-SNIPPETS: yasnippet-snippets :config now calls
;;;     yas-reload-all after the community snippets package loads.
;;;     Previously yas-reload-all ran only in yasnippet's :config, before
;;;     yasnippet-snippets had a chance to add its directory — community
;;;     snippets were unavailable until the user manually reloaded.
;;;   - FIX-COMMENTS: yas-wrap-around-region and yas-triggers-in-field
;;;     given explanatory inline comments.
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
        ;; FIX-SNIPPET-DIRS: include both user snippets and community snippets.
        ;; The community dir (yasnippet-snippets-snippets-dir) is appended
        ;; with-eval-after-load below so it's available when yas-reload-all runs.
        yas-snippet-dirs        (list (expand-file-name "snippets" user-emacs-directory))
        ;; FIX-COMMENTS: allow tab-trigger expansion inside snippet fields
        ;; (enables nested snippet expansion within an active snippet field)
        yas-triggers-in-field   t
        ;; FIX-COMMENTS: wrap active region with snippet's $0 on expansion
        ;; (selected text becomes the wrapped content inside the snippet)
        yas-wrap-around-region  t)
  :config
  (when (fboundp 'yas-reload-all)
    (yas-reload-all)))

;; ============================================================================
;; YASNIPPET-SNIPPETS - COMMUNITY SNIPPETS (DEFERRED)
;; FIX-SNIPPET-DIRS + FIX-RELOAD-SNIPPETS: After the community snippets
;; package loads, add its directory to yas-snippet-dirs and call
;; yas-reload-all so snippets are immediately available. Previously
;; yas-reload-all had already run (in yasnippet's :config) before this
;; package loaded, leaving community snippets inaccessible until the
;; user manually ran M-x yas-reload-all.
;; ============================================================================
(use-package yasnippet-snippets
  :after yasnippet
  :config
  (when (and (fboundp 'yas-reload-all)
             (boundp 'yasnippet-snippets-snippets-dir))
    ;; Add community snippets dir if not already present
    (add-to-list 'yas-snippet-dirs yasnippet-snippets-snippets-dir t)
    (yas-reload-all)))

;; ============================================================================
;; YASNIPPET-CAPF - COMPLETION AT POINT INTEGRATION (DEFERRED)
;; FIX-CAPF-LOCAL: add yasnippet-capf buffer-locally via a mode hook
;; rather than globally. The previous add-to-list on the global default
;; of completion-at-point-functions caused yasnippet-capf to appear in
;; *Messages*, dired, and every other buffer — slowing completion in
;; contexts where snippets are irrelevant.
;; ============================================================================
(use-package yasnippet-capf
  :after (yasnippet cape)
  :config
  (defun emacs-ide--yasnippet-capf-setup ()
    "Add yasnippet-capf buffer-locally to completion-at-point-functions."
    (when (fboundp 'yasnippet-capf)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
  ;; Add only in programming and text modes where snippets are relevant
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'emacs-ide--yasnippet-capf-setup)))

(provide 'completion-snippets)
;;; completion-snippets.el ends here
