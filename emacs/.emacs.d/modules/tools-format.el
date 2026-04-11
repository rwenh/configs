;;; tools-format.el --- Code Formatting with Apheleia -*- lexical-binding: t -*-
;;; Commentary:
;;; Unified code formatting using apheleia, supporting 40+ languages.
;;; Version: 3.0.4
;;; Part of Enterprise Emacs IDE v3.0.4
;;; Fixes vs 3.0.4 (audit):
;;;   - FIX-VERSION: Header bumped from 2.2.5 to 3.0.4.
;;;   - FIX-TAPLO: TOML formatter 'taplo' added to apheleia-formatters.
;;;     Previously TOML files could not be formatted on save because taplo
;;;     was missing from the allowlist.
;;;   - FIX-APHELEIA-LANGS-PATCH-REMOVED: Removed dependency on non-existent
;;;     apheleia-langs-patch module. Formatter registration now happens
;;;     directly in :config block after apheleia loads.
;;; Code:

(require 'cl-lib)

;; ============================================================================
;; APHELEIA — unified code formatting
;; ============================================================================
(use-package apheleia
  :defer t
  :config
  ;; FIX-TAPLO: Added taplo (TOML formatter) to apheleia-formatters
  ;; Now done in :config so apheleia-formatters alist is guaranteed to exist
  (when (boundp 'apheleia-formatters)
    (when (executable-find "taplo")
      (setf (alist-get 'toml-mode apheleia-formatters)
            '("taplo" "fmt" "-")))
    
    ;; Add other common formatters if available
    (when (executable-find "prettier")
      (setf (alist-get 'json-mode apheleia-formatters)
            '("prettier" "--parser" "json"))
      (setf (alist-get 'yaml-mode apheleia-formatters)
            '("prettier" "--parser" "yaml")))
    
    (when (executable-find "black")
      (setf (alist-get 'python-mode apheleia-formatters)
            '("black" "-")))
    
    (when (executable-find "rustfmt")
      (setf (alist-get 'rust-mode apheleia-formatters)
            '("rustfmt" "--edition" "2021"))))
  
  ;; Format on save when enabled
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (and (fboundp 'emacs-ide-config-get)
                         (emacs-ide-config-get 'formatting 'on-save t)
                         (fboundp 'apheleia-mode))
                (apheleia-mode 1))))
  
  (add-hook 'text-mode-hook
            (lambda ()
              (when (and (fboundp 'emacs-ide-config-get)
                         (emacs-ide-config-get 'formatting 'on-save t)
                         (fboundp 'apheleia-mode)
                         (not (derived-mode-p 'org-mode)))  ;; exclude org-mode
                (apheleia-mode 1)))))

(provide 'tools-format)
;;; tools-format.el ends here