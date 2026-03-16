;;; tools-repl.el --- Unified REPL Hub -*- lexical-binding: t -*-
;;; Commentary:
;;; Central dispatcher for REPLs across all lang tiers.
;;; Each lang module calls (emacs-ide-dev-attach-repl mode-map fn) for its
;;; own REPL, but this file provides:
;;;   - A single C-c r dispatch that routes to the right REPL
;;;   - Window management (split, focus, toggle)
;;;   - Buffer persistence (one REPL per lang, reuse on C-c r)
;;;   - Send region/buffer commands that work across all langs
;;;
;;; Version: 1.0.0
;;; Code:

(require 'cl-lib)
(require 'comint)

;; ============================================================================
;; REPL REGISTRY
;; lang modules self-register via emacs-ide-repl-register
;; ============================================================================

(defvar emacs-ide-repl--registry nil
  "Alist of (MAJOR-MODE . plist) where plist has :launch :buffer-name :send-region-fn")

(defun emacs-ide-repl-register (mode &rest plist)
  "Register a REPL for MAJOR-MODE with PLIST keys:
  :launch         zero-arg fn that opens the REPL buffer
  :buffer-name    string name of the REPL buffer (e.g. \"*Python*\")
  :send-region-fn fn(beg end) to send region to REPL"
  (setf (alist-get mode emacs-ide-repl--registry) plist))

;; ============================================================================
;; BUILT-IN REGISTRATIONS (wired here so lang modules stay lean)
;; ============================================================================

(with-eval-after-load 'python
  (emacs-ide-repl-register 'python-mode
    :launch         (lambda () (run-python nil nil t))
    :buffer-name    "*Python*"
    :send-region-fn #'python-shell-send-region))

(with-eval-after-load 'rust-mode
  (emacs-ide-repl-register 'rust-mode
    :launch         (lambda ()
                      (if (executable-find "evcxr")
                          (progn (make-comint "rust-repl" "evcxr") (switch-to-buffer "*rust-repl*"))
                        (message "tools-repl: install evcxr: cargo install evcxr_repl")))
    :buffer-name    "*rust-repl*"
    :send-region-fn nil))

(with-eval-after-load 'js2-mode
  (emacs-ide-repl-register 'js2-mode
    :launch         (lambda ()
                      (make-comint "node-repl" "node")
                      (switch-to-buffer "*node-repl*"))
    :buffer-name    "*node-repl*"
    :send-region-fn nil))

(with-eval-after-load 'go-mode
  (emacs-ide-repl-register 'go-mode
    :launch         (lambda ()
                      (if (executable-find "gore")
                          (progn (make-comint "go-repl" "gore") (switch-to-buffer "*go-repl*"))
                        (message "tools-repl: go install github.com/x-motemen/gore/cmd/gore@latest")))
    :buffer-name    "*go-repl*"
    :send-region-fn nil))

(with-eval-after-load 'lua-mode
  (emacs-ide-repl-register 'lua-mode
    :launch         (lambda () (make-comint "lua-repl" "lua") (switch-to-buffer "*lua-repl*"))
    :buffer-name    "*lua-repl*"
    :send-region-fn nil))

(with-eval-after-load 'clojure-mode
  (emacs-ide-repl-register 'clojure-mode
    :launch         (lambda () (when (fboundp 'cider-jack-in) (cider-jack-in nil)))
    :buffer-name    "*cider-repl*"
    :send-region-fn (lambda (beg end) (when (fboundp 'cider-eval-region) (cider-eval-region beg end)))))

(with-eval-after-load 'haskell-mode
  (emacs-ide-repl-register 'haskell-mode
    :launch         (lambda () (when (fboundp 'haskell-interactive-switch) (haskell-interactive-switch)))
    :buffer-name    "*haskell*"
    :send-region-fn (lambda (beg end) (when (fboundp 'haskell-process-load-file) (haskell-process-load-file)))))

(with-eval-after-load 'ess-r-mode
  (emacs-ide-repl-register 'ess-r-mode
    :launch         (lambda () (when (fboundp 'R) (R)))
    :buffer-name    "*R*"
    :send-region-fn (lambda (beg end) (when (fboundp 'ess-eval-region) (ess-eval-region beg end nil)))))

(with-eval-after-load 'julia-mode
  (emacs-ide-repl-register 'julia-mode
    :launch         (lambda () (when (fboundp 'julia-repl) (julia-repl)))
    :buffer-name    "*julia*"
    :send-region-fn nil))

;; ============================================================================
;; DISPATCH
;; ============================================================================

(defun emacs-ide-repl-get-info ()
  "Return REPL plist for current major-mode, or nil."
  (alist-get major-mode emacs-ide-repl--registry))

(defun emacs-ide-repl-launch ()
  "Launch or switch to the REPL for the current buffer's language.
If a REPL buffer already exists, switch to it. Otherwise launch a new one."
  (interactive)
  (let* ((info   (emacs-ide-repl-get-info))
         (launch (and info (plist-get info :launch)))
         (bufname (and info (plist-get info :buffer-name))))
    (cond
     ((null info)
      (message "tools-repl: no REPL registered for %s" major-mode))
     ((and bufname (get-buffer bufname))
      ;; Buffer exists — just pop to it
      (pop-to-buffer bufname
                     '(display-buffer-reuse-window
                       (inhibit-same-window . t))))
     (launch
      (funcall launch))
     (t
      (message "tools-repl: no launch function for %s" major-mode)))))

(defun emacs-ide-repl-send-region (beg end)
  "Send region BEG..END to the REPL for the current language."
  (interactive "r")
  (let* ((info (emacs-ide-repl-get-info))
         (fn   (and info (plist-get info :send-region-fn))))
    (cond
     ((null info)  (message "tools-repl: no REPL for %s" major-mode))
     ((null fn)    (message "tools-repl: no send-region for %s" major-mode))
     (t (funcall fn beg end)
        (message "tools-repl: sent region (%d chars)" (- end beg))))))

(defun emacs-ide-repl-send-buffer ()
  "Send entire buffer to the REPL."
  (interactive)
  (emacs-ide-repl-send-region (point-min) (point-max)))

(defun emacs-ide-repl-send-defun ()
  "Send the top-level form/function at point to the REPL."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (emacs-ide-repl-send-region (car bounds) (cdr bounds))
      (message "tools-repl: no defun at point"))))

(defun emacs-ide-repl-toggle-window ()
  "Toggle visibility of the REPL window for current language."
  (interactive)
  (let* ((info    (emacs-ide-repl-get-info))
         (bufname (and info (plist-get info :buffer-name)))
         (win     (and bufname (get-buffer-window bufname))))
    (cond
     ((null bufname)
      (message "tools-repl: no REPL buffer name for %s" major-mode))
     (win
      (delete-window win))
     ((get-buffer bufname)
      (display-buffer bufname
                      '(display-buffer-in-side-window
                        (side . bottom)
                        (slot . 1)
                        (window-height . 0.35))))
     (t
      (emacs-ide-repl-launch)))))

;; ============================================================================
;; WINDOW DISPLAY RULES
;; Make REPLs always open in a bottom side window
;; ============================================================================

(dolist (pattern '("\\*Python\\*"
                   "\\*node-repl\\*"
                   "\\*rust-repl\\*"
                   "\\*go-repl\\*"
                   "\\*lua-repl\\*"
                   "\\*julia\\*"
                   "\\*R\\*"
                   "\\*haskell\\*"
                   "\\*cider-repl.*\\*"
                   "\\*nix-repl\\*"
                   "\\*nrepl.*\\*"))
  (add-to-list 'display-buffer-alist
               `(,pattern
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 1)
                 (window-height . 0.35)
                 (reusable-frames . visible))))

;; ============================================================================
;; GLOBAL KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c x r") #'emacs-ide-repl-launch)
(global-set-key (kbd "C-c x s") #'emacs-ide-repl-send-region)
(global-set-key (kbd "C-c x b") #'emacs-ide-repl-send-buffer)
(global-set-key (kbd "C-c x d") #'emacs-ide-repl-send-defun)
(global-set-key (kbd "C-c x t") #'emacs-ide-repl-toggle-window)

;; ============================================================================
;; STATUS
;; ============================================================================

(defun emacs-ide-repl-status ()
  "Show all registered REPLs and which ones have live buffers."
  (interactive)
  (with-output-to-temp-buffer "*REPL Status*"
    (princ "=== REPL HUB STATUS ===\n\n")
    (princ (format "%-22s %-28s %s\n" "mode" "buffer" "live?"))
    (princ (make-string 60 ?─))
    (princ "\n")
    (dolist (entry emacs-ide-repl--registry)
      (let* ((mode    (car entry))
             (info    (cdr entry))
             (bufname (plist-get info :buffer-name))
             (live    (and bufname (get-buffer bufname))))
        (princ (format "%-22s %-28s %s\n"
                       mode
                       (or bufname "—")
                       (if live "✓ running" "—")))))))

(provide 'tools-repl)
;;; tools-repl.el ends here
