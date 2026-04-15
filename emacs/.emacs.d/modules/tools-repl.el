;;; tools-repl.el --- Unified REPL Hub -*- lexical-binding: t -*-
;;; Version: 3.1.1 | PATCH: Display rules now persistent across config reload (FIX #13-alt)
;;; Code:

(require 'cl-lib)
(require 'comint)

(defun emacs-ide-repl--cfg (key default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'repl key default)
    default))

(defun emacs-ide-repl--window-height () (emacs-ide-repl--cfg 'window-height 0.35))
(defun emacs-ide-repl--side ()         (emacs-ide-repl--cfg 'side 'bottom))
(defun emacs-ide-repl--auto-focus ()   (emacs-ide-repl--cfg 'auto-focus t))

;;; ─── Registry ────────────────────────────────────────────────────────────────

(defvar emacs-ide-repl--registry nil
  "Alist of (MAJOR-MODE . plist) — :launch :buffer-name :send-region-fn.")

(defun emacs-ide-repl-register (mode &rest plist)
  (let ((existing (assoc mode emacs-ide-repl--registry)))
    (if existing (setcdr existing plist)
      (push (cons mode plist) emacs-ide-repl--registry))))

;;; ─── Built-in registrations ──────────────────────────────────────────────────

(with-eval-after-load 'python
  (emacs-ide-repl-register 'python-mode
    :launch         (lambda () (run-python nil nil t))
    :buffer-name    "*Python*"
    :send-region-fn #'python-shell-send-region)
  (emacs-ide-repl-register 'python-ts-mode
    :launch         (lambda () (run-python nil nil t))
    :buffer-name    "*Python*"
    :send-region-fn #'python-shell-send-region))

(with-eval-after-load 'rust-mode
  (emacs-ide-repl-register 'rust-mode
    :launch (lambda ()
              (if (executable-find "evcxr")
                  (progn (make-comint "rust-repl" "evcxr")
                         (switch-to-buffer "*rust-repl*"))
                (message "tools-repl: install evcxr: cargo install evcxr_repl")))
    :buffer-name    "*rust-repl*"
    :send-region-fn nil))

(with-eval-after-load 'js2-mode
  (emacs-ide-repl-register 'js2-mode
    :launch (lambda ()
              (if (executable-find "node")
                  (progn (make-comint "node-repl" "node")
                         (switch-to-buffer "*node-repl*"))
                (message "tools-repl: node not found")))
    :buffer-name    "*node-repl*"
    :send-region-fn nil))

(with-eval-after-load 'go-mode
  (emacs-ide-repl-register 'go-mode
    :launch (lambda ()
              (if (executable-find "gore")
                  (progn (make-comint "go-repl" "gore")
                         (switch-to-buffer "*go-repl*"))
                (message "tools-repl: install gore: go install github.com/x-motemen/gore/cmd/gore@latest")))
    :buffer-name    "*go-repl*"
    :send-region-fn nil))

(with-eval-after-load 'lua-mode
  (emacs-ide-repl-register 'lua-mode
    :launch (lambda ()
              (if (executable-find "lua")
                  (progn (make-comint "lua-repl" "lua")
                         (switch-to-buffer "*lua-repl*"))
                (message "tools-repl: lua not found")))
    :buffer-name    "*lua-repl*"
    :send-region-fn nil))

(with-eval-after-load 'clojure-mode
  (emacs-ide-repl-register 'clojure-mode
    :launch         (lambda () (when (fboundp 'cider-jack-in) (cider-jack-in nil)))
    :buffer-name    "*cider-repl*"
    :send-region-fn (lambda (beg end)
                      (when (fboundp 'cider-eval-region)
                        (cider-eval-region beg end)))))

(with-eval-after-load 'haskell-mode
  (emacs-ide-repl-register 'haskell-mode
    :launch (lambda ()
              (when (fboundp 'haskell-interactive-switch)
                (haskell-interactive-switch)))
    :buffer-name    "*haskell*"
    :send-region-fn (lambda (beg end)
                      (cond
                       ((fboundp 'haskell-process-load-region)
                        (haskell-process-load-region beg end))
                       ((fboundp 'haskell-process-load-file)
                        (haskell-process-load-file))))))

(with-eval-after-load 'ess-r-mode
  (emacs-ide-repl-register 'ess-r-mode
    :launch         (lambda () (when (fboundp 'R) (R)))
    :buffer-name    "*R*"
    :send-region-fn (lambda (beg end)
                      (when (fboundp 'ess-eval-region) (ess-eval-region beg end nil)))))

(with-eval-after-load 'julia-mode
  (emacs-ide-repl-register 'julia-mode
    :launch         (lambda () (when (fboundp 'julia-repl) (julia-repl)))
    :buffer-name    "*julia*"
    :send-region-fn nil))

;;; ─── Core REPL operations ────────────────────────────────────────────────────

(defun emacs-ide-repl-get-info ()
  (cdr (assoc major-mode emacs-ide-repl--registry)))

(defun emacs-ide-repl-launch ()
  "Launch or switch to the REPL for the current major-mode."
  (interactive)
  (let* ((info    (emacs-ide-repl-get-info))
         (launch  (and info (plist-get info :launch)))
         (bufname (and info (plist-get info :buffer-name))))
    (cond
     ((null info)
      (message "tools-repl: no REPL registered for %s" major-mode))
     ((and bufname (get-buffer bufname))
      (if (emacs-ide-repl--auto-focus)
          (pop-to-buffer bufname
                         '(display-buffer-reuse-window (inhibit-same-window . t)))
        (display-buffer bufname)))
     (launch (funcall launch))
     (t (message "tools-repl: no launch function for %s" major-mode)))))

(defun emacs-ide-repl-send-region (beg end)
  "Send region BEG–END to the REPL."
  (interactive "r")
  (let* ((info (emacs-ide-repl-get-info))
         (fn   (and info (plist-get info :send-region-fn))))
    (cond
     ((null info) (message "tools-repl: no REPL for %s" major-mode))
     ((null fn)   (message "tools-repl: no send-region for %s" major-mode))
     (t
      (funcall fn beg end)
      (message "→ REPL: sent %d chars" (- end beg))))))

(defun emacs-ide-repl-send-buffer ()
  "Send the entire buffer to the REPL."
  (interactive)
  (emacs-ide-repl-send-region (point-min) (point-max)))

(defun emacs-ide-repl-send-defun ()
  "Send the defun at point to the REPL."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (emacs-ide-repl-send-region (car bounds) (cdr bounds))
      (message "tools-repl: no defun at point"))))

(defun emacs-ide-repl-send-line ()
  "Send the current line to the REPL and advance."
  (interactive)
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position)))
    (emacs-ide-repl-send-region line-beg line-end)
    (forward-line 1)))

(defun emacs-ide-repl-toggle-window ()
  "Show or hide the REPL side-window."
  (interactive)
  (let* ((info    (emacs-ide-repl-get-info))
         (bufname (and info (plist-get info :buffer-name)))
         (win     (and bufname (get-buffer-window bufname))))
    (cond
     ((null bufname)
      (message "tools-repl: no buffer name for %s" major-mode))
     (win (delete-window win))
     ((get-buffer bufname)
      (display-buffer bufname
                      `(display-buffer-in-side-window
                        (side . ,(emacs-ide-repl--side))
                        (slot . 1)
                        (window-height . ,(emacs-ide-repl--window-height)))))
     (t (emacs-ide-repl-launch)))))

;;; ─── Display rules ───────────────────────────────────────────────────────────

(defvar emacs-ide-repl--display-buffer-setup-done nil
  "Track if display rules have been configured.")

(defun emacs-ide-repl--setup-display-rules ()
  "Setup display rules — FIX #13: now persistent via config-reload-hook"
  (let ((height (emacs-ide-repl--window-height))
        (side   (emacs-ide-repl--side)))
    (dolist (pattern '("\\*Python\\*" "\\*node-repl\\*" "\\*rust-repl\\*"
                       "\\*go-repl\\*" "\\*lua-repl\\*" "\\*julia\\*"
                       "\\*R\\*" "\\*haskell\\*" "\\*cider-repl.*\\*"
                       "\\*nix-repl\\*" "\\*nrepl.*\\*" "\\*bash-repl\\*"))
      ;; Remove old rule if it exists
      (setq display-buffer-alist
            (cl-remove pattern display-buffer-alist :key #'car :test #'equal))
      ;; Add new rule
      (push `(,pattern
              (display-buffer-in-side-window)
              (side . ,side)
              (slot . 1)
              (window-height . ,height)
              (reusable-frames . visible))
            display-buffer-alist)))
  (setq emacs-ide-repl--display-buffer-setup-done t))

;; Initial setup
(emacs-ide-repl--setup-display-rules)

;; Re-setup on config reload
(when (boundp 'emacs-ide-config-reload-hook)
  (add-hook 'emacs-ide-config-reload-hook #'emacs-ide-repl--setup-display-rules))

;;; ─── Global keys ─────────────────────────────────────────────────────────────

(global-set-key (kbd "C-c x r") #'emacs-ide-repl-launch)
(global-set-key (kbd "C-c x s") #'emacs-ide-repl-send-region)
(global-set-key (kbd "C-c x b") #'emacs-ide-repl-send-buffer)
(global-set-key (kbd "C-c x d") #'emacs-ide-repl-send-defun)
(global-set-key (kbd "C-c x l") #'emacs-ide-repl-send-line)
(global-set-key (kbd "C-c x t") #'emacs-ide-repl-toggle-window)

;;; ─── Status ──────────────────────────────────────────────────────────────────

(defun emacs-ide-repl-status ()
  (interactive)
  (with-output-to-temp-buffer "*REPL Status*"
    (princ "=== REPL HUB STATUS ===\n\n")
    (princ (format "Window: side=%-8s  height=%.2f  auto-focus=%s\n\n"
                   (emacs-ide-repl--side)
                   (emacs-ide-repl--window-height)
                   (if (emacs-ide-repl--auto-focus) "yes" "no")))
    (princ (format "%-22s %-28s %s\n" "mode" "buffer" "live?"))
    (princ (make-string 64 ?─))
    (princ "\n")
    (if (null emacs-ide-repl--registry)
        (princ "  (no REPLs registered yet)\n")
      (dolist (entry emacs-ide-repl--registry)
        (let* ((mode    (car entry))
               (info    (cdr entry))
               (bufname (plist-get info :buffer-name))
               (live    (and bufname (get-buffer bufname))))
          (princ (format "%-22s %-28s %s\n"
                         mode (or bufname "—")
                         (if live "✓ running" "—"))))))))

(provide 'tools-repl)
;;; tools-repl.el ends here
