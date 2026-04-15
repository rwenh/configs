;;; tools-repl.el --- Unified REPL Hub -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)
(require 'comint)

(defun emacs-ide-repl--cfg (key default)
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'repl key default)
    default))

(defun emacs-ide-repl--window-height ()
  (emacs-ide-repl--cfg 'window-height 0.35))

(defun emacs-ide-repl--side ()
  (emacs-ide-repl--cfg 'side 'bottom))

(defvar emacs-ide-repl--registry nil
  "Alist of (MAJOR-MODE . plist) where plist has :launch :buffer-name :send-region-fn.")

(defun emacs-ide-repl-register (mode &rest plist)
  (let ((existing (assoc mode emacs-ide-repl--registry)))
    (if existing
        (setcdr existing plist)
      (push (cons mode plist) emacs-ide-repl--registry))))

(with-eval-after-load 'python
  (emacs-ide-repl-register 'python-mode
    :launch         (lambda () (run-python nil nil t))
    :buffer-name    "*Python*"
    :send-region-fn #'python-shell-send-region))

(with-eval-after-load 'rust-mode
  (emacs-ide-repl-register 'rust-mode
    :launch         (lambda ()
                      (if (executable-find "evcxr")
                          (progn (make-comint "rust-repl" "evcxr")
                                 (switch-to-buffer "*rust-repl*"))
                        (message "tools-repl: install evcxr: cargo install evcxr_repl")))
    :buffer-name    "*rust-repl*"
    :send-region-fn nil))

(with-eval-after-load 'js2-mode
  (emacs-ide-repl-register 'js2-mode
    :launch         (lambda ()
                      (if (executable-find "node")
                          (progn (make-comint "node-repl" "node")
                                 (switch-to-buffer "*node-repl*"))
                        (message "tools-repl: node not found on PATH")))
    :buffer-name    "*node-repl*"
    :send-region-fn nil))

(with-eval-after-load 'go-mode
  (emacs-ide-repl-register 'go-mode
    :launch         (lambda ()
                      (if (executable-find "gore")
                          (progn (make-comint "go-repl" "gore")
                                 (switch-to-buffer "*go-repl*"))
                        (message "tools-repl: go install github.com/x-motemen/gore/cmd/gore@latest")))
    :buffer-name    "*go-repl*"
    :send-region-fn nil))

(with-eval-after-load 'lua-mode
  (emacs-ide-repl-register 'lua-mode
    :launch         (lambda ()
                      (if (executable-find "lua")
                          (progn (make-comint "lua-repl" "lua")
                                 (switch-to-buffer "*lua-repl*"))
                        (message "tools-repl: lua not found on PATH")))
    :buffer-name    "*lua-repl*"
    :send-region-fn nil))

(with-eval-after-load 'clojure-mode
  (emacs-ide-repl-register 'clojure-mode
    :launch         (lambda ()
                      (when (fboundp 'cider-jack-in) (cider-jack-in nil)))
    :buffer-name    "*cider-repl*"
    :send-region-fn (lambda (beg end)
                      (when (fboundp 'cider-eval-region)
                        (cider-eval-region beg end)))))

(with-eval-after-load 'haskell-mode
  (emacs-ide-repl-register 'haskell-mode
    :launch         (lambda ()
                      (when (fboundp 'haskell-interactive-switch)
                        (haskell-interactive-switch)))
    :buffer-name    "*haskell*"
    :send-region-fn (lambda (beg end)
                      (cond
                       ((fboundp 'haskell-process-load-region)
                        (haskell-process-load-region beg end))
                       ((fboundp 'haskell-process-load-file)
                        (message "tools-repl: haskell-process-load-region unavailable, sending whole file")
                        (haskell-process-load-file))
                       (t
                        (message "tools-repl: no haskell send function available"))))))

(with-eval-after-load 'ess-r-mode
  (emacs-ide-repl-register 'ess-r-mode
    :launch         (lambda () (when (fboundp 'R) (R)))
    :buffer-name    "*R*"
    :send-region-fn (lambda (beg end)
                      (when (fboundp 'ess-eval-region)
                        (ess-eval-region beg end nil)))))

(with-eval-after-load 'julia-mode
  (emacs-ide-repl-register 'julia-mode
    :launch         (lambda () (when (fboundp 'julia-repl) (julia-repl)))
    :buffer-name    "*julia*"
    :send-region-fn nil))

(defun emacs-ide-repl-get-info ()
  (cdr (assoc major-mode emacs-ide-repl--registry)))

(defun emacs-ide-repl-launch ()
  (interactive)
  (let* ((info    (emacs-ide-repl-get-info))
         (launch  (and info (plist-get info :launch)))
         (bufname (and info (plist-get info :buffer-name))))
    (cond
     ((null info)
      (message "tools-repl: no REPL registered for %s" major-mode))
     ((and bufname (get-buffer bufname))
      (pop-to-buffer bufname
                     '(display-buffer-reuse-window
                       (inhibit-same-window . t))))
     (launch
      (funcall launch))
     (t
      (message "tools-repl: no launch function for %s" major-mode)))))

(defun emacs-ide-repl-send-region (beg end)
  (interactive "r")
  (let* ((info (emacs-ide-repl-get-info))
         (fn   (and info (plist-get info :send-region-fn))))
    (cond
     ((null info) (message "tools-repl: no REPL for %s" major-mode))
     ((null fn)   (message "tools-repl: no send-region for %s" major-mode))
     (t
      (funcall fn beg end)
      (message "tools-repl: sent region (%d chars)" (- end beg))))))

(defun emacs-ide-repl-send-buffer ()
  (interactive)
  (emacs-ide-repl-send-region (point-min) (point-max)))

(defun emacs-ide-repl-send-defun ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (emacs-ide-repl-send-region (car bounds) (cdr bounds))
      (message "tools-repl: no defun at point"))))

(defun emacs-ide-repl-toggle-window ()
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
                      `(display-buffer-in-side-window
                        (side . ,(emacs-ide-repl--side))
                        (slot . 1)
                        (window-height . ,(emacs-ide-repl--window-height)))))
     (t
      (emacs-ide-repl-launch)))))

(defun emacs-ide-repl--setup-display-rules ()
  (let ((height (emacs-ide-repl--window-height))
        (side   (emacs-ide-repl--side)))
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
      (setq display-buffer-alist
            (cl-remove pattern display-buffer-alist
                       :key #'car :test #'equal))
      (push `(,pattern
              (display-buffer-in-side-window)
              (side . ,side)
              (slot . 1)
              (window-height . ,height)
              (reusable-frames . visible))
            display-buffer-alist))))

(emacs-ide-repl--setup-display-rules)
(add-hook 'emacs-ide-config-reload-hook #'emacs-ide-repl--setup-display-rules)

(global-set-key (kbd "C-c x r") #'emacs-ide-repl-launch)
(global-set-key (kbd "C-c x s") #'emacs-ide-repl-send-region)
(global-set-key (kbd "C-c x b") #'emacs-ide-repl-send-buffer)
(global-set-key (kbd "C-c x d") #'emacs-ide-repl-send-defun)
(global-set-key (kbd "C-c x t") #'emacs-ide-repl-toggle-window)

(defun emacs-ide-repl-status ()
  (interactive)
  (with-output-to-temp-buffer "*REPL Status*"
    (princ "=== REPL HUB STATUS ===\n\n")
    (princ (format "Window: side=%-8s height=%.2f\n\n"
                   (emacs-ide-repl--side)
                   (emacs-ide-repl--window-height)))
    (princ (format "%-22s %-28s %s\n" "mode" "buffer" "live?"))
    (princ (make-string 60 ?─))
    (princ "\n")
    (if (null emacs-ide-repl--registry)
        (princ "  (no REPLs registered yet)\n")
      (dolist (entry emacs-ide-repl--registry)
        (let* ((mode    (car entry))
               (info    (cdr entry))
               (bufname (plist-get info :buffer-name))
               (live    (and bufname (get-buffer bufname))))
          (princ (format "%-22s %-28s %s\n"
                         mode
                         (or bufname "—")
                         (if live "✓ running" "—"))))))))

(provide 'tools-repl)
;;; tools-repl.el ends here
