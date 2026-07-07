;;; tools-repl.el --- Unified REPL Hub -*- lexical-binding: t -*-
;;; Version: 3.3.0
;;;
;;; Code:

(require 'cl-lib)
(require 'comint)

;;;; ── Config helpers ──────────────────────────────────────────────────────────

(defun emacs-ide-repl--cfg (key default)
  "Read KEY from the repl config section, returning DEFAULT if absent."
  (if (fboundp 'emacs-ide-config-get)
      (emacs-ide-config-get 'repl key default)
    default))

(defun emacs-ide-repl--window-height ()
  (emacs-ide-repl--cfg 'window-height 0.35))

(defun emacs-ide-repl--side ()
  (emacs-ide-repl--cfg 'side 'bottom))

(defun emacs-ide-repl--auto-focus ()
  (emacs-ide-repl--cfg 'auto-focus t))

;;;; ── Registry ────────────────────────────────────────────────────────────────

(defvar emacs-ide-repl--registry nil
  "Alist of (MAJOR-MODE . plist).
Each plist has keys :launch, :buffer-name, :send-region-fn.")

(defun emacs-ide-repl-register (mode &rest plist)
  "Register or replace the REPL entry for MAJOR-MODE.
PLIST should contain :launch, :buffer-name, and optionally :send-region-fn."
  (let ((existing (assoc mode emacs-ide-repl--registry)))
    (if existing
        (setcdr existing plist)
      (push (cons mode plist) emacs-ide-repl--registry))))

;;;; ── Built-in registrations ──────────────────────────────────────────────────

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
                (message "tools-repl: node not found on PATH")))
    :buffer-name    "*node-repl*"
    :send-region-fn nil))

(with-eval-after-load 'go-mode
  (emacs-ide-repl-register 'go-mode
    :launch (lambda ()
              (if (executable-find "gore")
                  (progn (make-comint "go-repl" "gore")
                         (switch-to-buffer "*go-repl*"))
                (message
                 "tools-repl: install gore: go install github.com/x-motemen/gore/cmd/gore@latest")))
    :buffer-name    "*go-repl*"
    :send-region-fn nil))

(with-eval-after-load 'lua-mode
  (emacs-ide-repl-register 'lua-mode
    :launch (lambda ()
              (if (executable-find "lua")
                  (progn (make-comint "lua-repl" "lua")
                         (switch-to-buffer "*lua-repl*"))
                (message "tools-repl: lua not found on PATH")))
    :buffer-name    "*lua-repl*"
    :send-region-fn nil))

(with-eval-after-load 'clojure-mode
  (emacs-ide-repl-register 'clojure-mode
    :launch (lambda ()
              (if (fboundp 'cider-jack-in)
                  (cider-jack-in nil)
                (message "tools-repl: CIDER not loaded — open a .clj file first")))
    :buffer-name    "*cider-repl*"
    :send-region-fn (lambda (beg end)
                      (if (fboundp 'cider-eval-region)
                          (cider-eval-region beg end)
                        (message "tools-repl: CIDER not connected")))))

(with-eval-after-load 'haskell-mode
  (emacs-ide-repl-register 'haskell-mode
    :launch (lambda ()
              (if (fboundp 'haskell-interactive-switch)
                  (haskell-interactive-switch)
                (message "tools-repl: haskell-interactive not available")))
    :buffer-name    "*haskell*"
    :send-region-fn (lambda (beg end)
                      (cond
                       ((fboundp 'haskell-process-load-region)
                        (haskell-process-load-region beg end))
                       ((fboundp 'haskell-process-load-file)
                        (haskell-process-load-file))
                       (t
                        (message "tools-repl: no haskell send function available"))))))

(with-eval-after-load 'ess-r-mode
  (emacs-ide-repl-register 'ess-r-mode
    :launch (lambda ()
              (if (fboundp 'R)
                  (R)
                (message "tools-repl: ESS R not available")))
    :buffer-name    "*R*"
    :send-region-fn (lambda (beg end)
                      (if (fboundp 'ess-eval-region)
                          (ess-eval-region beg end nil)
                        (message "tools-repl: ess-eval-region not available")))))

(with-eval-after-load 'julia-mode
  (emacs-ide-repl-register 'julia-mode
    :launch (lambda ()
              (cond
               ((fboundp 'julia-repl)   (julia-repl))
               ((executable-find "julia")
                (make-comint "julia-repl" "julia")
                (switch-to-buffer "*julia-repl*"))
               (t (message "tools-repl: julia not found on PATH"))))
    :buffer-name    "*julia*"
    :send-region-fn (lambda (beg end)
                      (let ((buf (get-buffer "*julia*")))
                        (if buf
                            (progn
                              (comint-send-region buf beg end)
                              (with-current-buffer buf
                                (comint-send-input)))
                          (message "tools-repl: julia REPL not running — C-c x r to start"))))))

;;;; ── Core REPL operations ────────────────────────────────────────────────────

(defun emacs-ide-repl-get-info ()
  "Return the registry plist for `major-mode', or nil."
  (cdr (assoc major-mode emacs-ide-repl--registry)))

(defun emacs-ide-repl-launch ()
  "Launch or switch to the REPL for `major-mode'."
  (interactive)
  (let* ((info    (emacs-ide-repl-get-info))
         (launch  (and info (plist-get info :launch)))
         (bufname (and info (plist-get info :buffer-name))))
    (cond
     ((null info)
      (message "tools-repl: no REPL registered for %s.  \
Open a source file and try again, or register via emacs-ide-repl-register."
               major-mode))
     ;; Buffer already live — just switch to it
     ((and bufname (get-buffer bufname))
      (if (emacs-ide-repl--auto-focus)
          (pop-to-buffer bufname
                         '(display-buffer-reuse-window
                           (inhibit-same-window . t)))
        (display-buffer bufname)))
     ;; Launch a new REPL
     (launch (funcall launch))
     (t
      (message "tools-repl: no launch function registered for %s" major-mode)))))

(defun emacs-ide-repl-send-region (beg end)
  "Send the region BEG–END to the REPL for `major-mode'."
  (interactive "r")
  (let* ((info (emacs-ide-repl-get-info))
         (fn   (and info (plist-get info :send-region-fn))))
    (cond
     ((null info)
      (message "tools-repl: no REPL registered for %s" major-mode))
     ((null fn)
      (message "tools-repl: no send-region function for %s.  \
Tip: paste region manually or implement :send-region-fn." major-mode))
     (t
      (funcall fn beg end)
      (message "→ REPL: sent %d chars" (- end beg))))))

(defun emacs-ide-repl-send-buffer ()
  "Send the entire buffer to the REPL."
  (interactive)
  (emacs-ide-repl-send-region (point-min) (point-max)))

(defun emacs-ide-repl-send-defun ()
  "Send the top-level form at point to the REPL."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (if bounds
        (emacs-ide-repl-send-region (car bounds) (cdr bounds))
      (message "tools-repl: no defun at point"))))

(defun emacs-ide-repl-send-line ()
  "Send the current line to the REPL and advance to the next line."
  (interactive)
  (emacs-ide-repl-send-region (line-beginning-position)
                               (line-end-position))
  (forward-line 1))

(defun emacs-ide-repl-toggle-window ()
  "Show the REPL in a side window, or close it if already visible."
  (interactive)
  (let* ((info    (emacs-ide-repl-get-info))
         (bufname (and info (plist-get info :buffer-name)))
         (win     (and bufname (get-buffer-window bufname))))
    (cond
     ((null bufname)
      (message "tools-repl: no buffer name registered for %s" major-mode))
     ;; Window is open — close it
     (win
      (delete-window win))
     ;; Buffer exists but not shown — display in side window
     ((get-buffer bufname)
      (display-buffer
       bufname
       `(display-buffer-in-side-window
         (side          . ,(emacs-ide-repl--side))
         (slot          . 1)
         (window-height . ,(emacs-ide-repl--window-height)))))
     (t
      (emacs-ide-repl-launch)))))

;;;; ── Display rules ───────────────────────────────────────────────────────────

(defun emacs-ide-repl--setup-display-rules ()
  "Register side-window display rules for all known REPL buffer names.
Re-runs on config reload so that changes to repl.side / repl.window-height
in config.yml take effect immediately without restarting Emacs."
  (let ((height (emacs-ide-repl--window-height))
        (side   (emacs-ide-repl--side)))
    (dolist (pattern '("\\*Python\\*"
                       "\\*node-repl\\*"
                       "\\*rust-repl\\*"
                       "\\*go-repl\\*"
                       "\\*lua-repl\\*"
                       "\\*julia\\*"
                       "\\*julia-repl\\*"
                       "\\*R\\*"
                       "\\*haskell\\*"
                       "\\*cider-repl.*\\*"
                       "\\*nix-repl\\*"
                       "\\*nrepl.*\\*"
                       "\\*bash-repl\\*"
                       "\\*SQL\\*"
                       "\\*ielm\\*"))
      (setq display-buffer-alist
            (cl-remove pattern display-buffer-alist
                       :key  #'car
                       :test #'equal))
      (push `(,pattern
              (display-buffer-in-side-window
               (side          . ,side)
               (slot          . 1)
               (window-height . ,height)
               (reusable-frames . visible)))
            display-buffer-alist))))

(emacs-ide-repl--setup-display-rules)

(when (boundp 'emacs-ide-config-reload-hook)
  (add-hook 'emacs-ide-config-reload-hook
            #'emacs-ide-repl--setup-display-rules))

;;;; ── Status ──────────────────────────────────────────────────────────────────

(defun emacs-ide-repl-status ()
  "Display a buffer listing all registered REPLs and their live status."
  (interactive)
  (with-output-to-temp-buffer "*REPL Status*"
    (princ "=== REPL HUB STATUS ===\n\n")
    (princ (format "Window:     side=%-8s  height=%.2f  auto-focus=%s\n\n"
                   (emacs-ide-repl--side)
                   (emacs-ide-repl--window-height)
                   (if (emacs-ide-repl--auto-focus) "yes" "no")))
    (princ (format "%-26s %-30s %-10s %s\n"
                   "mode" "buffer" "live?" "send-region?"))
    (princ (make-string 80 ?─))
    (princ "\n")
    (if (null emacs-ide-repl--registry)
        (princ "  (no REPLs registered yet — open a source file to trigger lazy loading)\n")
      (dolist (entry (sort (copy-sequence emacs-ide-repl--registry)
                           (lambda (a b)
                             (string< (symbol-name (car a))
                                      (symbol-name (car b))))))
        (let* ((mode    (car entry))
               (info    (cdr entry))
               (bufname (plist-get info :buffer-name))
               (live    (and bufname (get-buffer bufname)))
               (send-fn (plist-get info :send-region-fn)))
          (princ (format "%-26s %-30s %-10s %s\n"
                         mode
                         (or bufname "—")
                         (if live "✓ running" "—")
                         (if send-fn "✓" "—"))))))))

(provide 'tools-repl)
;;; tools-repl.el ends here
