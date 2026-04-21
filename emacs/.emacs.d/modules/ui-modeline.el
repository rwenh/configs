;;; ui-modeline.el --- Modeline configuration -*- lexical-binding: t -*-
;;; Version: 3.2.1 | FIX: health keymap deferred — was firing before doom-modeline
;;;           loaded, producing "<nil> <mouse-1> is undefined" on TTY/early load.
;;; Code:

(unless (locate-library "doom-modeline")
  (if (require 'powerline nil 'noerror)
      (add-hook 'after-init-hook (lambda () (powerline-default-theme)))
    (message "ui-modeline: neither doom-modeline nor powerline found — using default modeline")))

;; FIX: The keymap must NOT be created at top level as a defvar initializer,
;; because `define-key [mode-line mouse-1]` only makes sense in a GUI frame and
;; fires a void-keymap / undefined-key error when Emacs loads in a terminal or
;; before frames exist.  We create it lazily inside the function that uses it.

(defvar emacs-ide-modeline--health-map nil
  "Keymap for the IDE health modeline segment click action.
Created lazily on first use so it never fires in a frameless/TTY context.")

(defun emacs-ide-modeline--ensure-health-map ()
  "Initialise `emacs-ide-modeline--health-map' if not yet done."
  (unless emacs-ide-modeline--health-map
    (let ((map (make-sparse-keymap)))
      (condition-case nil
          (define-key map [mode-line mouse-1] #'emacs-ide-health-check-all)
        (error nil))
      (setq emacs-ide-modeline--health-map map))))

(defun emacs-ide-modeline--health-string ()
  (if (fboundp 'emacs-ide-health--summary-string)
      (let* ((s    (emacs-ide-health--summary-string))
             (face (cond
                    ((string-prefix-p "✗" s) 'error)
                    ((string-prefix-p "⚠" s) 'warning)
                    ((string-prefix-p "?" s) 'shadow)
                    (t                       'success))))
        (emacs-ide-modeline--ensure-health-map)
        (propertize (concat " 🏥" s " ")
                    'face       face
                    'help-echo  "IDE Health — click to run full check"
                    'mouse-face 'mode-line-highlight
                    'local-map  emacs-ide-modeline--health-map))
    ""))

(with-eval-after-load 'doom-modeline
  (unless (featurep 'emacs-ide-modeline-installed)
    (doom-modeline-def-segment emacs-ide-health
      "IDE health status — ✓ / ⚠N / ✗N."
      (when (eq (selected-window) (get-buffer-window))
        (emacs-ide-modeline--health-string)))

    (doom-modeline-def-modeline 'main
      '(bar window-number modals matches follow buffer-info
        remote-host buffer-position word-count parrot selection-info)
      '(compilation objed-state misc-info battery grip irc
        mu4e gnus github debug repl lsp minor-modes input-method
        indent-info buffer-encoding major-mode process vcs
        emacs-ide-health))

    (provide 'emacs-ide-modeline-installed)))

(unless (locate-library "doom-modeline")
  (unless (member '(:eval (emacs-ide-modeline--health-string))
                  global-mode-string)
    (add-to-list 'global-mode-string
                 '(:eval (emacs-ide-modeline--health-string))
                 t)))

(provide 'ui-modeline)
;;; ui-modeline.el ends here
