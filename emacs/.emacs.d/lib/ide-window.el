;;; ide-window.el --- Window layout helpers -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;;
;;; Code:

(require 'cl-lib)
(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory)))

(defgroup ide-window nil "Window layout helpers." :group 'convenience :prefix "ide-window-")

(defcustom ide-window-split-width-threshold 160
  "Width above which `ide-window-split-sensibly' splits side-by-side."
  :type 'integer :group 'ide-window)

(defun ide-window-split-sensibly (&optional window)
  "Split WINDOW side-by-side if wide enough, otherwise below."
  (interactive)
  (let ((window (or window (selected-window))))
    (if (>= (window-total-width window) ide-window-split-width-threshold)
        (split-window window nil 'right)
      (split-window window nil 'below))
    (other-window 1)))

(defun ide-window-toggle-split-direction ()
  "Toggle a two-window frame between side-by-side and stacked."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Needs exactly 2 windows"))
  (let* ((windows (window-list))
         (w1 (car windows)) (w2 (cadr windows))
         (b1 (window-buffer w1)) (b2 (window-buffer w2))
         (side-by-side (/= (window-left-column w1) (window-left-column w2))))
    (delete-other-windows w1)
    (if side-by-side (split-window-below) (split-window-right))
    (set-window-buffer (selected-window) b1)
    (set-window-buffer (next-window) b2)))

(defun ide-window-swap-buffers ()
  "Swap buffers in the selected window and the next window."
  (interactive)
  (if (= (count-windows) 1)
      (message "Only one window")
    (let* ((w1 (selected-window)) (w2 (next-window))
           (b1 (window-buffer w1)) (b2 (window-buffer w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (select-window w2))))

(defun ide-window-new-frame-with-current-buffer ()
  "Open the current buffer in a new frame."
  (interactive)
  (let ((buf (current-buffer)))
    (select-frame (make-frame))
    (switch-to-buffer buf)))

(defun ide-window-kill-side-windows ()
  "Delete all side windows (REPL, neotree, etc.) in one step."
  (interactive)
  (let ((n 0))
    (dolist (win (window-list (selected-frame)))
      (when (window-parameter win 'window-side)
        (ignore-errors (delete-window win))
        (cl-incf n)))
    (message (if (> n 0) "Closed %d side window(s)" "No side windows") n)))

(defun ide-window-display-buffer-bottom (buffer alist)
  "Display BUFFER in a bottom side window.  Suitable as display-buffer-alist action."
  (display-buffer-in-side-window buffer (append alist '((side . bottom) (slot . 1)))))

(defmacro ide-window-act-and-restore (&rest body)
  "Execute BODY then restore the window configuration."
  (declare (indent 0))
  `(let ((ide-window--saved (current-window-configuration)))
     (unwind-protect (progn ,@body)
       (set-window-configuration ide-window--saved))))

(provide 'ide-window)
;;; ide-window.el ends here
