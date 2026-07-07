;;; ide-simple.el --- Everyday QoL commands -*- lexical-binding: t -*-
;;; Commentary:
;;; "Why doesn't Emacs already do this" interactive commands.
;;; None bind themselves to keys — that's left to the modules that
;;; `require' this file.
;;; Version: 1.0.0
;;;
;;; Code:

(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory)))

;;;; ── Escape / movement ───────────────────────────────────────────────────────

(defun ide-simple-escape ()
  "DWIM ESC: abort minibuffer, deactivate region, hide popups, or keyboard-quit."
  (interactive)
  (cond
   ((minibufferp) (abort-recursive-edit))
   ((region-active-p) (deactivate-mark))
   ((and (bound-and-true-p corfu-mode) (fboundp 'corfu-quit))
    (corfu-quit))
   ((and (bound-and-true-p which-key-mode) (fboundp 'which-key--hide-popup))
    (which-key--hide-popup))
   (t (keyboard-quit))))

(defun ide-simple-smart-beginning-of-line ()
  "Toggle between first non-whitespace char and true beginning of line."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point))
      (beginning-of-line))))

;;;; ── Killing / lines ─────────────────────────────────────────────────────────

(defun ide-simple-kill-line-backward ()
  "Kill from point back to line indentation, or to bol if already there."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (if (< (point) start)
        (kill-region (point) start)
      (kill-region (line-beginning-position) start))))

(defun ide-simple-open-line-below ()
  "Insert a new indented line below and move to it."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(defun ide-simple-open-line-above ()
  "Insert a new indented line above and move to it."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun ide-simple-duplicate-line-or-region (&optional n)
  "Duplicate the current line or active region N times."
  (interactive "p")
  (let ((n (or n 1)))
    (if (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (text (buffer-substring-no-properties beg end)))
          (goto-char end)
          (dotimes (_ n) (insert text))
          (set-mark end)
          (goto-char (+ end (* n (length text))))
          (exchange-point-and-mark))
      (let ((text (concat (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           "\n"))
            (col (current-column)))
        (beginning-of-line)
        (dotimes (_ n) (insert text))
        (move-to-column col)))))

;;;; ── Insertion ───────────────────────────────────────────────────────────────

(defun ide-simple-insert-date (&optional arg)
  "Insert date at point.  C-u adds time.  C-u C-u inserts Org inactive ts."
  (interactive "P")
  (insert
   (pcase arg
     ('(4)  (format-time-string "%Y-%m-%d %H:%M"))
     ('(16) (format-time-string "[%Y-%m-%d %a %H:%M]"))
     (_     (format-time-string "%Y-%m-%d")))))

;;;; ── Files / buffers ─────────────────────────────────────────────────────────

(defun ide-simple-rename-file-and-buffer (new-name)
  "Rename the current buffer's file to NEW-NAME and update the buffer."
  (interactive (list (read-string "New name: " (buffer-name))))
  (let ((old-file (buffer-file-name)))
    (unless old-file (user-error "Buffer is not visiting a file"))
    (let* ((dir (file-name-directory old-file))
           (new-file (if (file-name-absolute-p new-name)
                         new-name
                       (expand-file-name new-name dir))))
      (when (file-exists-p new-file)
        (user-error "File %s already exists" new-file))
      (rename-file old-file new-file 1)
      (set-visited-file-name new-file t t)
      (message "Renamed to %s" new-file))))

(defun ide-simple-delete-file-and-buffer ()
  "Delete the file backing the current buffer and kill the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file (user-error "Buffer is not visiting a file"))
    (when (yes-or-no-p (format "Delete %s and its buffer? " file))
      (delete-file file)
      (kill-buffer (current-buffer))
      (message "Deleted %s" file))))

(defun ide-simple-copy-buffer-file-name (&optional relative)
  "Copy the current file path to the kill ring.  Prefix → project-relative."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (unless file (user-error "Buffer is not visiting a file"))
    (let ((path (if relative
                    (file-relative-name file (ide-common-project-root))
                  file)))
      (kill-new path)
      (message "Copied: %s" path))))

(defun ide-simple-scratch-buffer-for-mode (&optional mode)
  "Switch to a *scratch-MODE* buffer running MODE."
  (interactive)
  (let* ((target (or mode major-mode))
         (name (format "*scratch-%s*" (ide-common-mode-label target)))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (unless (eq major-mode target) (funcall target)))
    (pop-to-buffer buf)))

;;;; ── Window focus ────────────────────────────────────────────────────────────

(defvar ide-simple--monocle-config nil)

(defun ide-simple-monocle ()
  "Toggle a single-window zoomed view; repeat restores the previous layout."
  (interactive)
  (if ide-simple--monocle-config
      (progn
        (set-window-configuration ide-simple--monocle-config)
        (setq ide-simple--monocle-config nil)
        (message "Monocle off"))
    (setq ide-simple--monocle-config (current-window-configuration))
    (delete-other-windows)
    (message "Monocle on")))

(provide 'ide-simple)
;;; ide-simple.el ends here
