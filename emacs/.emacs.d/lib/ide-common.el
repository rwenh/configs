;;; ide-common.el --- Shared utility functions -*- lexical-binding: t -*-
;;;
;;; Version: 1.0.0
;;; Code:

(require 'cl-lib)

;;;; ── Project root / name ─────────────────────────────────────────────────────

(defun ide-common-project-root ()
  "Return the current project root, or `default-directory' if none."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      (and (fboundp 'project-current)
           (when-let ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (with-no-warnings (project-roots proj))))))
      default-directory))

(defun ide-common-project-name ()
  "Return a short name for the current project."
  (or (and (fboundp 'projectile-project-name)
           (ignore-errors
             (let ((name (projectile-project-name)))
               (unless (string= name "-") name))))
      (file-name-nondirectory
       (directory-file-name (ide-common-project-root)))))

;;;; ── Buffer / region predicates ──────────────────────────────────────────────

(defun ide-common-line-empty-p ()
  "Return non-nil if the current line is empty or whitespace-only."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ \t]*$")))

(defun ide-common-line-matches-p (regexp)
  "Return non-nil if the current line matches REGEXP."
  (save-excursion
    (beginning-of-line)
    (looking-at-p regexp)))

(defun ide-common-region-or-symbol ()
  "Return the active region as a string, or the symbol at point.
Return nil if neither is available."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((thing-at-point 'symbol)
    (substring-no-properties (thing-at-point 'symbol)))
   (t nil)))

(defun ide-common-large-buffer-p (&optional buffer)
  "Return non-nil if BUFFER (default current buffer) exceeds the configured threshold."
  (let ((threshold (if (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'performance
                                               'large-file-threshold 100000)
                      100000)))
    (> (buffer-size buffer) threshold)))

;;;; ── External process helpers ────────────────────────────────────────────────

(defun ide-common-shell-command-to-lines (command)
  "Run COMMAND and return stdout as a list of non-empty trimmed lines, or nil."
  (with-temp-buffer
    (let ((status (call-process shell-file-name nil t nil
                                 shell-command-switch command)))
      (if (/= status 0)
          nil
        (cl-remove-if
         #'string-empty-p
         (mapcar #'string-trim
                 (split-string (buffer-string) "\n")))))))

(defun ide-common-executable-or-message (exe context)
  "Return full path to EXE, or nil with a standardised message."
  (or (executable-find exe)
      (progn (message "%s: %s not found on PATH" context exe) nil)))

;;;; ── Formatting helpers ──────────────────────────────────────────────────────

(defun ide-common-format-bytes (bytes)
  "Return BYTES as a human-readable string (B/KB/MB/GB)."
  (cond
   ((< bytes 1024)              (format "%dB"     bytes))
   ((< bytes (* 1024 1024))     (format "%.1fKB"  (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes (* 1024.0 1024.0))))
   (t                            (format "%.2fGB" (/ bytes (* 1024.0 1024.0 1024.0))))))

(defun ide-common-mode-label (&optional mode)
  "Return MODE (default `major-mode') as a display label with -mode stripped."
  (let ((name (symbol-name (or mode major-mode))))
    (if (string-suffix-p "-mode" name)
        (substring name 0 -5)
      name)))

;;;; ── Alist helpers ───────────────────────────────────────────────────────────

(defun ide-common-alist-get-in (keys alist)
  "Walk nested ALIST through KEYS, returning the final value or nil."
  (let ((cur alist))
    (dolist (key keys cur)
      (setq cur (cdr (assoc key cur))))))

(provide 'ide-common)
;;; ide-common.el ends here
