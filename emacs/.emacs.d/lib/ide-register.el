;;; ide-register.el --- Register and bookmark conveniences -*- lexical-binding: t -*-
;;; Version: 1.0.0
;;;
;;; Code:

(require 'cl-lib)
(eval-and-compile
  (require 'ide-common
           (expand-file-name "lib/ide-common.el" user-emacs-directory)))

(defun ide-register-save-window-configuration (register)
  "Save the current window configuration to REGISTER."
  (interactive (list (register-read-with-preview "Save window config to register: ")))
  (window-configuration-to-register register)
  (message "Window configuration saved to register %c" register))

(defun ide-register-jump-to-register-or-bookmark ()
  "Jump to a register or a bookmark via one unified prompt."
  (interactive)
  (let* ((regs (mapcar (lambda (r)
                          (cons (format "%c — %s" (car r)
                                        (register-describe-oneline (car r)))
                                (car r)))
                        register-alist))
         (choices (append (mapcar #'car regs) '("Bookmark…")))
         (pick (completing-read "Jump to: " choices nil t)))
    (if (string= pick "Bookmark…")
        (call-interactively #'bookmark-jump)
      (jump-to-register (cdr (assoc pick regs))))))

(defun ide-register-list-populated ()
  "Display a buffer listing all populated registers."
  (interactive)
  (if (null register-alist)
      (message "No registers are populated")
    (with-output-to-temp-buffer "*Registers*"
      (princ "=== POPULATED REGISTERS ===\n\n")
      (dolist (r (sort (copy-sequence register-alist)
                        (lambda (a b) (< (car a) (car b)))))
        (princ (format "  %c — %s\n" (car r)
                       (register-describe-oneline (car r))))))))

(defun ide-register--quick-files ()
  "Return (CHAR . PATH) alist from config.yml registers section, or nil."
  (when (and (fboundp 'emacs-ide-config-get) (boundp 'emacs-ide-config-data))
    (let ((section (ide-common-alist-get-in '(registers) emacs-ide-config-data)))
      (cl-loop for (key . val) in section
               unless (or (eq key 'quick-files) (null val))
               collect (cons (aref (symbol-name key) 0)
                              (expand-file-name
                               (if (symbolp val) (symbol-name val) val)))))))

(defun ide-register-store-quick-files ()
  "Populate registers from config.yml's `registers' section."
  (interactive)
  (let ((files (ide-register--quick-files)))
    (if (null files)
        (message "ide-register: no quick-files configured under `registers'")
      (dolist (entry files)
        (set-register (car entry) (cons 'file (cdr entry))))
      (message "ide-register: stored %d quick-file register(s) (%s)"
               (length files)
               (mapconcat (lambda (e) (format "%c" (car e))) files ", ")))))

(defun ide-register-quick-jump ()
  "Jump to one of the configured quick-file registers."
  (interactive)
  (let ((files (ide-register--quick-files)))
    (if (null files)
        (message "ide-register: no quick-files configured")
      (let* ((choices (mapcar (lambda (e)
                                (format "%c — %s" (car e) (cdr e)))
                              files))
             (pick (completing-read "Quick file: " choices nil t)))
        (jump-to-register (string-to-char pick))))))

(provide 'ide-register)
;;; ide-register.el ends here
