;;; ui-theme.el --- Theme toggling integrated with Emacs IDE (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Theme utilities that respect configuration and avoid duplicate toggles.
;;; Code:

(defun emacs-ide--current-theme-symbol ()
  "Return first enabled theme symbol or nil."
  (car (custom-enabled-themes)))

(defun emacs-ide-toggle-theme ()
  "Toggle between configured light/dark themes.
Respects `emacs-ide-theme' if set; safe if themes missing."
  (interactive)
  (let* ((preferred (when (boundp 'emacs-ide-theme) emacs-ide-theme))
         (current (emacs-ide--current-theme-symbol)))
    (cond
     ((and preferred (eq current preferred))
      ;; try alternate if configured
      (let ((alternate (if (eq preferred 'modus-vivendi) 'modus-operandi 'modus-vivendi)))
        (when (custom-theme-enabled-p preferred) (disable-theme preferred))
        (when (ignore-errors (load-theme alternate t))
          (message "Theme switched to %s" alternate))))
     (t
      ;; fallback toggle between modus themes if available
      (cond
       ((custom-theme-enabled-p 'modus-vivendi)
        (disable-theme 'modus-vivendi)
        (ignore-errors (load-theme 'modus-operandi t))
        (message "☀️ Light theme"))
       (t
        (disable-theme 'modus-operandi)
        (ignore-errors (load-theme 'modus-vivendi t))
        (message "🌙 Dark theme")))))))

(provide 'ui-theme)
;;; ui-theme.el ends here