;;; ui-theme.el --- Theme toggling for ef-themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Replaces modus-themes toggle with ef-themes toggle.
;;; Respects config.yml theme key. F12 calls emacs-ide-toggle-theme.
;;; Version: 3.0.0
;;; Code:

(defun emacs-ide--current-theme ()
  "Return currently enabled theme symbol or nil."
  (car (custom-enabled-themes)))

(defun emacs-ide-toggle-theme ()
  "Toggle between dark and light ef-theme.
Uses ef-themes-to-toggle pair if set, else defaults to ef-dark/ef-light."
  (interactive)
  (if (fboundp 'ef-themes-toggle)
      (ef-themes-toggle)
    ;; Fallback if ef-themes not loaded yet
    (let* ((current (emacs-ide--current-theme))
           (dark-themes  '(ef-dark ef-night ef-duo-dark ef-trio-dark
                           ef-cherie ef-winter ef-autumn ef-bio))
           (light-themes '(ef-light ef-day ef-duo-light ef-trio-light
                           ef-spring ef-summer ef-frost ef-cyprus)))
      (cond
       ((memq current dark-themes)
        (disable-theme current)
        (load-theme 'ef-light t)
        (message "ef-light"))
       ((memq current light-themes)
        (disable-theme current)
        (load-theme 'ef-dark t)
        (message "ef-dark"))
       (t
        (load-theme 'ef-dark t)
        (message "ef-dark"))))))

(defun emacs-ide-select-theme ()
  "Interactively select any ef-theme by name."
  (interactive)
  (if (fboundp 'ef-themes-select)
      (call-interactively #'ef-themes-select)
    (message "ui-theme: ef-themes not loaded")))

(provide 'ui-theme)
;;; ui-theme.el ends here
