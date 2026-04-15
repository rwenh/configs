;;; ui-theme.el --- Theme toggling for ef-themes -*- lexical-binding: t -*-
;;; Version: 3.0.4
;;; Code:

(defun emacs-ide--current-theme ()
  (car (custom-enabled-themes)))

(defun emacs-ide-toggle-theme ()
  (interactive)
  (if (fboundp 'ef-themes-toggle)
      (ef-themes-toggle)
    (let* ((current      (emacs-ide--current-theme))
           (dark-themes  '(ef-dark ef-night ef-duo-dark ef-trio-dark
                           ef-cherie ef-winter ef-autumn ef-bio
                           ef-elea-dark ef-cyprus))
           (light-themes '(ef-light ef-day ef-duo-light ef-trio-light
                           ef-spring ef-summer ef-frost
                           ef-elea-light))
           (load-fn      (if (fboundp 'ef-themes-load-theme)
                             #'ef-themes-load-theme
                           (lambda (theme) (load-theme theme t)))))
      (cond
       ((memq current dark-themes)
        (funcall load-fn 'ef-light)
        (message "Theme: ef-light"))
       ((memq current light-themes)
        (funcall load-fn 'ef-dark)
        (message "Theme: ef-dark"))
       (t
        (funcall load-fn 'ef-dark)
        (message "Theme: ef-dark"))))))

(defun emacs-ide-select-theme ()
  (interactive)
  (if (fboundp 'ef-themes-select)
      (call-interactively #'ef-themes-select)
    (message "ui-theme: ef-themes not loaded")))

(provide 'ui-theme)
;;; ui-theme.el ends here
