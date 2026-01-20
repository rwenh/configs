;;; ui-theme.el --- Theme configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Theme toggling integrated with Emacs IDE
;;; Author: Enterprise Emacs Team
;;; Version: 2.0.0
;;; Code:

(defun my/toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn
        (disable-theme 'modus-operandi)
        (load-theme 'modus-vivendi t)
        (message "🌙 Dark theme"))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)
    (message "☀️ Light theme")))

(provide 'ui-theme)
;;; ui-theme.el ends here
