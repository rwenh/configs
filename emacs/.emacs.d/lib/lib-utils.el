;;; lib-utils.el --- Utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Common utility functions for Emacs IDE
;;; Author: Enterprise Emacs Team
;;; Version: 2.0.0
;;; Code:

(defun my/string-trim (str)
  "Trim whitespace from STR."
  (string-trim str))

(defun my/load-if-exists (file)
  "Load FILE if it exists, return t on success."
  (when (file-exists-p file)
    (load file nil 'nomessage)
    t))

(defun my/ensure-directory (dir)
  "Ensure DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(provide 'lib-utils)
;;; lib-utils.el ends here
