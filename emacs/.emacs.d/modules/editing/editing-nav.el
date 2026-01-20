;;; editing-nav.el --- Navigation configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Avy and navigation utilities
;;; Author: Enterprise Emacs Team
;;; Version: 2.0.0
;;; Code:

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

(provide 'editing-nav)
;;; editing-nav.el ends here
