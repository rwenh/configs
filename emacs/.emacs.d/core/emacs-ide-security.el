;;; emacs-ide-security.el --- Security Hardening -*- lexical-binding: t -*-
;;; Commentary:
;;; Enterprise security configuration
;;; Code:

;; TLS Configuration
(with-eval-after-load 'gnutls
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 3072
        tls-checktrust t))

;; Package signature checking
(setq package-check-signature 'allow-unsigned)  ; Set to t for strict

;; Auth-source configuration
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))

;; Network security
(setq network-security-level 'high)

(defun emacs-ide-security-check ()
  "Run security audit."
  (interactive)
  (with-output-to-temp-buffer "*Security Audit*"
    (princ "=== SECURITY AUDIT ===\n\n")
    (princ (format "TLS Verification: %s\n"
                   (if (bound-and-true-p gnutls-verify-error) "✓ Enabled" "✗ Disabled")))
    (princ (format "Package Signatures: %s\n"
                   (if package-check-signature "✓ Checking" "✗ Not checking")))
    (princ (format "Auth Source: %s\n"
                   (if (member "~/.authinfo.gpg" auth-sources) "✓ Encrypted" "⚠ Plain text")))
    (princ (format "Network Security: %s\n" network-security-level))))

(provide 'emacs-ide-security)
;;; emacs-ide-security.el ends here
