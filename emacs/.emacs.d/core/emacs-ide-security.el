;;; emacs-ide-security.el --- Security Hardening (CALIBRATED) -*- lexical-binding: t -*-
;;; Commentary:
;;; Enterprise security configuration with config integration
;;; Code:

;; ============================================================================
;; TLS CONFIGURATION - DEFERRED LOADING
;; ============================================================================
(with-eval-after-load 'gnutls
  (let ((tls-verify (or (bound-and-true-p emacs-ide-tls-verify) t)))
    (setq gnutls-verify-error tls-verify
          gnutls-min-prime-bits 3072
          tls-checktrust t)))

;; ============================================================================
;; PACKAGE SIGNATURE CHECKING
;; ============================================================================
(let ((check-sigs (or (bound-and-true-p emacs-ide-package-check-signature)
                     'allow-unsigned)))
  (setq package-check-signature check-sigs))

;; ============================================================================
;; AUTH-SOURCE CONFIGURATION
;; ============================================================================
(require 'auth-source)

;; Use config values if available, otherwise defaults
(let ((auth-sources (or (and (boundp 'emacs-ide-config-data)
                            (let ((sec (cdr (assoc 'security emacs-ide-config-data))))
                              (when sec
                                (cdr (assoc 'auth-sources sec)))))
                       '("~/.authinfo.gpg" "~/.netrc"))))
  (setq auth-sources (mapcar (lambda (s)
                              (expand-file-name s))
                            auth-sources)))

;; ============================================================================
;; NETWORK SECURITY
;; ============================================================================
(let ((level (or (and (boundp 'emacs-ide-config-data)
                      (let ((sec (cdr (assoc 'security emacs-ide-config-data))))
                        (when sec
                          (cdr (assoc 'network-security-level sec)))))
                 'high)))
  (setq network-security-level level))

;; ============================================================================
;; SECURITY AUDIT FUNCTION
;; ============================================================================
(defun emacs-ide-security-check ()
  "Run security audit with detailed reporting."
  (interactive)
  (let ((warnings '())
        (ok '()))
    
    ;; Check TLS
    (if (and (boundp 'gnutls-verify-error)
            gnutls-verify-error)
        (push "✓ TLS verification enabled" ok)
      (push "⚠️  TLS verification not enforced" warnings))
    
    ;; Check package signatures
    (if (bound-and-true-p package-check-signature)
        (push "✓ Package signatures checked" ok)
      (push "⚠️  Package signatures not checked" warnings))
    
    ;; Check auth source
    (if (and (boundp 'auth-sources)
            (member "~/.authinfo.gpg" auth-sources))
        (push "✓ Encrypted auth source available" ok)
      (push "⚠️  No encrypted auth source configured" warnings))
    
    ;; Check custom file location
    (if (and (boundp 'custom-file)
            (not (string= custom-file user-init-file)))
        (push "✓ Custom file is separate" ok)
      (push "⚠️  Custom file should be separate from init.el" warnings))
    
    ;; Check GPG
    (if (executable-find "gpg")
        (push "✓ GPG is available" ok)
      (push "⚠️  GPG not found (needed for auth-source encryption)" warnings))
    
    ;; Display report
    (with-output-to-temp-buffer "*Security Audit*"
      (princ "=== SECURITY AUDIT ===\n\n")
      
      (when ok
        (princ "PASSED:\n")
        (dolist (item ok)
          (princ (format "  %s\n" item)))
        (princ "\n"))
      
      (if warnings
          (progn
            (princ "WARNINGS:\n")
            (dolist (item warnings)
              (princ (format "  %s\n" item))))
        (princ "No security warnings found.\n")))))

(provide 'emacs-ide-security)
;;; emacs-ide-security.el ends here