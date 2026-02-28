;;; emacs-ide-security.el --- Security Hardening -*- lexical-binding: t -*-
;;; Commentary:
;;; Enterprise security configuration with config integration.
;;; Version: 2.2.1
;;; Fixes:
;;;   - auth-sources: was using `let` binding then `setq auth-sources` which
;;;     assigned to the LOCAL let variable, not the global `auth-sources`.
;;;     The mapcar result was silently discarded. Fixed to setq the global directly.
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
;; FIX: Previously `(let ((auth-sources ...)) (setq auth-sources (mapcar ...))))`
;;      assigned to the local `let` variable, never touching the global.
;;      Now we compute and assign to the global `auth-sources` directly.
;; ============================================================================
(require 'auth-source)

(setq auth-sources
      (mapcar
       #'expand-file-name
       (or (and (boundp 'emacs-ide-config-data)
                (let* ((sec (cdr (assoc 'security emacs-ide-config-data)))
                       (sources (cdr (assoc 'auth-sources sec))))
                  (and (listp sources) sources)))
           '("~/.authinfo.gpg" "~/.netrc"))))

;; ============================================================================
;; NETWORK SECURITY
;; ============================================================================
(setq network-security-level
      (or (and (boundp 'emacs-ide-config-data)
               (let ((sec (cdr (assoc 'security emacs-ide-config-data))))
                 (cdr (assoc 'network-security-level sec))))
          'high))

;; ============================================================================
;; SECURITY AUDIT FUNCTION
;; ============================================================================
(defun emacs-ide-security-check ()
  "Run security audit with detailed reporting."
  (interactive)
  (let ((warnings '())
        (ok '()))

    ;; Check TLS
    (if (and (boundp 'gnutls-verify-error) gnutls-verify-error)
        (push "✓ TLS verification enabled" ok)
      (push "⚠️  TLS verification not enforced" warnings))

    ;; Check package signatures
    (if (bound-and-true-p package-check-signature)
        (push "✓ Package signatures checked" ok)
      (push "⚠️  Package signatures not checked" warnings))

    ;; Check auth source (expand-file-name already applied, so compare expanded)
    (if (and (boundp 'auth-sources)
             (member (expand-file-name "~/.authinfo.gpg") auth-sources))
        (push "✓ Encrypted auth source available" ok)
      (push "⚠️  No encrypted auth source configured" warnings))

    ;; Check custom file location
    (if (and (boundp 'custom-file)
             custom-file
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
        (dolist (item (nreverse ok))
          (princ (format "  %s\n" item)))
        (princ "\n"))
      (if warnings
          (progn
            (princ "WARNINGS:\n")
            (dolist (item (nreverse warnings))
              (princ (format "  %s\n" item))))
        (princ "No security warnings found.\n"))))

(provide 'emacs-ide-security)
;;; emacs-ide-security.el ends here
