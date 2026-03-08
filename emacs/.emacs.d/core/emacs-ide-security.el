;;; emacs-ide-security.el --- Security Hardening -*- lexical-binding: t -*-
;;; Commentary:
;;; Enterprise security configuration with config integration.
;;; Version: 2.2.3
;;; Fixes:
;;;   - 2.2.3: tls-program now set here (was only in early-init.el). If
;;;     early-init is bypassed (e.g. emacs -Q followed by manual load, or
;;;     package load during testing) the insecure openssl s_client default
;;;     remained. Security.el is the canonical place for TLS policy.
;;;   - 2.2.3: auth-sources: mapcar expand-file-name over a mixed list that
;;;     may contain plists or keyword-based entries (macos-keychain-internet,
;;;     (:source "pass:...")) would corrupt those entries.  Now only string
;;;     entries are expanded; non-string entries are passed through unchanged.
;;;   - 2.2.2: TLS (with-eval-after-load 'gnutls) replaced with (require
;;;     'gnutls) so settings apply before any network call.
;;;   - 2.2.1: auth-sources local let variable assignment bug fixed.
;;; Code:

;; ============================================================================
;; TLS CONFIGURATION
;; FIX 2.2.2: (require 'gnutls) so TLS settings are applied before any
;;   network call. The (with-eval-after-load 'gnutls ...) form in earlier
;;   versions meant the first network call (straight bootstrap, package
;;   refresh) ran with Emacs's default unverified TLS settings.
;; FIX 2.2.3: tls-program also set here (was only in early-init.el) so this
;;   module is self-contained and correct when early-init is bypassed.
;; ============================================================================
(require 'gnutls)

(let ((tls-verify (or (and (boundp 'emacs-ide-config-data)
                           (let ((sec (cdr (assoc 'security emacs-ide-config-data))))
                             (cdr (assoc 'tls-verify sec))))
                      t)))
  (setq gnutls-verify-error   tls-verify
        gnutls-min-prime-bits (or (and (boundp 'emacs-ide-config-data)
                                       (let ((sec (cdr (assoc 'security emacs-ide-config-data))))
                                         (cdr (assoc 'tls-min-prime-bits sec))))
                                  3072)
        tls-checktrust         t
        ;; FIX 2.2.3: set tls-program here, not only in early-init.el
        tls-program            '("gnutls-cli --x509cafile %t -p %p %h")))

;; ============================================================================
;; PACKAGE SIGNATURE CHECKING
;; ============================================================================
(let ((check-sigs (or (and (boundp 'emacs-ide-config-data)
                           (let ((sec (cdr (assoc 'security emacs-ide-config-data))))
                             (cdr (assoc 'package-signatures sec))))
                      'allow-unsigned)))
  (setq package-check-signature check-sigs))

;; ============================================================================
;; AUTH-SOURCE CONFIGURATION
;; FIX 2.2.3: auth-sources may contain non-string entries such as
;;   macos-keychain-internet, macos-keychain-generic, or plist forms like
;;   (:source "pass:..."). Applying expand-file-name to those corrupts them
;;   (expand-file-name coerces plists to strings via prin1-to-string, and
;;   symbol names like macos-keychain-internet become bogus paths).
;;   Now only string entries are expanded; all other entries pass through.
;; FIX 2.2.1: assignment was to a local let variable, not the global.
;; ============================================================================
(require 'auth-source)

(setq auth-sources
      (mapcar
       (lambda (entry)
         (if (stringp entry)
             (expand-file-name entry)
           entry))
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

    ;; Check tls-program is set to gnutls-cli (not insecure openssl default)
    (if (and (boundp 'tls-program)
             (listp tls-program)
             (cl-some (lambda (p) (string-match-p "gnutls-cli" p)) tls-program))
        (push "✓ TLS program set to gnutls-cli" ok)
      (push "⚠️  tls-program may be using insecure openssl s_client default" warnings))

    ;; Check package signatures
    (if (bound-and-true-p package-check-signature)
        (push "✓ Package signatures checked" ok)
      (push "⚠️  Package signatures not checked" warnings))

    ;; Check auth source (expand-file-name already applied to string entries)
    (if (and (boundp 'auth-sources)
             (cl-some (lambda (s)
                        (and (stringp s)
                             (string= s (expand-file-name "~/.authinfo.gpg"))))
                      auth-sources))
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
        (princ "No security warnings found.\n")))))

(provide 'emacs-ide-security)
;;; emacs-ide-security.el ends here
