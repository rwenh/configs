;;; emacs-ide-security.el --- Security Hardening -*- lexical-binding: t -*-
;;; Commentary:
;;; Enterprise security configuration with config integration.
;;; Version: 3.0.4
;;; Code:

(require 'cl-lib)

(defconst emacs-ide-security--tls-verify-default       t)
(defconst emacs-ide-security--prime-bits-default        3072)
(defconst emacs-ide-security--pkg-sig-default          'allow-unsigned)
(defconst emacs-ide-security--net-sec-default          'high)
(defconst emacs-ide-security--auth-sources-default
  '("~/.authinfo.gpg" "~/.netrc"))

(defun emacs-ide-security--apply ()
  "Apply security settings from config or fall back to safe defaults."
  (let* ((cfg-get (lambda (key default)
                    (if (fboundp 'emacs-ide-config-get)
                        (emacs-ide-config-get 'security key default)
                      default)))
         (tls-verify  (funcall cfg-get 'tls-verify
                               emacs-ide-security--tls-verify-default))
         (prime-bits  (funcall cfg-get 'tls-min-prime-bits
                               emacs-ide-security--prime-bits-default))
         (check-sigs  (funcall cfg-get 'package-signatures
                               emacs-ide-security--pkg-sig-default))
         (net-sec     (funcall cfg-get 'network-security-level
                               emacs-ide-security--net-sec-default))
         (raw-sources (if (fboundp 'emacs-ide-config-get)
                          (emacs-ide-config-get 'security 'auth-sources nil)
                        nil))
         (sources     (if (and (listp raw-sources) raw-sources)
                          raw-sources
                        emacs-ide-security--auth-sources-default)))

    (require 'gnutls)
    (setq gnutls-verify-error tls-verify)

    (when (boundp 'gnutls-min-prime-bits)
      (setq gnutls-min-prime-bits prime-bits))

    (setq tls-checktrust t)
    (setq tls-program '("gnutls-cli --x509cafile %t -p %p %h"))

    (setq package-check-signature check-sigs)

    (require 'auth-source)
    (setq auth-sources
          (mapcar (lambda (entry)
                    (if (stringp entry)
                        (expand-file-name entry)
                      entry))
                  sources))

    (setq network-security-level net-sec)))

(emacs-ide-security--apply)

(with-eval-after-load 'emacs-ide-config
  (emacs-ide-security--apply))

(defun emacs-ide-security-harden ()
  "Re-apply all security settings from config.yml."
  (interactive)
  (emacs-ide-security--apply)
  (message "✓ Security settings re-applied"))

(defun emacs-ide-security-check ()
  "Run security audit with detailed reporting."
  (interactive)
  (let ((warnings '())
        (ok '()))

    (if (and (boundp 'gnutls-verify-error) gnutls-verify-error)
        (push "✓ TLS verification enabled" ok)
      (push "⚠️  TLS verification not enforced" warnings))

    (if (and (boundp 'tls-program)
             (listp tls-program)
             (cl-some (lambda (p) (string-match-p "gnutls-cli" p)) tls-program))
        (push "✓ TLS program set to gnutls-cli" ok)
      (push "⚠️  tls-program may be using insecure openssl s_client default" warnings))

    (if (boundp 'gnutls-min-prime-bits)
        (if (>= gnutls-min-prime-bits 2048)
            (push (format "✓ gnutls-min-prime-bits = %d (strong)" gnutls-min-prime-bits) ok)
          (push (format "⚠️  gnutls-min-prime-bits = %d (weak, recommend >= 2048)"
                        gnutls-min-prime-bits) warnings))
      (push "✓ gnutls-min-prime-bits not applicable (Emacs 29+ uses GnuTLS priority string)" ok))

    (if (bound-and-true-p package-check-signature)
        (push "✓ Package signatures checked" ok)
      (push "⚠️  Package signatures not checked" warnings))

    (if (and (boundp 'auth-sources)
             (cl-some (lambda (s)
                        (and (stringp s)
                             (or (string-suffix-p ".gpg" s)
                                 (string-suffix-p ".age" s))))
                      auth-sources))
        (push "✓ Encrypted auth source configured" ok)
      (push "⚠️  No encrypted auth source (.gpg/.age) configured" warnings))

    (if (and (stringp custom-file)
             (not (string= custom-file user-init-file)))
        (push "✓ Custom file is separate" ok)
      (push "⚠️  Custom file should be separate from init.el" warnings))

    (if (executable-find "gpg")
        (push "✓ GPG is available" ok)
      (push "⚠️  GPG not found (needed for auth-source encryption)" warnings))

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
