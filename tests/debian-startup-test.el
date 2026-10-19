;;; debian-startup-test.el --- Debian package startup compatibility -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bytecomp)

(defconst my/debian-test-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

;; Represent the already-compiled, lexically bound Debian function that caused
;; the error, with a separately loaded snippet reading the dynamic value.
(defun my/debian-test-snippet-flavor ()
  "Read the dynamic variable expected by the Mozc package snippet."
  (symbol-value 'flavor))

(defconst my/debian-test-lexical-startup
  (byte-compile '(lambda (flavor) (list flavor (my/debian-test-snippet-flavor)))))

(load (or (getenv "DEBIAN_TEST_EARLY_INIT")
          (expand-file-name "early-init.el" my/debian-test-root)) nil t)

(ert-deftest debian-startup-compiled-lexical-argument ()
  (skip-unless (fboundp 'my/debian-startup-with-flavor))
  (should-not (boundp 'flavor))
  (should-error (funcall my/debian-test-lexical-startup 'emacs)
                :type 'void-variable)
  (should (equal (my/debian-startup-with-flavor
                  my/debian-test-lexical-startup 'emacs)
                 '(emacs emacs)))
  (should-not (boundp 'flavor)))

(ert-deftest debian-startup-preserves-flavor-and-unwinds-errors ()
  (skip-unless (fboundp 'my/debian-startup-with-flavor))
  (cl-progv '(flavor) '(outer)
    (should (equal (my/debian-startup-with-flavor
                    my/debian-test-lexical-startup 'another-emacs)
                   '(another-emacs another-emacs)))
    (should (eq (symbol-value 'flavor) 'outer))
    (should-error (my/debian-startup-with-flavor
                   (lambda (_)
                     (should (eq (symbol-value 'flavor) 'emacs))
                     (error "Test failure"))
                   'emacs))
    (should (eq (symbol-value 'flavor) 'outer))))

(ert-deftest debian-startup-loads-system-snippets ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (file-readable-p "/usr/share/emacs/site-lisp/debian-startup.el")
                    (file-readable-p "/etc/emacs/site-start.d/50emacs-mozc.el")))
  (let ((load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
        (original-message (symbol-function 'message)) messages)
    (load "/usr/share/emacs/site-lisp/debian-startup.el" nil t)
    (should (advice-member-p #'my/debian-startup-with-flavor 'debian-startup))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (when format-string
                   (push (apply #'format-message format-string args) messages))
                 (apply original-message format-string args))))
      (debian-startup 'emacs))
    (should-not (cl-find-if (lambda (line) (string-match-p "Error while loading" line))
                            messages))
    (should (member "/usr/share/emacs/site-lisp/emacs-mozc" load-path))
    (should (locate-library "mozc"))
    (should (fboundp 'gtags-mode))
    (should-not (boundp 'flavor))))
