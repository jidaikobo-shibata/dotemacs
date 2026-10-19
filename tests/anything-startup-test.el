;;; anything-startup-test.el --- Package activation and launcher tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defconst my/anything-startup-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(setq user-emacs-directory my/anything-startup-root
      load-prefer-newer t)
(defun my/anything-startup-require (original feature &rest args)
  (cond ((memq feature '(cl cl-compat))
         (error "Unexpected obsolete dependency: %s" feature))
        (t (apply original feature args))))
(advice-add 'require :around #'my/anything-startup-require)
(require 'package)
(package-initialize)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/global")
(add-to-list 'load-path (expand-file-name "inits" my/anything-startup-root))
(require 'search-center)
(require 'anything.init)
;; Never save history into the regular profile from tests.
(remove-hook 'kill-emacs-hook #'anything-c-adaptive-save-history)

(ert-deftest anything-startup-vendor-overrides-elpa ()
  (should (string-prefix-p (expand-file-name "vendor/anything/" my/anything-startup-root)
                           (symbol-file 'anything)))
  (should my/anything-available)
  (should my/foeb-available)
  (should (featurep 'anything-gtags))
  (should-not (featurep 'cl)))

(ert-deftest anything-startup-launcher-and-keybindings ()
  (should (equal my/anything-sources
                 '(foeb/anything-c-source-buffers
                   anything-c-source-my-dired-buffer
                   anything-c-source-bookmarks
                   anything-c-source-my-hosts
                   anything-c-source-my-ftp-hosts
                   anything-c-source-recentf
                   anything-c-source-search-center-presets
                   anything-c-source-coding-system)))
  (dolist (key '("C-;" "M-s-<left>" "M-s-<right>"))
    (should (eq (key-binding (kbd key)) 'my-anything-launcher)))
  (dolist (binding '(("<tab>" . anything-select-action)
                     ("C-;" . anything-keyboard-quit)
                     ("C-d" . foeb/anything-execute-persistent-kill)
                     ("s-w" . foeb/anything-execute-persistent-kill)))
    (should (eq (lookup-key anything-map (kbd (car binding))) (cdr binding))))
  (cl-letf (((symbol-function 'anything-other-buffer)
             (lambda (sources buffer)
               (should (equal sources my/anything-sources))
               (should (equal buffer "*my-anything-launcher*"))
               'opened)))
    (should (eq (my-anything-launcher) 'opened))))

(ert-deftest anything-startup-local-candidate-sources ()
  ;; Exercise local sources without reading real SSH/FTP files or connecting.
  (cl-letf (((symbol-function 'insert-file-contents)
             (lambda (file &rest _)
               (insert (if (string-match-p "ssh" file)
                           "Host example\n  HostName example.invalid\nHost *\n"
                         "Alias Example\nPath /ftp:example.invalid:/\nPassword dummy\n")))))
    (let ((tramp-default-method "ssh"))
      (should (equal (anything-c-source-my-hosts-candidates) '("/ssh:example:"))))
    (should (equal (anything-c-source-my-ftp-hosts-candidates)
                   '("Example  /ftp:example.invalid:/  dummy"))))
  (should (equal (funcall (cdr (assq 'candidates anything-c-source-coding-system)))
                 '("set UTF-8" "set EUC-JP" "set Shift-JIS" "set ISO-2022-JP"
                   "set LF" "set CR" "set CR+LF")))
  (should (equal (funcall (cdr (assq 'candidates anything-c-source-search-center-presets)))
                 (sc/preset-names))))
