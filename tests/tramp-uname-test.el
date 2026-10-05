(require 'ert)
(require 'cl-lib)
(setq user-emacs-directory (make-temp-file "tramp-check-" t))
(defvar my/auto-save-directory user-emacs-directory)
(load (expand-file-name "../inits/tramp.init.el"
                        (file-name-directory load-file-name)) nil t)
(require 'tramp-sh)
(setq my/tramp-ssh-config-file
      (expand-file-name "ssh-config" user-emacs-directory))
(with-temp-file my/tramp-ssh-config-file
  (insert "Host miyakoanshinsumai.com\n HostName ssh-miyakosumai.heteml.net\n"
          "Host other-heteml\n HostName ssh-other.heteml.net\n"
          "Host misleading\n HostName ssh-other.heteml.net.example.com\n"
          "Host *\n CanonicalizeHostname no\n"))
(defconst my/test-uname-command "echo \\\"`uname -sr`\\\"")
(ert-deftest heteml-host-and-alias-detection ()
  (dolist (host '("ssh-example.heteml.net" "SSH-EXAMPLE.HETEML.NET"
                  "ssh-example.heteml.net." "miyakoanshinsumai.com"
                  "other-heteml"))
    (should (my/tramp-heteml-host-p host)))
  (dolist (host '("example.com" "misleading" "fakeheteml.net"
                  "ssh-example.heteml.net.example.com"))
    (should-not (my/tramp-heteml-host-p host))))
(ert-deftest fallback-scope ()
  (dolist (method '("ssh" "scp"))
   (dolist (host '("miyakoanshinsumai.com" "other-heteml"
                   "ssh-example.heteml.net"))
    (let* ((vec (tramp-dissect-file-name
                 (format "/%s:%s:/" method host)))
           (args (list vec my/test-uname-command t "marker"))
           (result (my/tramp-uname-fallback-args args)))
      (should-not (equal (cadr result) my/test-uname-command))
      (should (equal (cddr result) '(t "marker")))
      (should (eq (car result) vec))))))
(ert-deftest unchanged-commands-and-hosts ()
  (dolist (path '("/ssh:example.com:/" "/scp:example.com:/"
                  "/ssh:miyakoanshinsumai.com.example.com:/"))
    (let ((args (list (tramp-dissect-file-name path) my/test-uname-command)))
      (should (eq (my/tramp-uname-fallback-args args) args))))
  (let ((args (list (tramp-dissect-file-name "/scp:miyakoanshinsumai.com:/")
                    "ls /does-not-exist")))
    (should (eq (my/tramp-uname-fallback-args args) args))))
(ert-deftest missing-and-existing-uname ()
  (let* ((vec (tramp-dissect-file-name "/scp:miyakoanshinsumai.com:/"))
         (command (cadr (my/tramp-uname-fallback-args
                         (list vec my/test-uname-command))))
         (process-environment (copy-sequence process-environment)))
    (setenv "PATH" "/nonexistent")
    (should (equal (read (shell-command-to-string command))
                   "Linux (uname unavailable)"))
    (setenv "PATH" "/usr/bin:/bin")
    (should (equal (read (shell-command-to-string command))
                   (string-trim (shell-command-to-string "uname -sr"))))))
(ert-run-tests-batch-and-exit)
