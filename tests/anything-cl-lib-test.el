;;; anything-cl-lib-test.el --- Anything migration regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(setq load-prefer-newer t)
(defvar gtags-select-buffer)

(defconst my/anything-test-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defconst my/anything-test-original (getenv "ANYTHING_TEST_ORIGINAL"))

(defun my/anything-test-reject-old-cl (feature &rest _)
  "Reject any attempt to load the obsolete FEATURE."
  (when (memq feature '(cl cl-compat))
    (error "Unexpected obsolete dependency: %s" feature)))
(unless my/anything-test-original
  (advice-add 'require :before #'my/anything-test-reject-old-cl))

(add-to-list 'load-path
             (expand-file-name (or (getenv "ANYTHING_TEST_LIBRARY")
                                   (if my/anything-test-original
                                       "elpa/anything-20170125.1710"
                                     "vendor/anything"))
                               my/anything-test-root))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/global")
(require 'anything-gtags)
(add-to-list 'load-path (expand-file-name "elisp/focus-on-editable-buffers"
                                        my/anything-test-root))
(require 'focus-on-editable-buffers)
(require 'anything-focus-on-editable-buffers)

(ert-deftest anything-cl-no-obsolete-dependency ()
  (unless my/anything-test-original
    (should-not (featurep 'cl))
    (should-not (featurep 'cl-compat))
    (should-not (fboundp 'loop))
    (dolist (feature '(anything anything-config anything-match-plugin anything-gtags))
      (should (string-prefix-p
               (file-truename (expand-file-name
                               (or (getenv "ANYTHING_TEST_LIBRARY") "vendor/anything")
                               my/anything-test-root))
               (file-truename (symbol-file feature 'provide)))))))

(ert-deftest anything-cl-candidate-limit-and-deduplication ()
  ;; Both nested dolist loops must still support early cl-return.
  (let ((anything-pattern "a"))
    (should (equal (anything-match-from-candidates
                    '("alpha" "beta" "alpha" "gamma")
                    (list (lambda (s) (string-match-p "a" s))
                          (lambda (s) (string-match-p "m" s))) 2)
                   '("alpha" "beta")))
    (should (equal (anything-match-from-candidates
                    '("alpha" "beta" "alpha" "gamma")
                    (list (lambda (s) (string-match-p "a" s))) 10)
                   '("alpha" "beta" "gamma")))))

(ert-deftest anything-cl-multiple-regexp-matching ()
  (should (anything-mp-3-match "foo--bar" "bar foo"))
  (should (anything-mp-3-match "foo--baz" "foo !bar"))
  (should-not (anything-mp-3-match "foo--bar" "foo !bar"))
  (should-not (anything-mp-3-match "foo" "foo bar")))

(ert-deftest anything-cl-quoted-action-retains-dynamic-binding ()
  ;; A plain cl-flet would miss the quoted action-fn in these sources.
  (let ((original (and (fboundp 'action-fn) (symbol-function 'action-fn))))
    (cl-letf (((symbol-function 'anything)
               (lambda (&rest args)
                 (let* ((source (cadr (plist-get args :sources)))
                        (action (cdr (assq 'action source))))
                   (should (eq action 'action-fn))
                   (funcall action "chosen")))))
      (should (equal (anything-comp-read "Choose: " '("chosen")) "chosen")))
    (should (eq original (and (fboundp 'action-fn) (symbol-function 'action-fn))))))

(ert-deftest anything-cl-match-plugin-toggle ()
  (let ((anything-compile-source-functions
         '(anything-compile-source--match-plugin))
        (anything-mp-highlight-delay 0.2)
        (anything-mp-initial-highlight-delay nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (anything-mp-toggle-match-plugin)
      (should-not (memq 'anything-compile-source--match-plugin
                        anything-compile-source-functions))
      (anything-mp-toggle-match-plugin)
      (should (memq 'anything-compile-source--match-plugin
                    anything-compile-source-functions))
      (should (= anything-mp-highlight-delay 0.2)))))

(ert-deftest anything-cl-headline-helper-dynamic-calls ()
  (with-temp-buffer
    (insert "# One\n## Two\n# Three\n")
    (let ((anything-current-buffer (current-buffer)))
      (should (equal (mapcar #'car
                            (anything-headline-get-candidates
                             '("^# \\(.*\\)$" "^## \\(.*\\)$") 1))
                     '("H1:One" "H2:One / Two" "H1:Three"))))))

(ert-deftest anything-cl-delayed-visible-marks-retain-capture ()
  (let ((buffer (generate-new-buffer " *anything-marks-test*"))
        callback)
    (unwind-protect
        (with-current-buffer buffer
          (insert "one two")
          (let* ((a (make-overlay 1 4)) (b (make-overlay 5 8))
                 (anything-buffer (buffer-name buffer))
                 (anything-visible-mark-overlays (list b a)))
            (overlay-put a 'string "one")
            (overlay-put b 'string "two")
            (cl-letf (((symbol-function 'anything-run-after-quit)
                       (lambda (fn &rest _) (setq callback fn))))
              (save-window-excursion
                (switch-to-buffer buffer)
                (anything-display-all-visible-marks)))
            (setq anything-visible-mark-overlays nil)
            (funcall callback)
            (should (equal (with-current-buffer "*anything visible marks*"
                             (buffer-string)) "onetwo\n"))))
      (kill-buffer buffer)
      (when (get-buffer "*anything visible marks*")
        (kill-buffer "*anything visible marks*")))))

(ert-deftest anything-cl-process-sentinel-retains-command ()
  (let ((anything-c-external-commands-list '("one" "two"))
        (anything-raise-command "raise %s")
        sentinels calls)
    (cl-letf (((symbol-function 'get-process) (lambda (_) nil))
              ((symbol-function 'start-process-shell-command) (lambda (&rest _) nil))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ sentinel) (push sentinel sentinels)))
              ((symbol-function 'anything-c-get-pid-from-process-name)
               (lambda (name) (push name calls) nil))
              ((symbol-function 'shell-command) (lambda (&rest _) nil)))
      (anything-run-or-raise "one")
      (anything-run-or-raise "two")
      (mapc (lambda (fn) (funcall fn 'test-process "finished\n")) sentinels)
      (should (equal calls '("one" "two"))))))

(ert-deftest anything-cl-gtags-classification ()
  (with-temp-buffer
    (insert "foo 10 a.c definition\nbar 20 b.c definition\nbaz 30 b.c definition\n")
    (let ((gtags-select-buffer (current-buffer)) sources)
      (unwind-protect
          (cl-letf (((symbol-function 'aggs-set-anything-current-position) #'ignore)
                    ((symbol-function 'anything-set-sources)
                     (lambda (value &rest _) (setq sources value)))
                    ((symbol-function 'anything-funcall-foreach) #'ignore))
            (aggs-meta-source-init)
            (should (equal (mapcar (lambda (s) (cdr (assq 'name s))) sources)
                           '("a.c" "b.c")))
            (should (equal (with-current-buffer (aggs-candidate-buffer-by-filename "b.c")
                             (buffer-string))
                           "bar 20 b.c definition\nbaz 30 b.c definition\n")))
        (dolist (name '("a.c" "b.c"))
          (when-let ((buffer (get-buffer (concat "*anything gtags*" name))))
            (kill-buffer buffer)))))))

(ert-deftest anything-cl-editable-buffer-actions ()
  (let ((buffer (generate-new-buffer "anything-cl-editable")))
    (unwind-protect
        (let* ((source foeb/anything-c-source-buffers)
               (candidates (funcall (cdr (assq 'candidates source)))))
          (should (member (buffer-name buffer) candidates))
          (save-window-excursion
            (funcall (cdr (assq 'action source)) (buffer-name buffer))
            (should (eq (current-buffer) buffer)))
          (cl-letf (((symbol-function 'anything-get-selection)
                     (lambda (&rest _) (buffer-name buffer)))
                    ((symbol-function 'anything-update) #'ignore))
            (funcall (cdr (assq 'persistent-action-2 source)) (buffer-name buffer)))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest anything-cl-asynchronous-copy-child-script ()
  ;; The child Emacs program is embedded in a string, outside normal macros.
  (let* ((directory (make-temp-file "anything-copy-test-" t))
         (source (expand-file-name "source.txt" directory))
         (destination (expand-file-name "destination/" directory))
         (anything-c-copy-files-async-log-file (expand-file-name "copy.log" directory))
         script)
    (unwind-protect
        (progn
          (make-directory destination)
          (with-temp-file source (insert "copy contents"))
          (dotimes (round 2)
            (cl-letf (((symbol-function 'start-file-process)
                       (lambda (&rest args) (setq script (car (last args))))))
              (anything-c-copy-files-async-1 (list source) destination))
            (with-temp-buffer
              (should
               (zerop (call-process
                       (expand-file-name invocation-name invocation-directory)
                       nil t nil "--batch" "-Q" "--eval"
                       (if my/anything-test-original script
                         (concat
                          "(progn (advice-add 'require :before "
                          "(lambda (feature &rest _) "
                          "(when (memq feature '(cl cl-compat)) "
                          "(error \"Obsolete child dependency\")))) "
                          script ")"))))))
            (should (equal (with-temp-buffer
                             (insert-file-contents
                              (expand-file-name "source.txt" destination))
                             (buffer-string)) "copy contents"))
            (should (string-match-p
                     (if (zerop round) "1 File(s) Copied" "1 File(s) Overwrited")
                     (with-temp-buffer
                       (insert-file-contents anything-c-copy-files-async-log-file)
                       (buffer-string))))))
      (delete-directory directory t))))
