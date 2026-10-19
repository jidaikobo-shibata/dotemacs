;;; foreign-regexp-cl-lib-test.el --- Foreign regexp regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(setq load-prefer-newer t)
(defconst my/foreign-test-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(defconst my/foreign-test-original (getenv "FOREIGN_REGEXP_TEST_ORIGINAL"))
(defun my/foreign-test-reject-cl (feature &rest _)
  (when (memq feature '(cl cl-compat))
    (error "Obsolete dependency requested: %s" feature)))
(unless my/foreign-test-original
  (advice-add 'require :before #'my/foreign-test-reject-cl))
(add-to-list 'load-path
             (expand-file-name (or (getenv "FOREIGN_REGEXP_TEST_LIBRARY")
                                   (if my/foreign-test-original
                                       "elpa/foreign-regexp-20200325.50"
                                     "vendor/foreign-regexp"))
                               my/foreign-test-root))
(require 'foreign-regexp)
(foreign-regexp/regexp-type/set 'perl)

(defmacro my/with-foreign-test-buffer (contents &rest body)
  "Run BODY with CONTENTS in an isolated Perl-regexp buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (let ((case-fold-search nil)
           (foreign-regexp/dot-match-a-newline-p nil)
           (foreign-regexp/use-extended-regexp-p nil)
           (foreign-regexp/eval-replacement-p nil)
           (foreign-regexp/tmp-dir (make-temp-file "foreign-regexp-test-" t)))
       (unwind-protect (progn ,@body)
         (delete-directory foreign-regexp/tmp-dir t)))))

(ert-deftest foreign-cl-no-obsolete-dependency ()
  (progn
    (skip-unless (not my/foreign-test-original))
    (should-not (featurep 'cl))
    (should-not (featurep 'cl-compat))
    (should-not (fboundp 'loop))
    (should (string-prefix-p
             (file-truename (expand-file-name
                             (or (getenv "FOREIGN_REGEXP_TEST_LIBRARY") "vendor/foreign-regexp")
                             my/foreign-test-root))
             (file-truename (symbol-file 'foreign-regexp/search/forward))))))

(ert-deftest foreign-cl-perl-forward-backward-captures-and-unicode ()
  (my/with-foreign-test-buffer "日本語12\nabc34"
    (should (= (foreign-regexp/search/forward "(?<=日本語)(\\d+)") 6))
    (should (equal (match-string 1) "12"))
    (goto-char (point-max))
    (should (= (foreign-regexp/search/backward "[a-z]+(\\d+)") 7))
    (should (equal (match-string 1) "34"))))

(ert-deftest foreign-cl-search-bounds-count-and-failure ()
  (my/with-foreign-test-buffer "x1 x2 x3"
    (should (= (foreign-regexp/search/forward "x\\d" nil nil 2) 6))
    (should-not (foreign-regexp/search/forward "x\\d" 6 t))
    (should (= (point) 6))
    (should-error (foreign-regexp/search/forward "missing") :type 'search-failed)
    (goto-char (point-max))
    (should (= (foreign-regexp/search/backward "x\\d" nil nil 2) 4))))

(ert-deftest foreign-cl-search-options ()
  (my/with-foreign-test-buffer "Alpha\na\nb"
    (let ((case-fold-search t))
      (should (foreign-regexp/search/forward "alpha")))
    (goto-char (point-min))
    (let ((foreign-regexp/dot-match-a-newline-p t))
      (should (foreign-regexp/search/forward "a.b")))
    (goto-char (point-min))
    (let ((foreign-regexp/use-extended-regexp-p t))
      (should (foreign-regexp/search/forward "A l p h a # comment")))))

(ert-deftest foreign-cl-perl-replacement-and-region ()
  (my/with-foreign-test-buffer "outside1\n日本語12 abc34\noutside2"
    (foreign-regexp/replace/perform-replace
     "(\\d+)" "[${1}]" nil nil nil nil nil 10 21)
    (should (equal (buffer-string) "outside1\n日本語[12] abc[34]\noutside2"))))

(ert-deftest foreign-cl-query-replace-skip-and-act ()
  (my/with-foreign-test-buffer "a1 a2 a3"
    (let ((events (list ?n ?y ?!)))
      (cl-letf (((symbol-function 'read-event)
                 (lambda (&rest _) (or (pop events) (error "Unexpected prompt")))))
        (foreign-regexp/replace/perform-replace "a(\\d)" "b${1}" t nil nil)))
    (should (equal (buffer-string) "a1 b2 b3"))))

(ert-deftest foreign-cl-zero-width-replacement ()
  (my/with-foreign-test-buffer "ab"
    (foreign-regexp/replace/perform-replace "(?=b)" "!" nil nil nil)
    (should (equal (buffer-string) "a!b"))))

(ert-deftest foreign-cl-quote-meta ()
  (let ((quoted (foreign-regexp/quote-meta "日本語[a].+")))
    (my/with-foreign-test-buffer "日本語[a].+"
      (should (foreign-regexp/search/forward quoted))
      (should (equal (match-string 0) "日本語[a].+")))))

(ert-deftest foreign-cl-regexp-wrapper-identity-and-restoration ()
  (let ((pattern (copy-sequence "[0-9]+"))
        (other (copy-sequence "[0-9]+"))
        (original (symbol-function 're-search-forward)) calls)
    (cl-letf (((symbol-function 'foreign-regexp/search/forward)
               (lambda (&rest args) (push args calls) 'foreign-result)))
      (foreign-regexp/search/with-regarding-these-string-as-foreign-regexp
       ((list pattern) 10)
       (should (eq (re-search-forward pattern 7 t 2) 'foreign-result))
       (should (equal (car calls) (list pattern 7 t 2 10)))
       (with-temp-buffer
         (insert "123") (goto-char 1)
         (should (= (re-search-forward other) 4))))
      (should (eq original (symbol-function 're-search-forward)))
      (should-error
       (foreign-regexp/search/with-regarding-these-string-as-foreign-regexp
        ((list pattern)) (error "test unwind")))
      (should (eq original (symbol-function 're-search-forward))))))

(ert-deftest foreign-cl-isearch-hook-captures-pattern ()
  (progn
    (skip-unless (not my/foreign-test-original))
    (let ((isearch-mode-hook nil)
          (isearch-string "later") (isearch-message "later") callback)
      (let ((pattern "captured"))
        (setq callback (foreign-regexp/.make-isearch-setup-hook pattern)))
      (add-hook 'isearch-mode-hook callback)
      (cl-letf (((symbol-function 'isearch-push-state) #'ignore))
        (run-hooks 'isearch-mode-hook))
      (should (equal isearch-string "captured"))
      (should (equal isearch-message "captured"))
      (should-not (memq callback isearch-mode-hook)))))

(ert-deftest foreign-cl-option-hook-keeps-origin-and-prevents-recursion ()
  (progn
    (skip-unless (not my/foreign-test-original))
    (let ((origin (generate-new-buffer " *foreign-origin*"))
          (foreign-regexp/case-fold-will-change-hook nil)
          seen callback)
      (unwind-protect
          (progn
            (setq callback
                  (foreign-regexp/.make-option-hook
                   origin 'foreign-regexp/case-fold-will-change-hook
                   (lambda (flag)
                     (should flag)
                     (push (current-buffer) seen)
                     (run-hooks 'foreign-regexp/case-fold-will-change-hook))))
            (add-hook 'foreign-regexp/case-fold-will-change-hook callback)
            (cl-letf (((symbol-function 'foreign-regexp/.signal-option-changed) #'ignore))
              (funcall callback))
            (should (equal seen (list origin)))
            (should (memq callback foreign-regexp/case-fold-will-change-hook)))
        (kill-buffer origin)))))

(ert-deftest foreign-cl-minibuffer-transition-initial-contents ()
  (progn
    (skip-unless (not my/foreign-test-original))
    (let (seen)
      (cl-letf (((symbol-function 'call-interactively)
                 (lambda (command &rest _)
                   (setq seen (list command foreign-regexp/.initial-contents this-command)))))
        (foreign-regexp/.call-with-initial-contents "日本語(\\d+)" 'foreign-regexp/query-replace))
      (should (equal seen '(foreign-regexp/query-replace "日本語(\\d+)"
                            foreign-regexp/query-replace))))))

(ert-deftest foreign-cl-re-builder-isearch-handoff ()
  (let* ((target (generate-new-buffer " *foreign-reb-target*"))
         (builder (generate-new-buffer " *foreign-reb*"))
         (reb-buffer (buffer-name builder)) (reb-target-buffer target)
         (reb-re-syntax 'foreign-regexp) (isearch-mode-hook nil)
         (isearch-string "later") (isearch-message "later") callback)
    (unwind-protect
        (with-current-buffer builder
          (insert "日本語(\\d+)")
          (cl-letf (((symbol-function 'foreign-regexp/re-builder/assert-in-reb-buffer) #'ignore)
                    ((symbol-function 'foreign-regexp/isearch/assert-available) #'ignore)
                    ((symbol-function 'reb-quit) #'ignore)
                    ((symbol-function 'foreign-regexp/isearch-forward)
                     (lambda (&rest _) (setq callback (car isearch-mode-hook)))))
            (foreign-regexp/re-builder/isearch-forward-on-target-buffer))
          (cl-letf (((symbol-function 'isearch-push-state) #'ignore))
            (funcall callback))
          (should (equal isearch-string "日本語(\\d+)"))
          (should-not (memq callback isearch-mode-hook)))
      (when (buffer-live-p builder) (kill-buffer builder))
      (kill-buffer target))))

(ert-deftest foreign-cl-search-center-region-replacement ()
  (let ((original-map (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map (copy-keymap original-map))
          (add-to-list 'load-path (expand-file-name "inits" my/foreign-test-root))
          (require 'search-center)
          (should sc/is-foreign-regexp)
          (my/with-foreign-test-buffer "before a12 a34 after"
            (let ((mark-active t) (transient-mark-mode t))
              (set-mark 8) (goto-char 15)
              (sc/replace-region "a(\\d+)" "b${1}" t))
            (should (equal (buffer-string) "before b12 b34 after"))))
      (use-global-map original-map))))

(ert-deftest foreign-cl-repeat-search-dispatch ()
  (let ((menu-bar-last-search-type 'foreign-regexp)
        (foreign-regexp/history '("a(\\d+)")) calls)
    (cl-letf (((symbol-function 'foreign-regexp/search/forward)
               (lambda (pattern &rest _) (push (cons 'forward pattern) calls) 'next))
              ((symbol-function 'foreign-regexp/search/backward)
               (lambda (pattern &rest _) (push (cons 'backward pattern) calls) 'previous)))
      (should (eq (nonincremental-repeat-search-forward) 'next))
      (should (eq (nonincremental-repeat-search-backward) 'previous))
      (should (equal calls '((backward . "a(\\d+)") (forward . "a(\\d+)")))))))

(ert-deftest foreign-cl-repeat-search-delegates-to-original ()
  (skip-unless (not my/foreign-test-original))
  (let ((menu-bar-last-search-type 'string))
    (should (equal
             (foreign-regexp/nonincremental-repeat-search-forward
              (lambda (&rest args) args) 'one 'two)
             '(one two)))
    (should (equal
             (foreign-regexp/nonincremental-repeat-search-backward
              (lambda (&rest args) args) 'three)
             '(three)))))
