;;; editor-tabs-test.el --- Editable tabs regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(defvar my/tabs-test-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(add-to-list 'load-path (expand-file-name "inits" my/tabs-test-root))
(add-to-list 'load-path
             (expand-file-name "elisp/focus-on-editable-buffers" my/tabs-test-root))
(require 'editor-tabs.init)

(defmacro my/with-test-tabs (&rest body)
  "Run BODY with isolated test buffers and restore tabs afterwards."
  (declare (indent 0) (debug t))
  `(let ((a (generate-new-buffer "tabs-a"))
         (b (generate-new-buffer "tabs-b"))
         (directory (generate-new-buffer "tabs-directory"))
         (internal (generate-new-buffer "*tabs-hidden*"))
         (foeb/non-ignore-buffers "\\`\\*scratch\\*\\'"))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer directory (setq major-mode 'dired-mode))
           (my/editor-tabs-mode 1)
           (setq my/editor-tabs--buffers (list a b))
           ,@body)
       (my/editor-tabs-mode -1)
       (dolist (buffer (list a b directory internal))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))))))

(ert-deftest editor-tabs-filter-and-stable-order ()
  (my/with-test-tabs
    (let ((before (copy-sequence (my/editor-tabs--targets))))
      (should (memq a before))
      (should-not (memq directory before))
      (should-not (memq internal before))
      (switch-to-buffer b)
      (switch-to-buffer a)
      (should (equal before (my/editor-tabs--targets)))
      (let ((new (generate-new-buffer "tabs-new")))
        (unwind-protect
            (should (equal (my/editor-tabs--targets) (append before (list new))))
          (kill-buffer new))))))

(ert-deftest editor-tabs-share-list-and-select-in-current-pane ()
  (my/with-test-tabs
    (delete-other-windows)
    (switch-to-buffer a)
    (let* ((left (selected-window))
           (right (split-window-right))
           (tabs (my/editor-tabs--tabs)))
      (set-window-buffer right directory)
      (with-selected-window right
        (should (equal (mapcar (lambda (tab) (alist-get 'buffer tab)) tabs)
                       (mapcar (lambda (tab) (alist-get 'buffer tab))
                               (my/editor-tabs--tabs))))
        (should-not (cl-some (lambda (tab) (alist-get 'selected tab))
                             (my/editor-tabs--tabs)))
        (should tab-line-mode)
        (my/editor-tabs-next)
        (should (eq (current-buffer) a))
        (foeb/switch-to-next-buffer)
        (should (eq (current-buffer) b)))
      (should (eq (window-buffer left) a)))))

(ert-deftest editor-tabs-close-confirmation-and-neighbour ()
  (my/with-test-tabs
    (switch-to-buffer a)
    (with-current-buffer a (insert "unsaved"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (my/editor-tabs--close a))
    (should (buffer-live-p a))
    (should (buffer-modified-p a))
    (should (eq (current-buffer) a))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (my/editor-tabs--close a))
    (should-not (buffer-live-p a))
    (should (eq (current-buffer) b))
    (should-not (memq a (my/editor-tabs--targets)))))

(ert-deftest editor-tabs-close-background-tab-preserves-dired ()
  (my/with-test-tabs
    (switch-to-buffer directory)
    (funcall (alist-get 'close
                       (cl-find b (my/editor-tabs--tabs)
                                :key (lambda (tab) (alist-get 'buffer tab)))))
    (should-not (buffer-live-p b))
    (should (eq (current-buffer) directory))))

(ert-deftest editor-tabs-toggle-restores-settings ()
  (let ((before tab-line-tabs-function)
        (bindings (mapcar (lambda (key) (cons key (key-binding (kbd key))))
                          '("C-M-<left>" "C-M-<right>"
                            "M-s-<left>" "M-s-<right>" "C-t" "s-t")))
        (global-before global-tab-line-mode))
    (my/with-test-tabs
      (my/editor-tabs-mode 1)
      (dolist (key '("C-M-<left>"))
        (should (eq (key-binding (kbd key)) #'my/editor-tabs-previous)))
      (dolist (key '("C-M-<right>"))
        (should (eq (key-binding (kbd key)) #'my/editor-tabs-next)))
      (dolist (key '("C-t"))
        (should (eq (key-binding (kbd key)) #'my/editor-tabs-new)))
      ;; Anything and Super shortcuts keep their original bindings even on tabs.
      (dolist (key '("M-s-<left>" "M-s-<right>" "s-t"))
        (should (equal (cdr (assoc key bindings)) (key-binding (kbd key)))))
      (should (eq tab-line-tabs-function #'my/editor-tabs--tabs)))
    (dolist (binding bindings)
      (should (equal (cdr binding) (key-binding (kbd (car binding))))))
    (should (eq before tab-line-tabs-function))
    (should (eq global-before global-tab-line-mode))
    (should-not (advice-member-p #'my/editor-tabs--neighbour
                                'foeb/switch-to-neighbour-buffer))))

(ert-deftest editor-tabs-new-opens-distinct-unsaved-tabs-at-end ()
  (my/with-test-tabs
    (switch-to-buffer directory)
    (let ((expected-directory default-directory)
          (before (copy-sequence (my/editor-tabs--targets)))
          created)
      (unwind-protect
          (progn
            (dotimes (_ 2)
              (call-interactively (key-binding (kbd "C-t")))
              (push (current-buffer) created)
              (should-not buffer-file-name)
              (should (= (buffer-size) 0))
              (should (eq major-mode 'text-mode))
              (should (equal default-directory expected-directory)))
            (should-not (eq (car created) (cadr created)))
            (should (equal (my/editor-tabs--targets)
                           (append before (reverse created)))))
        (mapc #'kill-buffer created)))))

(ert-run-tests-batch-and-exit)
