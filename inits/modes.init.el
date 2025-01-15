;;; modes.init.el --- init for modes
;;; Commentary:
;; provide modes.init.
;;; Code:

;;; ------------------------------------------------------------
;; 設定ファイル用のメジャーモード
;; (require 'generic-x)

;;; ------------------------------------------------------------
;;; which-key-mode

(which-key-mode 1)
(which-key-setup-side-window-bottom)

;;; ------------------------------------------------------------
;;; sh-script-mode

(setq auto-mode-alist
      (append '(("^\\." . sh-script-mode))
              auto-mode-alist))

(add-hook 'sh-script-mode-hook
          #'(lambda ()
             (setq sh-basic-offset 2)
             (setq indent-tabs-mode nil)
             (setq sh-indentation 2)))

;;; ------------------------------------------------------------
;;; text-mode

(add-hook 'text-mode-hook
          #'(lambda()
             (font-lock-add-keywords nil '(("^# .+" . font-lock-comment-face)))
             (font-lock-add-keywords nil '(("^//.+" . font-lock-comment-face)))
             (font-lock-add-keywords nil '(("^■.+" . font-lock-comment-face)))
             (font-lock-add-keywords nil '(("^●.+" . font-lock-builtin-face)))
             (font-lock-add-keywords nil '(("^○.+" . font-lock-keyword-face)))
             (font-lock-add-keywords nil '(("^> .+" . font-lock-keyword-face)))
             (font-lock-add-keywords nil '(("^>> .+" .font-lock-type-face)))
             (font-lock-add-keywords nil '(("^>>>.+" . font-lock-string-face)))))

;;; ------------------------------------------------------------
;;; kontiki-mode - ワイアフレームモード

(easy-mmode-define-minor-mode kontiki-mode
                              "This is a Mode for Kontiki-Draft."
                              :init-value nil
                              :lighter " Kontiki-Draft")

(add-hook 'kontiki-mode-hook
          #'(lambda()
             (font-lock-add-keywords nil '(("^//.+" . font-lock-comment-face)))
             (font-lock-add-keywords nil '(("<.+?>" . font-lock-keyword-face)))
             (font-lock-add-keywords nil '(("\\[memo:.+?\\]" . font-lock-builtin-face)))
             (font-lock-add-keywords nil '(("^[a-zA-Z_]+?:" . font-lock-function-name-face)))
             (font-lock-add-keywords nil '(("^\\*.+" . font-lock-function-name-face)))))

;;; ------------------------------------------------------------
;;; mail-mode

(add-hook 'mail-mode-hook
          #'(lambda()
             (font-lock-add-keywords nil '(("^> .+" . font-lock-keyword-face)))
             (font-lock-add-keywords nil '(("^>> .+" .font-lock-type-face)))
             (font-lock-add-keywords nil '(("^>>>.+" . font-lock-string-face)))))

;;; ------------------------------------------------------------
;;; grep-mode

(add-hook 'grep-mode-hook
          #'(lambda()
             (define-key grep-mode-map (kbd "C-o")
               (lambda () (interactive) (other-window 1)))
             (define-key grep-mode-map (kbd "C-S-o")
               (lambda () (interactive) (other-window -1)))))

;;; ------------------------------------------------------------
;;; html-mode

(add-hook 'html-mode-hook
          #'(lambda()
             (setq-local syntax-propertize-function
                         (syntax-propertize-rules
                          ;; xoops smarty comment-out
                          ("\\(<\\){\\*" (1 "< c"))
                          ("\\*}\\(>\\)" (1 "> c"))
                          ;; html comment-out ; it seems wired and buggy...
                          ("\\(<\\)!--" (1 "< c"))
                          ("--[ \t\n]*\\(>\\)" (1 "> c"))
                          ))
             ;; xoops - peak smarty
             (font-lock-add-keywords
              nil
              '(("<{.+?}>" . font-lock-keyword-face)))
             (define-key html-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; css-mode

(autoload 'css-mode "css-mode"
  "Major mode for css files" t)
;; (require 'css-mode)
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode) auto-mode-alist))

(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq cssm-indent-function #'cssm-c-style-indenter)))

;;; ------------------------------------------------------------
;;; js-mode

(add-hook 'js-mode-hook
          #'(lambda ()
;;             (flycheck-mode t)
             (setq js-indent-level 2)
             (setq indent-tabs-mode t)))

;; GAS - Google App Script
(add-to-list 'auto-mode-alist '("\\.gs$" . js-mode))

;;; ------------------------------------------------------------
;;; yaml-mode

(autoload 'yaml-mode "yaml-mode"
  "Major mode for yaml files" t)
;; (require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;;; ------------------------------------------------------------
;;; php-mode
;; not working at ubuntu

(require 'php-mode)

(setq auto-mode-alist
       (append '(("\\.php$" . php-mode))
           auto-mode-alist))

(defun unindent-closure ()
  "Fix php-mode indent for closures."
  (let ((syntax (mapcar 'car c-syntactic-context)))
    (if (and (member 'arglist-cont-nonempty syntax)
             (or
              (member 'statement-block-intro syntax)
              (member 'brace-list-intro syntax)
              (member 'brace-list-close syntax)
              (member 'block-close syntax)))
       (save-excursion
          (beginning-of-line)
          (delete-char (* (count 'arglist-cont-nonempty syntax)
                          c-basic-offset))) )))

(setq php-mode-enable-project-coding-style nil)

(add-hook 'php-mode-hook
          #'(lambda()
             (gtags-mode 1)

             (setq-local syntax-propertize-function
                         (syntax-propertize-rules
                          ;; html
                          ("\\(<\\)!--" (1 "< c"))
                          ("--[ \t\n]*\\(>\\)" (1 "> c"))))

             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil) ;; タブ文字をスペースに変換

             (add-hook 'c-special-indent-hook 'unindent-closure)

             (c-set-offset 'case-label 4)
             (c-set-offset 'arglist-intro 4)
             (c-set-offset 'arglist-cont-nonempty 4)
             (c-set-offset 'arglist-close 0)

             (setq php-speedbar-config nil)
             (setq php-template-compatibility t)
             (setq php-mode-warn-if-mumamo-off nil)
             ;; (setq php-mode-coding-style 'default)
             (setq php-manual-url "http://jp2.php.net/manual/ja/")

             (define-key php-mode-map (kbd "C-.") 'goto-last-change-reverse) ; override `php-show-arglist'
             (define-key php-mode-map ")" 'self-insert-command)
             (define-key php-mode-map "(" 'self-insert-command)
             (define-key php-mode-map "{" 'self-insert-command)
             (define-key php-mode-map "}" 'self-insert-command)
             (define-key php-mode-map "/" 'self-insert-command)
             (define-key php-mode-map "#" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; rainbow-mode
;; thx http://qiita.com/ironsand/items/cf8c582da3ec20715677

(autoload 'rainbow-mode "rainbow-mode"
  "Major mode for rainbow" t)
;; (require 'rainbow-mode)
(with-eval-after-load 'rainbow-mode
  (pop rainbow-hexadecimal-colors-font-lock-keywords)
  (push '("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{2,4\\}\\)" (1 (rainbow-colorize-itself 1)))
        rainbow-hexadecimal-colors-font-lock-keywords)
  (push '("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{2,4\\}\\)" (0 (rainbow-colorize-itself)))
        rainbow-hexadecimal-colors-font-lock-keywords))
(add-hook 'fundamental-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'lisp-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;;; ------------------------------------------------------------
;;; flycheck
;;; ------------------------------------------------------------
(require 'flycheck)
(autoload 'flycheck "flycheck-mode"
  "Major mode for flycheck" t)

(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; flycheckのwindowは単独で表示
  (add-to-list 'same-window-buffer-names "*Flycheck errors*")

  ;; キーバインド
  (global-set-key (kbd "C-M-c") 'flycheck-buffer)
  (global-set-key (kbd "C-M-l") 'flycheck-list-errors)
  (global-set-key (kbd "<C-M-up>") 'flycheck-previous-error)
  (global-set-key (kbd "<C-M-down>") 'flycheck-next-error)
  (define-key flycheck-error-list-mode-map (kbd "C-g") 'quit-window)
  (define-key flycheck-error-list-mode-map [escape] 'quit-window))

;; enable
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;;; ------------------------------------------------------------
;;; provides

(provide 'modes.init)

;;; modes.init.el ends here
