;;; init.el --- init.el for emacs

;;; Commentary:
;; Load Packages and elisps.

;;; Update Packages:
;; M-x package-list-packages U x は残念ながら有効にならない
;; M-x list-packages
;; したあと、updateしたいパッケージを探して、手動でinstallする

;;; Using Packages:
;; ace-jump-mode
;; anything
;; auto-async-byte-compile
;; auto-complete
;; cursor-chg
;; flycheck
;; foreign-regexp
;; google-translate
;; gtags
;; migemo
;; multiple-cursors
;; php-mode
;; rainbow-mode
;; smartrep
;; web-beautify
;; which-key
;; zlc

;;; experimental area:
;; (global-set-key (kbd "C--") 'func)
;; (message "this-event: %s this-command: %s" last-input-event this-command)
;; (message "initial: %s point: %s" initial-point (point))

;;; Code:

;;; ------------------------------------------------------------
;; supress error
;; 一番最初にセットしないとエラーがたくさん出る
(setq byte-compile-warnings nil)

;;; ------------------------------------------------------------
;; custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(jidaikobo-dark))
 '(delete-by-moving-to-trash t)
 '(foreign-regexp/regexp-type 'perl)
 '(package-selected-packages
   '(markdown-mode gtags-mode ace-window zlc yaml-mode which-key web-mode-edit-element web-beautify smartrep rainbow-mode popwin php-mode multiple-cursors mic-paren google-translate foreign-regexp flycheck cursor-chg auto-complete auto-async-byte-compile async anything ace-jump-mode))
 '(reb-re-syntax 'foreign-regexp)
 '(sc/is-use-super t)
 '(sc/split-direction "vertical" t)
 '(trash-directory "~/.Trash"))

;;; ------------------------------------------------------------
;;; font
(set-face-attribute 'default nil :family "MyricaM M" :height 160)
(set-fontset-font t 'japanese-jisx0208 "MyricaM M")

;;; ------------------------------------------------------------
;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/inits")

;;; ------------------------------------------------------------
;; HTMLのマークアップのキーバインド集
(require 'web-authoring-set)
(require 'web-select-html-ele)

;;; ------------------------------------------------------------
;; 検索センター - search-center
(require 'search-center)
(search-center-mode t)

;;; ------------------------------------------------------------
;; require
(require 'behaviour.init)
(require 'editing.init)
(require 'keyboard.init)
(require 'mozc.init)
(require 'window.init)
(require 'filer.init)
(require 'tramp.init)
(require 'anything.init)
(require 'ace-jump-mode.init)
(require 'auto-complete.init)
(require 'modes.init)
(require 'elisp.init)
(require 'gtags.init)
(require 'migemo.init)
(require 'markdown.init)
(require 'util.init)

;;; ------------------------------------------------------------
;; Theme
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/"))
(load-theme 'jidaikobo-dark t)

;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :family "MyricaM M"))))
 '(markdown-pre-face ((t (:inherit (## font-lock-constant-face) :height 1.0)))))
