;;; init.el --- init.el for emacs

;;; Commentary:
;; Load Packages and elisps.

;;; Update Packages:
;; M-x package-list-packages U x
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

;;; --- Dev mode switch -------------------------------------------------
;; どれかが真なら開発モード
(defvar my-dev-mode-on nil
  "Non-nil なら Emacs を最小構成の開発モードで起動する。")

;; $ EMACS_DEV=1 emacs でも有効化できるように
(when (getenv "EMACS_DEV")
  (setq my-dev-mode-on t))

;; --dev で起動できるように（例: emacs --dev）
(add-to-list 'command-switch-alist
             '("--dev" . (lambda (_sw) (setq my-dev-mode-on t))))

;; ~/.emacs.d/.dev ファイルがあれば有効化（CI や一時的切り替え用）
(when (file-exists-p (expand-file-name ".dev" user-emacs-directory))
  (setq my-dev-mode-on t))

(when my-dev-mode-on
  ;; エラーを即座に掴む
  (setq debug-on-error t
        init-file-debug t          ; init 中のエラーのスタックも出る
        load-prefer-newer t)       ; .el が新しければ .elc より優先

  ;; ネイティブコンパイル関連の警告（必要に応じて）
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent)) ; うるさいときは 'silent

  ;; ログを厚めに
  (setq message-log-max 10000)

  ;; 起動時のノイズ抑制
  (setq inhibit-startup-screen t)

  ;; モードラインに表示（視認性）
  (setq-default mode-line-format
                (append mode-line-format
                        '(:eval (when my-dev-mode-on " [DEV]")))))

;;; ------------------------------------------------------------
;; prepare

(setq custom-safe-themes t) ;; カスタムテーマの読み込みも無視
(setq enable-local-variables nil) ;; ローカル変数の読み込み禁止
(setq custom-file (make-temp-file "emacs-custom")) ;; カスタムファイル無効

;;; ------------------------------------------------------------
;; supress error
;; 一番最初にセットしないとエラーがたくさん出る
(setq byte-compile-warnings nil)

;;; ------------------------------------------------------------
;;; font
(set-face-attribute 'default nil :family "MyricaM M" :height 160)
(set-fontset-font t 'japanese-jisx0208 "MyricaM M")

;;; ------------------------------------------------------------
;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/inits")

;;; ------------------------------------------------------------
;; custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-by-moving-to-trash t)
 '(foreign-regexp/regexp-type 'perl)
 '(package-selected-packages
   '(ace-jump-mode ace-window ack anything async auto-async-byte-compile
                   auto-complete cursor-chg flycheck foreign-regexp
                   google-translate gtags-mode mic-paren
                   multiple-cursors php-mode popwin rainbow-mode
                   smartrep web-beautify web-mode-edit-element
                   which-key yaml-mode zlc))
 '(reb-re-syntax 'foreign-regexp)
 ;; '(trash-directory "~/.Trash")
 '(sc/is-use-super t)
 '(sc/split-direction "vertical" t))

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
(if my-dev-mode-on
    ;; --- Dev モード時に最低限だけロード ---
    (progn
      (require 'gc.init)
      (require 'behaviour.init)
      (require 'editing.init)
      (require 'keyboard.init)
      ;; 必要に応じて追加
      )
 ;; --- 通常モード時（全部ロード） ---
  (progn
    (require 'gc.init)
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
    (require 'markdown.init)
    (require 'util.init)
    ))

;;; ------------------------------------------------------------
;; Theme
(unless my-dev-mode-on
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory "~/.emacs.d/themes/"))
  (load-theme 'jidaikobo-dark t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
