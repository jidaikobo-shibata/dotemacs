;;; jidaikobo.init.el --- jidaikobo.init.el for jidaikobo.  Provides Mac OS like interface.
;; Copyright (C) 2016 by jidaikobo-shibata
;; Author: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; usage: emacsのインストール（要Xcode Command Line Tools）
;; thx http://masutaka.net/chalow/2015-04-12-1.html
;; ftp://ftp.math.s.chiba-u.ac.jp/emacsを確認して、あたらしいパッチの存在を確認すると良い
;; @ terminal
;; curl -LO http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.xz
;; curl -LO ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-24.5-mac-5.17.tar.gz
;; tar xfJ emacs-24.5.tar.xz
;; tar xfz emacs-24.5-mac-5.17.tar.gz
;; cd emacs-24.5
;; patch -p 1 < ../emacs-24.5-mac-5.17/patch-mac
;; cp -r ../emacs-24.5-mac-5.17/mac mac
;; cp ../emacs-24.5-mac-5.17/src/* src
;; cp ../emacs-24.5-mac-5.17/lisp/term/mac-win.el lisp/term
;; \cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
;; ./configure --prefix=$HOME/opt/emacs-24.5 --with-mac --without-x
;; make
;; make GZIP_PROG='' install
;; cp -r mac/Emacs.app /Applications

;;; Usage: 利用前の準備
;; このjidaikobo.init.elを~/.emacs.dに入れる前に、以下手順を踏んでおくこと。
;; @ terminal
;; sudo port install global

;;; Code:


;;; ------------------------------------------------------------
;;; minimal settings 最小限設定
;;; ------------------------------------------------------------

;; 設定ファイルのパス
(defvar jidaikobo-dir (file-name-directory
                       (or (buffer-file-name) load-file-name)))
(defvar dotfiles-dir (expand-file-name (concat jidaikobo-dir "../")))

;; ディレクトリ類
(defvar my-work-dir (expand-file-name "~/"))
(defvar my-fetch-app-dir (expand-file-name "~/"))

;; リージョンを上書きできるようにする
(delete-selection-mode t)

;; 選択範囲を可視化
(setq transient-mark-mode t)

;; font-lock-mode
(global-font-lock-mode t)

;; sort-linesはcase insensitiveで
(setq-default sort-fold-case t)

;; スクロールを一行ずつにする
(setq scroll-step 1)

;; クリップボードを他のアプリケーションと共用にする
(setq x-select-enable-clipboard t)

;; optionキーをMetaキーに
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; yes/noをy/nへ
(fset 'yes-or-no-p 'y-or-n-p)

;; 起動画面を抑止
(setq inhibit-startup-message t)

;; スクラッチメッセージを抑止
(setq initial-scratch-message nil)

;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 自動保存を無効
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; オートインデント無効
(when (functionp 'electric-indent-mode) (electric-indent-mode -1))

;; whitespaceの可視化と自動クリーンアップ
(global-whitespace-mode 1)
(setq-default whitespace-action '(auto-cleanup))

;; ツールバーを非表示
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f %%* Emacs@%s" (system-name)))

;; ミニバッファ履歴を保存
(savehist-mode 1)

;; キーストロークのミニバッファへの表示を早く
(setq echo-keystrokes 0.1)

;; ミニバッファでは半角英数で
(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1)

  ;; ヘルプは全角で操作しない
  (require 'helper)
  (global-set-key [f1] (lambda () (interactive)
                         (mac-auto-ascii-select-input-source)
                         (Helper-help)))
  (define-key Helper-help-map "a" 'apropos-command)
  (define-key Helper-help-map "b" 'describe-bindings)
  (define-key Helper-help-map "c" 'describe-key-briefly)
  (define-key Helper-help-map "d" 'apropos-documentation)
  (define-key Helper-help-map "e" 'view-echo-area-messages)
  (define-key Helper-help-map "f" 'describe-function)
  (define-key Helper-help-map "i" (lambda () (interactive) (info "(emacs245-ja)Top")))
  (define-key Helper-help-map "k" 'describe-key)
  (define-key Helper-help-map "m" 'describe-mode)
  (define-key Helper-help-map "p" 'finder-by-keyword)
  (define-key Helper-help-map "P" 'describe-package)
  (define-key Helper-help-map "r" 'info-emacs-manual)
  (define-key Helper-help-map "s" 'describe-syntax)
  (define-key Helper-help-map "t" 'help-with-tutorial)
  (define-key Helper-help-map "w" 'where-is)
  (define-key Helper-help-map "v" 'describe-variable)
  (define-key Helper-help-map "q" 'help-quit))

;; dont let the cursor go into minibuffer prompt
;; reference | http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt
                  face minibuffer-prompt))

;; タブ幅
(setq-default tab-width 2)

;; backward-delete-char-untabifyは、タブをバラさない
(setq backward-delete-char-untabify-method nil)

;; cua-modeの設定
(cua-mode t) ; C-RET
(setq-default cua-enable-cua-keys nil)

;; 原則画面分割しない
(setq display-buffer-alist nil)

;; Helpバッファは、ウィンドウを分割せず、常に選択する
(setq help-window-select t)
(add-to-list 'same-window-buffer-names "*Help*")

;; grepバッファは、ウィンドウを分割しない
(add-to-list 'same-window-buffer-names "*grep*")

;; tabbar使いなので、ほとんどの場合、window分割はしない
(add-to-list 'same-window-regexps "^[a-zA-Z0-9_-]+")

;; 複数フレームを開かないようにする
(setq-default ns-pop-up-frames nil)

;; emacsclientを使う
(server-start)

;; emacsclientバッファを落とす時に出る確認を抑止
(remove-hook
 'kill-buffer-query-functions
 'server-kill-buffer-query-function)

;; M-¥でバックスラッシュを入力
(global-set-key (kbd "M-¥") "\\")

;; 日本語入力時にM-/で全角スラッシュを入力
(global-set-key (kbd "M-/")
                (lambda () (interactive)
                  (if (fboundp 'mac-input-source)
                      (let ((mac-input-source (mac-input-source)))
                        (if (string-match
                             "com.apple.inputmethod.Kotoeri.japanese"
                             mac-input-source)
                            (insert "／")
                          (dabbrev-expand nil)))
                    (dabbrev-expand nil))))

;; 機能の有効化
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'delete-region 'disabled nil)
(custom-set-variables
 '(delete-by-moving-to-trash t)
 '(trash-directory "~/.Trash"))

;;; infoを日本語で
;; thx https://ayatakesi.github.io
;; thx http://rubikitch.com/2016/07/06/emacs245-manual-ja/
(when (file-directory-p "~/.emacs.d/info/")
  (require 'info)
  (add-to-list 'Info-directory-list "~/.emacs.d/info/")
  (defun Info-find-node--info-ja (orig-fn filename &rest args)
    (apply orig-fn
           (pcase filename
             ("emacs" "emacs245-ja")
             (t filename))
           args))
  (advice-add 'Info-find-node :around 'Info-find-node--info-ja))


;;; ------------------------------------------------------------
;;; frame フレーム
;;; ------------------------------------------------------------

;; 初期値
(add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
(add-to-list 'default-frame-alist '(width . 105))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
(ignore-errors (add-to-list 'default-frame-alist '(font . "ricty-16")))

;; フレームの大きさと位置を変更 (cmd+shift+w)
(defun resize-selected-frame ()
  "Resize frame to jidaikobo's default."
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 105 60))
(global-set-key (kbd "s-W") 'resize-selected-frame)


;;; ------------------------------------------------------------
;;; package関連
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; Load packages

;; load-pathの追加
(add-to-list 'load-path jidaikobo-dir)
(add-to-list 'load-path (concat dotfiles-dir "elisp"))

;; package.override.el
(defvar override-el (concat dotfiles-dir "package.override.el"))

;; load packages
(if (file-exists-p override-el)
    (load override-el)

  ;; Packages
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)

  ;; package-refresh-contents
	(unless (package-installed-p 'anything) (package-refresh-contents))

  ;; my-packages
  (defvar my-packages
    '(anything
      auto-async-byte-compile
      auto-complete
      cursor-chg
      flycheck
      foreign-regexp
      google-translate
      gtags
      mic-paren
      multiple-cursors
      php-mode
      popwin
      rainbow-mode
      recentf-ext
      smartrep
      tabbar
      undo-tree
      undohist
      web-beautify
      yagist
      zlc))

  ;; my-packagesからインストールしていないパッケージをインストール
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))


;;; ------------------------------------------------------------
;;; jidaikobo's elisp.
;;; ------------------------------------------------------------

;; 検索センター - search-center
(custom-set-variables
 '(sc/is-use-super t)
 '(sc/split-direction "vertical"))
(require 'search-center)
(search-center-mode t)

;; HTMLのマークアップのキーバインド集 - web-authoring-set
(require 'web-authoring-set)

;; テーマ - jidaikobo's theme
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat jidaikobo-dir "themes/")))
(load-theme 'jidaikobo-dark t)


;;; ------------------------------------------------------------
;;; undo、redo関連
;;; ------------------------------------------------------------

;; undohist
(require 'undohist)
(undohist-initialize)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; undo in regionしない
(defadvice undo-tree-undo (before deactivate-region activate)
  "Deactivate Region when attempt to undo."
  (deactivate-mark))


;;; ------------------------------------------------------------
;;; recentf
;;; ------------------------------------------------------------

(require 'recentf-ext)
(recentf-mode 1)
(setq recentf-exclude
      '("/TAGS$"
        "/var/tmp/"
        ".recentf"
        "^/[^/:]+:" ; TRAMP
        ".+Fetch Temporary Folder.+"))
(setq recentf-max-saved-items 10000)


;;; ------------------------------------------------------------
;;; キーボード操作
;;; ------------------------------------------------------------

;; mac-likeなcmd関係
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "<s-kp-add>") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "<s-kp-subtract>") 'text-scale-decrease)
(global-set-key (kbd "<s-kp-equal>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "s-=") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "<s-kp-0>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "<s-0>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "<backspace>") 'delete-backward-char)
(global-set-key (kbd "<s-up>") (lambda () (interactive "^") (goto-char (point-min))))
(global-set-key (kbd "<s-down>") (lambda () (interactive "^") (goto-char (point-max))))
(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)
(global-set-key (kbd "<prior>") 'backward-page)
(global-set-key (kbd "<next>") 'forward-page)

;; M-g or cmd+opt+j で指定行へジャンプ
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s-j") 'goto-line)

;; kill-lineがkill ringをnewするのでdelete-lineにする
(global-set-key (kbd "C-k")
                (lambda ()
                  (interactive)
                  (delete-char (- (save-excursion (end-of-line) (point)) (point)))))

;; window操作
(global-set-key (kbd "C-o") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-S-o") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "<C-kp-1>") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-vertically)
(global-set-key (kbd "<C-kp-2>") 'split-window-vertically)
(global-set-key (kbd "C-3") 'split-window-horizontally)
(global-set-key (kbd "<C-kp-3>") 'split-window-horizontally)
(global-set-key (kbd "C-0") 'delete-window)

;; split-windowは、フォーカスを移動してほしい
(defadvice split-window (after split-window-and-select activate)
  "Split window and select."
  (other-window 1))

;; escでM-g
(setq-default normal-escape-enabled t)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(global-set-key (kbd "<escape>") 'keyboard-quit) ; everywhere else
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)

;; align-regexp
(global-set-key (kbd "<backtab>") 'align-regexp)
(global-set-key (kbd "<C-tab>") 'align-regexp)

;; インデント整形
(global-set-key (kbd "s-}") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-]") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-{") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)

;;; ------------------------------------------------------------
;;; 次/前の空行
;; gist-description: Emacs(Elisp): forward/backward-paragraphだとparagraph判定がおそらくシンタックステーブル依存になり、字義通りの「次の空行」にならないので、別途用意。
;; gist-id: ad27b19dd3779ccc1ff2
;; gist-name: move-to-next(previous)-blank-line.el
;; gist-private: nil

(defun move-to-previous-blank-line ()
  "Go to previous empty lines."
  (interactive "^")
  (goto-char
   (or (save-excursion
         (unless (bobp)
           (backward-char)
           (re-search-backward "^$" nil t)))
       (point-min))))

(defun move-to-next-blank-line ()
  "Go to next empty lines."
  (interactive "^")
  (goto-char
   (or (save-excursion
         (unless (eobp)
           (forward-char)
           (re-search-forward "^$" nil t)))
       (point-max))))

(global-set-key (kbd "<M-up>") 'move-to-previous-blank-line)
(global-set-key (kbd "<M-down>") 'move-to-next-blank-line)

;;; ------------------------------------------------------------
;;; 自分好みのカーソル移動
;; gist-description: Emacs(Elisp): forward/backward-wordだと、移動距離が微妙に大きい。単語境界も微妙だった。ので、ちょっと変質的にカーソル移動をカスタマイズ。
;; gist-id: 467f4302c002049bfb95511bd21cdbe7
;; gist-name: skip-chars-(forward|backward)-dwim.el
;; gist-private: nil

(defun skip-chars-forward-dwim ()
  "Skip chars forward dwim."
  (interactive "^")
  (let ((start (point)))
    (if (eq last-command this-command)
        (skip-chars-forward "a-zA-Z0-9_-")
      (skip-chars-forward "a-zA-Z0-9_"))
    (when (eq start (point))
      (skip-syntax-forward " "))
    (when (eq start (point))
      (skip-syntax-forward "()"))
    (when (eq start (point))
      (skip-syntax-forward "<>"))
    (when (eq start (point))
      (skip-chars-forward "-"))
    (when (eq start (point))
      (skip-chars-forward "ぁ-んー"))
    (when (eq start (point))
      (skip-chars-forward "ァ-ヶー"))
    (when (eq start (point))
      (skip-chars-forward "亜-黑ー"))
    (when (eq start (point))
      (goto-char (+ (point) 1)))))

(defun skip-chars-backward-dwim ()
  "Skip chars backward dwim."
  (interactive "^")
  (let ((start (point)))
    (if (eq last-command this-command)
        (skip-chars-backward "a-zA-Z0-9_-")
      (skip-chars-backward "a-zA-Z0-9_"))
    (when (eq start (point))
      (skip-syntax-backward " "))
    (when (eq start (point))
      (skip-syntax-backward "()"))
    (when (eq start (point))
      (skip-syntax-backward "<>"))
    (when (eq start (point))
      (skip-chars-backward "-"))
    (when (eq start (point))
      (skip-chars-backward "ぁ-んー"))
    (when (eq start (point))
      (skip-chars-backward "ァ-ヶー"))
    (when (eq start (point))
      (skip-chars-backward "亜-黑ー"))
    (when (eq start (point))
      (goto-char (- (point) 1)))))

(global-set-key (kbd "<M-left>") 'skip-chars-backward-dwim)
(global-set-key (kbd "<M-right>") 'skip-chars-forward-dwim)

;;; ------------------------------------------------------------
;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
;; macふうの挙動だが、Emacsふうでないので、ちょっと様子見しつつ運用
;; C-@とどちらをとるか悩ましい

(defvar is-deactivate-region nil)
(when is-deactivate-region
  ;; regionの解除advice版
  (defadvice previous-line (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))
  (defadvice next-line (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))
  (defadvice left-char (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))
  (defadvice right-char (before deactivate-region activate)
    "Deactivate Region by cursor."
    (my-deactivate-region))

  ;; リージョン解除関数
  (defun my-deactivate-region ()
    "Logic of deactivate region by cursor."
    ;; (message "l: %s c: %s" last-input-event this-command)
    ;; (message "m:%s r:%s u:%s" mark-active (region-active-p) (use-region-p))
    ;; (message "s:%s e:%s" (region-beginning) (region-end))

    (when (and (not (memq last-input-event '(S-left S-right S-down S-up C-S-left C-S-right C-S-down C-S-up M-S-left M-S-right M-S-down M-S-up)))
               mark-active)
      (cond
       ((memq last-input-event '(right down))
        (goto-char (region-end)))
       ((memq this-command '(left-char previous-line))
        (goto-char (region-beginning))))
      (deactivate-mark))))

;;; ------------------------------------------------------------
;;; 対になるパーレンに移動
;; thx https://gist.github.com/donghee/3937661

(defun jump-match-paren (arg)
  "Go to the matching parenthesis.  ARG."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\[") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)\\|\\s\]") (forward-char 1) (backward-list 1))
        (t (back-to-indentation))))

(global-set-key (kbd "s-b") 'jump-match-paren)

;;; ------------------------------------------------------------
;;; 複数箇所選択 - multiple-cursors and smartrep
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(define-key mc/keymap (kbd "<return>")
  (lambda () (interactive) (insert (char-to-string 10))))

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

(smartrep-define-key global-map "C-t"
  '(("C-t" . 'mc/mark-next-like-this)
    ("n"   . 'mc/mark-next-like-this)
    ("p"   . 'mc/mark-previous-like-this)
    ("m"   . 'mc/mark-more-like-this-extended)
    ("u"   . 'mc/unmark-next-like-this)
    ("U"   . 'mc/unmark-previous-like-this)
    ("s"   . 'mc/skip-to-next-like-this)
    ("S"   . 'mc/skip-to-previous-like-this)
    ("*"   . 'mc/mark-all-like-this)
    ("d"   . 'mc/mark-all-like-this-dwim)
    ("i"   . 'mc/insert-numbers)
    ("o"   . 'mc/sort-regions)
    ("O"   . 'mc/reverse-regions)))

;;; ------------------------------------------------------------
;;; よく使うところに早く移動

(defvar next-block-previous-direction nil)
(defun next-block (direction)
  "Go to next block by mode.  DIRECTION[prev|next]."
  (interactive)
  (when (not (string= next-block-previous-direction direction))
    (if (string= direction "prev") (beginning-of-line) (end-of-line)))
  (setq next-block-previous-direction direction)
  (let
      (target)
    (cond
     ((string= major-mode "emacs-lisp-mode")
      (setq target "^;;; -+$"))
     ((string= major-mode "php-mode")
      (setq target "^\t*function\\|^\t*class\\|^\t*private\\|^\t*public"))
     (t
      (setq target "^;;; -+$\\|^■\\|^///")))
    (if (string= direction "prev")
        (re-search-backward target)
      (re-search-forward target))))

(global-set-key (kbd "<M-s-down>") (lambda () (interactive) (next-block "next")))
(global-set-key (kbd "<M-s-up>") (lambda () (interactive) (next-block "prev")))

;;; ------------------------------------------------------------
;;; 前回１秒以上立ち止まった場所にジャンプするコマンド
;; thx http://qiita.com/zk_phi/items/c145b7bd8077b8a0f537

(require 'ring)
(require 'edmacro)

(defvar-local jump-back!--marker-ring nil)

(defun jump-back!--ring-update ()
  "Jump-back! ring-update."
  (let ((marker (point-marker)))
    (unless jump-back!--marker-ring
      (setq jump-back!--marker-ring (make-ring 30)))
    (ring-insert jump-back!--marker-ring marker)))

(run-with-idle-timer 1 t 'jump-back!--ring-update)

(defun jump-back! ()
  "Jump back."
  (interactive)
  (if (ring-empty-p jump-back!--marker-ring)
      (error "No further undo information")
    (let ((marker (ring-ref jump-back!--marker-ring 0))
          (repeat-key (vector last-input-event)))
      (ring-remove jump-back!--marker-ring 0)
      (if (= (point-marker) marker)
          (jump-back!)
        (goto-char marker)
        (message "(Type %s to repeat)" (edmacro-format-keys repeat-key))
        (set-temporary-overlay-map
         (let ((km (make-sparse-keymap)))
           (define-key km repeat-key 'jump-back!)
           km))))))
(global-set-key (kbd "C-z") 'jump-back!)

;;; ------------------------------------------------------------
;;; 自分好みのタブの振る舞い（やや偏執的……）

(defun my-tab-dwim ()
  "Insert tab, indent, jump to link etc."
  (interactive)
  (cond
   ;; ewwバッファだったら次のリンク
   ((eq major-mode 'eww-mode)
    (shr-next-link))
   ;; read onlyバッファだったら次のリンク
   (buffer-read-only
    (forward-button 1 t))
   ;; ミニバッファだったらミニバッファ補完
   ((minibufferp (current-buffer))
    (minibuffer-complete))
   ;; 選択範囲に改行を含んでいるか、直前がエンターだったらインデント
   ((or (memq last-command '(newline))
        (and mark-active
             (string-match
              "\n"
              (buffer-substring-no-properties (region-beginning)
                                              (region-end)))))
    (indent-for-tab-command))
   ;; タブ／インデントを挿入
   (t
    (when mark-active (delete-region (region-beginning) (region-end)))
    (insert "\t"))))

(global-set-key (kbd "<tab>") 'my-tab-dwim)

(add-hook 'php-mode-hook
          '(lambda()
             (define-key php-mode-map (kbd "TAB") 'my-tab-dwim)
             (define-key php-mode-map (kbd "<tab>") 'my-tab-dwim)))


;;; ------------------------------------------------------------
;;; ファイル操作
;;; ------------------------------------------------------------

;; find-fileをzshライクに
;; thx http://d.hatena.ne.jp/mooz/20101003/p1
(require 'zlc)
(zlc-mode 1)
(let ((map minibuffer-local-map))
  (define-key map (kbd "<down>") 'next-history-element)
  (define-key map (kbd "<up>")   'previous-history-element))

;; root権限でファイルを開き直す
;; thx http://qiita.com/k_ui/items/d9e03ea9523036970519
(defun reopen-with-sudo ()
  "Reopen current buffer-file with sudo."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))

;; 現在バッファのファイルのフルパスを取得
(defun get-current-path ()
  "Get current file path."
  (interactive)
  (insert (or (buffer-file-name) (expand-file-name default-directory))))
(global-set-key (kbd "M-s-k") 'get-current-path)

;; Finderで現在バッファのファイルを表示
(defun point-current-buffer-by-finder ()
  "Point current buffer by Mac Finder."
  (interactive)
  (shell-command (concat "open " (expand-file-name default-directory))))
(global-set-key (kbd "M-s-K") 'point-current-buffer-by-finder)

;;; ------------------------------------------------------------
;;; 現在バッファパスにterminal/iTermでcdする
;; thx http://stackoverflow.com/questions/29404870/change-directory-in-osx-terminal-app-from-emacs-nw

(defun my-open-terminal-in-current-dir (&optional command)
  "Change directory to current buffer path by Terminal.app.  COMMAND is execute after cd."
  (interactive)
  (shell-command
   (concat "open -b com.apple.terminal " (expand-file-name ".") command)))

;; thx http://qiita.com/ganmacs/items/cfc5f9c2213a6a9e6579
(defun cd-on-iterm (&optional command)
  "Change directory to current buffer path by iTerm.app.  COMMAND is execute after cd."
  (interactive)
  (util/execute-on-iterm
   (concat (format "cd %s" default-directory) command)))

(defun util/execute-on-iterm (command)
  "Change directory to current buffer path by iTerm.app.  COMMAND."
  (interactive "MCommand: ")
  (do-applescript
   (format "tell application \"iTerm2\"
activate
tell current session of current window
write text \"%s\"
end tell
end tell"
           command)))

(global-set-key (kbd "C-c d") 'cd-on-iterm)
(global-set-key (kbd "C-c g") (lambda () (interactive) (cd-on-iterm " && git ci -a")))

;;; ------------------------------------------------------------
;;; gtags

(require 'gtags)
(setq gtags-path-style 'relative)

(setq-default gtags-mode-hook
              '(lambda ()
                 (local-set-key "\M-t" 'gtags-find-tag)
                 (local-set-key "\M-r" 'gtags-find-rtag)
                 (local-set-key "\M-s" 'gtags-find-symbol)
                 (local-set-key "\M-T" 'gtags-pop-stack)))

(setq-default gtags-select-mode-hook
              '(lambda ()
                 (local-set-key (kbd "RET") 'gtags-select-tag)))

(add-hook 'php-mode-hook '(lambda () (gtags-mode 1)))

;; update gtags
;; thx http://qiita.com/hayamiz/items/8e8c7fca64b4810d8e78
(defun my-update-gtags ()
  "Update gtags."
  (when (and (gtags-get-rootpath)
             (executable-find "global"))
    (start-process "gtags-update" nil
                   "global" "-uv")
    (message "gtags updated successfully.")))
(add-hook 'after-save-hook 'my-update-gtags)

;;; ------------------------------------------------------------
;;; ファイラ (dired)

(require 'dired)
(require 'dired-aux)
(require 'dired-explorer)
(require 'wdired)

;; diredでファイル編集（rで編集モードに）
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(define-key dired-explorer-mode-map "\M-r" 'wdired-change-to-wdired-mode)
(define-key wdired-mode-map (kbd "C-g") 'wdired-abort-changes)
(define-key wdired-mode-map [escape] 'wdired-abort-changes)

;; diredの前後の行移動をshift対応に
;; thx rubikitch
(defun dired-next-line--shift-select (&rest them)
  "Dired next line shift select.  THEM."
  (interactive "^p")
  (apply them))
(advice-add 'dired-next-line :around 'dired-next-line--shift-select)
(advice-add 'dired-previous-line :around 'dired-next-line--shift-select)

;; C-x C-f で現在位置を開く
(ffap-bindings)

;; diredでマークをつけたファイルを開く（F）
(define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
(defun my-dired-find-marked-files (&optional arg)
  "Open each of the marked files.  ARG."
  (interactive "P")
  (let* ((fn-list (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))

;; ディレクトリ操作は再帰的に
(setq dired-recursive-copies 'always)

;; diredバッファでC-sした時にファイル名だけにマッチするように
(add-hook 'dired-mode-hook 'dired-isearch-filenames-mode)

;; ウィンドウ分割で左右に違うDiredを開いているときにRやCのデフォルト値がもう片方になる
(setq dired-dwim-target t)

;; key-binds
(define-key dired-mode-map (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "RET") 'dired-explorer-dired-open)
(define-key dired-mode-map (kbd "<s-return>") 'dired-explorer-dired-open)
(define-key dired-mode-map (kbd "a") 'dired-find-file)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
(define-key dired-mode-map (kbd "M-s") 'dired-isearch-filenames-regexp)
(define-key dired-mode-map (kbd "s-d") (lambda () (interactive) (find-file "~/Desktop")))
(global-set-key (kbd "s-n") (lambda () (interactive) (find-file "~/")))
(global-set-key (kbd "s-N") (lambda () (interactive) (find-file "~/Sites")))
(global-set-key (kbd "C-x C-d") (lambda () (interactive) (find-file default-directory)))

;; dired-download-to-desktop
(defun dired-download-to-desktop ()
  "Download to desktop."
  (interactive)
  (dired-copy-file-recursive
   (dired-get-filename) "~/Desktop" t dired-copy-preserve-time t 'always)
  (message "Download to desktop."))
(define-key dired-mode-map (kbd "C-d") 'dired-download-to-desktop)
(define-key dired-explorer-mode-map (kbd "C-d") 'dired-download-to-desktop)

;;; ------------------------------------------------------------
;;; TRAMP

(require 'tramp)

;; TRAMPでは自動バックアップしない
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; FTPではパッシブモードでの接続を試みる
(setq-default ange-ftp-try-passive-mode t)

;; scpで接続
(setq tramp-default-method "scp")

;;; ------------------------------------------------------------
;; .poファイルを保存したらmsgfmt -oする

(add-hook 'after-save-hook
          (lambda ()
            (when (string= (file-name-extension (buffer-file-name)) "po")
              (shell-command (concat
                              "msgfmt -o "
                              (substring (buffer-file-name) 0 -2) "mo "
                              (buffer-file-name))))))


;;; ------------------------------------------------------------
;;; auto-complete
;;; ------------------------------------------------------------

(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-dwim t)
(setq ac-auto-show-menu 0.1)
(setq ac-delay 0.2)
(setq ac-auto-start 2)
(setq ac-ignore-case t)
(setq ac-disable-faces nil)
(setq ac-quick-help-delay 1)
(setq ac-use-comphist nil)
(setq ac-use-dictionary-as-stop-words nil)

;; ユーザ辞書設定
(defvar ac-my-dictionary (concat jidaikobo-dir "ac-dict/my-dictionary"))
(defvar ac-my-dictionary-dict '((candidates . (ac-file-dictionary ac-my-dictionary))))
(setq-default ac-sources '(ac-my-dictionary-dict
                           ac-source-words-in-same-mode-buffers))
;; (setq-default ac-sources '(ac-my-dictionary-dict))

;; 条件の追加
;; global-auto-complete-modeで足されていないものたち
(add-to-list 'ac-modes 'conf-mode)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'html-mode)

;;; 辞書に文字列を足して、git commit
(defun add-strings-to-ac-my-dictionary (dict-path)
  "Add strings to ac my dictionary.  DICT-PATH."
  (interactive)
  (let* ((beg (if mark-active (region-beginning) nil))
         (end (if mark-active (region-end) nil))
         (strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string " String: " ""))))
    (with-temp-buffer
      (insert-file-contents dict-path)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" strings "$") nil t)
          (message (concat "strings was already exists: " strings))
        (goto-char (point-max))
        (insert (concat "\n" strings))
        (sort-lines nil (point-min) (point-max))
        (delete-duplicate-lines (point-min) (point-max))
        (write-file dict-path)
        (shell-command (concat "git commit " dict-path " -m \"dictionary update.\""))
        (ac-clear-dictionary-cache)
        (message (concat "Add \"" strings "\"and git commit."))))))

;;; 辞書から文字列を削除して、git commit
(defun remove-strings-from-ac-my-dictionary (dict-path)
  "Remove strings from ac my dictionary.  DICT-PATH."
  (interactive)
  (let* ((beg (if mark-active (region-beginning) nil))
         (end (if mark-active (region-end) nil))
         (strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string " String: " ""))))
    (with-temp-buffer
      (insert-file-contents dict-path)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" strings "$") nil t)
          (if (yes-or-no-p (concat "Remove?:" strings))
              (progn
                (beginning-of-line)
                (kill-whole-line)
                (sort-lines nil (point-min) (point-max))
                (delete-duplicate-lines (point-min) (point-max))
                (write-file dict-path)
                (shell-command (concat "git commit " dict-path " -m \"dictionary update.\""))
                (ac-clear-dictionary-cache)
                (message (concat "Remove \"" strings "\"and git commit.")))
            (message (concat "Did nothing with: " strings)))
        (message (concat "Not found: " strings))))))

(global-set-key (kbd "C-c a") (lambda () (interactive)
                                (add-strings-to-ac-my-dictionary ac-my-dictionary)))
(global-set-key (kbd "C-c r") (lambda () (interactive)
                                (remove-strings-from-ac-my-dictionary ac-my-dictionary)))

;; auto-complete の候補に日本語を含む単語が含まれないようにする
;; thx http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
;; see also http://club.jidaikobo.com/knowledge/150.html
(defadvice ac-candidates (after remove-japanese-from-ac-candidates activate)
  "Do not contain multi byte character in auto-complete candidates."
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value (remove-if contain-japanese ad-return-value))))

;; 候補と入力文字が完全に一致している時にRETでac-completeするとnewlineしてしまうので抑止
(defadvice ac-complete (after advice-ac-complete activate)
  "Inhibit newline when full string was matched with candidate."
  (when (memq this-command '(newline))
    (delete-backward-char 1)
    (message "full string was matched with candidate.")))

;; 日本語に続く文字列でもauto-completeする
;; thx https://github.com/lugecy/dot-emacs/blob/master/conf.d/050-auto-complete.el
(defalias 'ac-prefix-default 'ac-prefix-for-ja)
(defun ac-prefix-for-ja ()
  "Alias fo ac-prefix-default."
  (save-match-data
    (let ((prefix-regexp "\\(?:\\sw\\|\\s_\\)+")
          (category-regexp
           (concat (lugecy-char-category-to-regexp (or (char-before) 0)) "+"))
          prefix-limit)
      (and
       (looking-back prefix-regexp (line-beginning-position) t)
       (setq prefix-limit (match-beginning 0))
       (looking-back category-regexp prefix-limit t)
       (max (match-beginning 0) prefix-limit)))))

(defun lugecy-char-category-to-regexp (char)
  "Multibyte CHAR."
  (let ((c (char-category-set char)))
    (cond
     ((aref c ?j)                       ; Japanese
      (cond
       ((aref c ?K) "\\cK")             ; katakana
       ((aref c ?A) "\\cA")             ; 2byte alphanumeric
       ((aref c ?H) "\\cH")             ; hiragana
       ((aref c ?C) "\\cC")             ; kanji
       (t "\\cj")))
     ((aref c ?k) "\\ck")               ; hankaku-kana
     ((aref c ?a) "\\ca")               ; ASCII
     (t "\\(?:\\sw\\|\\s_\\)"))))


;;; ------------------------------------------------------------
;;; popwin
;;; ------------------------------------------------------------

(require 'popwin)
(popwin-mode 1)
(setq-default display-buffer-function 'popwin:display-buffer)

;; *Help* *grep* はpopwinで管理しない
(setq popwin:special-display-config
      (delete 'help-mode popwin:special-display-config))
(setq popwin:special-display-config
      (delete (assoc 'grep-mode popwin:special-display-config) popwin:special-display-config))

;; anything
(setq-default anything-samewindow nil)
(push '("*anything*" :height 20)
      popwin:special-display-config)

;; Messages buffer
(push '("*Messages*" :height 10 :stick t :position bottom :tail t :noselect t)
      popwin:special-display-config)

;; Backtrace buffer
(push '("*Backtrace*" :height 10)
      popwin:special-display-config)

;; auto-async-byte-compile buffer
(push '(" *auto-async-byte-compile*" :height 10)
      popwin:special-display-config)

;; key-binds
(global-set-key (kbd "M-p p") 'popwin:display-last-buffer)
(global-set-key (kbd "M-p m") (lambda () (interactive)
                                (display-buffer "*Messages*")
                                (auto-revert-mode 1)))

;; defadvice-indent-for-tab-command
;; gist-description: Emacs(Elisp): To integrate indent style, delete existing whitespaces before indent-for-tab-command. indent-for-tab-commandの前に存在する行頭ホワイトスペースを削除することでインデントスタイルを統一する
;; gist-id: 604173d11ff376036635fd4811df6abb
;; gist-name: defadvice-indent-for-tab-command.el
;; gist-private: nil

(defadvice indent-for-tab-command (around advise-indent-for-tab-command activate)
  "To integrate indent style, delete existing whitespaces before indentation."
  (let (beg
        end
        (end-line nil))
    (cond
     ((use-region-p)
      (setq beg (region-beginning)
            end (region-end)
            end-line (line-number-at-pos end)))
     (t
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))))

    (perform-replace "^[\t ]+" "" nil t nil nil nil beg end)
    (goto-char beg)
    (set-mark-command nil)
    (goto-char end)

    ad-do-it

    (when end-line (goto-line (- end-line 1))) ;; why should i have to do minus?
    (back-to-indentation)))

;;; Auto Setting `indent-tabs-mode' Variable
;; thx https://github.com/moriyamahiroshi/hm-dot-emacs-files/blob/master/init.el

(defvar inside-string-or-comment-p)
(defvar re-search-forward-without-string-and-comments)

(defun inside-string-or-comment-p ()
  "Inside string or comment p."
  (let ((state (parse-partial-sexp (point-min) (point))))
    (or (nth 3 state) (nth 4 state))))

(defun re-search-forward-without-string-and-comments (&rest args)
  "Re search forward without string and comments.  ARGS."
  (let ((value (apply #'re-search-forward args)))
    (if (and value (inside-string-or-comment-p))
        (apply #'re-search-forward-without-string-and-comments args)
      value)))

(defun my-buffer-indent-tabs-code-p (&optional buffer)
  "Check first indent char.  BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (and (re-search-forward-without-string-and-comments "^[ \t]"
                                                              (point-max) t)
               (string= (match-string 0) "\t")))))))

(defun my-set-indent-tabs-mode ()
  "Set indent tab mode."
  (setq indent-tabs-mode (my-buffer-indent-tabs-code-p)))

(add-hook 'emacs-lisp-mode-hook #'my-set-indent-tabs-mode)
(add-hook 'php-mode-hook #'my-set-indent-tabs-mode)
(add-hook 'conf-mode-hook #'my-set-indent-tabs-mode)
(add-hook 'sh-script-mode-hook #'my-set-indent-tabs-mode)


;;; ------------------------------------------------------------
;;; Anything
;;; ------------------------------------------------------------

(require 'anything)
(require 'anything-config)

(defvar alist-anything-for-files
  '(anything-c-source-find-by-gtags
    anything-c-source-bookmarks
    anything-c-source-recentf
    ;; anything-c-source-buffers-list ;; *のバッファでAnythingを止めることがある
    ))

;; key binds
(define-key anything-map [escape] 'anything-keyboard-quit)
(define-key anything-map (kbd "<tab>") 'anything-select-action)

;; M-xによる補完をAnythingで行なう
(require 'anything-complete)
(anything-read-string-mode 1)

;; Anythingでファイルを開く方法をFind file as rootにしたときにsudoで開くように
(setq anything-su-or-sudo "sudo")

;; 編集対象でないバッファを除外(必要な場合、switch-to-buffer)
;; thx https://github.com/skkzsh/.emacs.d/blob/master/conf/anything-init.el
(setq anything-c-boring-buffer-regexp
      (rx "*" (+ not-newline) "*"))

;;; ------------------------------------------------------------
;;; あればgtagsを起点にしてfindし、なければカレントディレクトリを対象にした情報源

(defun my-get-project-name (x)
  "Project title for anything.  X."
  (with-anything-current-buffer
    (concat (if (gtags-get-rootpath) "gtags" "ls") ": "
            (if (string-match "/Sites/\\(.+?\\)\\b" default-directory)
                (substring default-directory (match-beginning 1) (match-end 1))
              (file-name-nondirectory (directory-file-name default-directory))))))

(defvar anything-c-source-find-by-gtags
  '((name . "Find by gtags or ls")
    (header-name . my-get-project-name)
    (candidates . (lambda ()
                    (let
                        ((default-directory
                           (with-current-buffer anything-current-buffer default-directory))
                         (find-opt " -type d -name \"logs\" -prune -o -type d -name \"cache\" -prune -o -type f ! -name \"*.png\" ! -name \"*.ico\" ! -name \"*.gif\" ! -name \"*.jpg\" ! -name \".DS_Store\""))
                      (cond
                       ;; gtags-get-rootpathが返ったらgtagsをあてにして良い
                       ((gtags-get-rootpath)
                        (split-string
                         (shell-command-to-string
                          (concat "find "
                                  (directory-file-name (gtags-get-rootpath))
                                  find-opt))
                         "\n"))
                       ;; gtagsがないならls
                       (t
                        (split-string
                         (shell-command-to-string
                          (concat "ls -1 " (shell-command-to-string "pwd"))) "\n"))))))
    (type . file)))

;;; ------------------------------------------------------------
;;; ~/.ssh/configを情報源として、tramp接続
;; thx rubikitch

(defvar anything-c-source-my-hosts
  '((name . "SSH hosts")
    (candidates . anything-c-source-my-hosts-candidates)
    (type . file)
    (action . find-file)))

(defun anything-c-source-my-hosts-candidates ()
  "Tramp candidates."
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents "~/.ssh/config")
                   (buffer-string))
                 "\n"))
        (hosts (list)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
        (setq host (string-trim (substring host (match-beginning 1) (match-end 2))))
        (unless (string= host "*")
          (add-to-list
           'hosts
           (concat "/" tramp-default-method ":" host ":") t))))
    hosts))

(add-to-list 'alist-anything-for-files 'anything-c-source-my-hosts t)

;;; ------------------------------------------------------------
;;; ~/.ftp/configを情報源として、tramp接続

(defvar anything-c-source-my-ftp-hosts
  '((name . "FTP hosts")
    (candidates . anything-c-source-my-ftp-hosts-candidates)
    (type . file)
    (action . (("FTP" . anything-tramp-ftp-open)))))

(defun anything-c-source-my-ftp-hosts-candidates ()
  "Tramp candidates."
  (let (alias
        path
        password
        (source (split-string
                 (with-temp-buffer
                   (insert-file-contents "~/.ftp/config")
                   (buffer-string))
                 "Alias"))
        (hosts (list)))
    (dolist (lines source)
      (dolist (line (split-string lines "\n"))
        (cond ((string-match "[P\\|p]ath +\\(.+?\\)$" line)
               (setq path (string-trim (substring line (match-beginning 1) (match-end 2)))))
              ((string-match "[P\\|p]assword +\\(.+?\\)$" line)
               (setq password (string-trim (substring line (match-beginning 1) (match-end 2)))))
              ((not (string= "" line))
               (setq alias (string-trim (string-trim line))))
))
      (when path (add-to-list 'hosts (concat alias "  " path "  " password))))
      hosts))

(add-to-list 'alist-anything-for-files 'anything-c-source-my-ftp-hosts t)

(defun anything-tramp-ftp-open (str)
  "Tramp FTP open.  STR is path and password."
  (let* ((strs (split-string str "  "))
         (path (car (cdr strs)))
         (password (car (reverse strs))))
    (kill-new password)
    (find-file path)))

;;; ------------------------------------------------------------
;;; my-anything-for-files

(defun my-anything-for-files ()
  "Anything command included find by gtags."
  (interactive)
  (anything-other-buffer
   alist-anything-for-files
   "*my-anything-for-files*"))
(global-set-key (kbd "C-;") 'my-anything-for-files)

;;; ------------------------------------------------------------
;; diredでanythingしたらfindする
(defvar anything-c-source-find-at-dired
  '((name . "Find file")
    (candidates . (lambda ()
                    (with-current-buffer anything-current-buffer
                      (let* ((shell-file-name
                              (if (string-match
                                   "\\.sakura"
                                   (or (file-remote-p dired-directory t) ""))
                                  "/usr/local/bin/bash"
                                "/bin/bash"))
                             (current-dir (dired-current-directory))
                             (sep-point (string-match ":/" current-dir))
                             (pwd (if sep-point (substring current-dir (+ (match-beginning 0) 1))
                                    current-dir))
                             (tramp-host (file-remote-p dired-directory t))
                             (tramp-results (list))
                             (results (split-string
                                       (shell-command-to-string
                                        (concat "find "
                                                (replace-regexp-in-string "/$" "" pwd)
                                                (replace-regexp-in-string "\n" " "
                                                                          "
-type d -name \"logs\" -prune -o
-type d -name \"cache\" -prune -o
-type d -name \".git\" -prune -o
-type f ! -name \"*.png\"
! -name \"*.ico\"
! -name \"*.gif\"
! -name \"*.jpg\"
! -name \".DS_Store\"")))
                                       "\n")))
                        (if tramp-host
                            (progn
                              (dolist (result results)
                                (add-to-list 'tramp-results (concat tramp-host result)))
                              tramp-results)
                          results)))))
    (type . file)))

(defun my-anything-c-source-find-at-dired ()
  "Anything command for find at dired."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-find-at-dired
     anything-c-source-my-hosts
    anything-c-source-bookmarks
    anything-c-source-recentf)
   "*my-anything-c-source-find-at-dired*"))
(define-key dired-mode-map (kbd "C-;") 'my-anything-c-source-find-at-dired)
(define-key dired-explorer-mode-map (kbd "C-;") 'my-anything-c-source-find-at-dired)

;;; ------------------------------------------------------------
;;; Anything - Encode and Line folding

(defvar anything-c-source-coding-system
  '((name . "Encode and Line Folding")
    (candidates . (lambda ()
                    '("set UTF-8"
                      "set EUC-JP"
                      "set Shift-JIS"
                      "set ISO-2022-JP"
                      "set LF"
                      "set CR"
                      "set CR+LF")))
    (action ("default" . anything-coding-system))))

(defun anything-coding-system (act)
  "Change Encode and Lin folding.  ACT is what to do."
  (message act)
  (cond ((string= act "set UTF-8")
         (set-buffer-file-coding-system 'utf-8))
        ((string= act "set EUC-JP")
         (set-buffer-file-coding-system 'euc-jp))
        ((string= act "set Shift-JIS")
         (set-buffer-file-coding-system 'shift_jis))
        ((string= act "set ISO-2022-JP")
         (set-buffer-file-coding-system 'iso-2022-jp))
        ((string= act "set LF")
         (set-buffer-file-coding-system 'unix))
        ((string= act "set CR")
         (set-buffer-file-coding-system 'mac))
        ((string= act "set CR+LF")
         (set-buffer-file-coding-system 'dos)))
  (save-buffer)
  (revert-buffer))

(defun my-anything-for-coding-system ()
  "Anything command for program."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-coding-system)
   "*my-anything-c-source-coding-system*"))
(global-set-key (kbd "C-^") 'my-anything-for-coding-system)

;;; ------------------------------------------------------------
;;; Anything my-anything-for-functions

(defun my-anything-for-functions ()
  "Anything command for program."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-emacs-functions-with-abbrevs
     anything-c-source-emacs-commands
     anything-c-source-emacs-variables
     anything-c-source-imenu)
   "*my-anything-for-functions*"))
(global-set-key (kbd "C-,") 'my-anything-for-functions)


;;; ------------------------------------------------------------
;;; タブ関連 - tabbar
;;; ------------------------------------------------------------

(defvar is-use-tabbar nil)
(autoload 'tabbar-mode "tabbar" "" t)

(when is-use-tabbar
  (tabbar-mode 1)

  ;; my-tabbar-buffer-list
  ;; thx http://ser1zw.hatenablog.com/entry/2012/12/31/022359
  (defun my-tabbar-buffer-list ()
    "My tabbar buffer list."
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ;; Always include the current buffer.
                       ((eq (current-buffer) b) b)
                       ((buffer-file-name b) b)
                       ((char-equal ?\ (aref (buffer-name b) 0)) nil)
                       ;; *scratch*バッファは表示する
                       ((equal "*scratch*" (buffer-name b)) b)
                       ;; *grep*バッファは表示する
                       ((equal "*grep*" (buffer-name b)) b)
                       ;; *eww*バッファは表示する
                       ((equal "*eww*" (buffer-name b)) b)
                       ;; それ以外の * で始まるバッファは表示しない
                       ((char-equal ?* (aref (buffer-name b) 0)) nil)
                       ((buffer-live-p b) b)))
                  (buffer-list))))
  (setq-default tabbar-buffer-list-function 'my-tabbar-buffer-list)

  ;; 変更をラベルで可視化
  ;; thx http://www.emacswiki.org/emacs/TabBarMode
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " + " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))
  (defun ztl-modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  (defun ztl-on-buffer-modification ()
    (set-buffer-modified-p t)
    (ztl-modification-state-change))
  (add-hook 'after-save-hook 'ztl-modification-state-change)
  (add-hook 'first-change-hook 'ztl-on-buffer-modification)

  ;; タブ同士の間隔
  (setq-default tabbar-separator '(0.7))

  ;; グループ化しない
  (setq-default tabbar-buffer-groups-function nil)

  ;; 画像を使わない
  (setq-default tabbar-use-images nil)

  ;; hide buttons
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  ;; move-current-tab-to-top
  ;; gist-description: Emacs(Elisp): move current tab (buffer) to top at tabbar-mode. tabbarで選択中のタブ（バッファ）を左端に移動します。
  ;; gist-id: 54dab2fc5f2e278833f5
  ;; gist-name: move-current-tab-to-top.el
  ;; gist-private: nil

  (eval-when-compile (defvar tabbar-current-tabset))
  (defun move-current-tab-to-top ()
    "Move current tab to top."
    (interactive)
    (let* ((bufset (tabbar-current-tabset t))
           (bufs (tabbar-tabs bufset))
           (car-bufs (list))
           (cdr-bufs (list)))
      ;; 現在のバッファと一致するものを探して先頭へ
      (dolist (buf bufs)
        (if (string= (buffer-name) (format "%s" (car buf)))
            (add-to-list 'car-bufs buf)
          (add-to-list 'cdr-bufs buf)))
      (setq cdr-bufs (reverse cdr-bufs))
      (set bufset (append car-bufs cdr-bufs))
      ;; タブバー書き換え
      (tabbar-set-template bufset nil)
      (tabbar-display-update)))

  ;; キーバインド
  (global-set-key (kbd "M-s-<right>") 'tabbar-forward-tab)
  (global-set-key (kbd "M-s-<left>") 'tabbar-backward-tab)
  (global-set-key (kbd "C-s-t") 'move-current-tab-to-top)

  ;; thx open-junk-file by rubikitch
  (global-set-key (kbd "s-t")
                  (lambda ()
                    (interactive)
                    (find-file-other-window
                     (format-time-string "~/Tasks/_tmp/%Y%m%d-%H%M%S.txt" (current-time)))))

  ;; 幾つかのウィンドウでは、タブ移動しない
  (defadvice tabbar-forward-tab (around advise-tabbar-forward-tab activate)
    "Do not forward at specified baffers."
    (if (member (buffer-name) '("*RE-Builder*" "*Messages*"))
        nil
      ad-do-it))
  (defadvice tabbar-backward-tab (around advise-tabbar-backward-tab activate)
    "Do not backward at specified baffers."
    (if (member (buffer-name) '("*RE-Builder*" "*Messages*"))
        nil
      ad-do-it)))

;;; ------------------------------------------------------------
;;; ウィンドウ/スクリーンを閉じる

(defun my-delete-windows ()
  "Contexual delete windows."
  (interactive)
  (cond
   ;; なんだかミニバッファにいたら抜ける
   ((minibufferp (current-buffer))
    (minibuffer-keyboard-quit)
    (other-window 1)
    (my-delete-windows))
   ;; ウィンドウ構成が多ければまず自分を消す
   ((not (one-window-p)) (delete-window))
   ;; ウィンドウ構成がひとつでバッファに変更があれば破棄を確認する
   ((or (and (buffer-modified-p)
             ;; read-onlyなら無視
             (not buffer-read-only)
             ;; スクラッチ以外でアスタリスクで始まるバッファ名も保存を尋ねない
             (not (string=
                   (substring (buffer-name (current-buffer)) 0 1)
                   "*")))
        ;; スクラッチバッファでメモ代わりに使っていたら保存を尋ねる
        (and (buffer-modified-p) (string= (buffer-name) "*scratch*")))
    (unless (yes-or-no-p "Buffer is modified. Close anyway?")
      (call-interactively (save-buffer)))
    (kill-buffer))
   ;; kill-buffer for is-use-tabbar and other situation
   (t (kill-buffer))))

(global-set-key (kbd "s-w") 'my-delete-windows)


;;; ------------------------------------------------------------
;;; カーソル関連
;;; ------------------------------------------------------------

;; cursor-chg
;; カーソルの色と形状を変更（ブロックカーソルが苦手なので）
(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 0)
(setq curchg-default-cursor-color "White")
(setq curchg-input-method-cursor-color "firebrick")
(setq curchg-change-cursor-on-input-method-flag t)


;;; ------------------------------------------------------------
;;; 行設定
;;; ------------------------------------------------------------

;; 行カーソル
;; thx http://rubikitch.com/tag/emacs-post-command-hook-timer/
(require 'hl-line)
(defun global-hl-line-timer-function ()
  "Line cursor."
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq-default global-hl-line-timer
              (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; 行間隔を少し広げる
(set-default 'line-spacing 3)

;; 行番号を表示する
(defun show-line-number ()
  "Show line number."
  (interactive)
  (require 'linum)
  (setq-default linum-delay t)
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current))
  ;; (global-linum-mode t)
  (setq-default linum-format "%5d: "))
(add-hook 'emacs-lisp-mode-hook (lambda () (show-line-number) (linum-mode t)))
(add-hook 'php-mode-hook (lambda () (show-line-number) (linum-mode t)))
(add-hook 'css-mode-hook (lambda () (show-line-number) (linum-mode t)))


;;; ------------------------------------------------------------
;;; モードライン設定
;;; ------------------------------------------------------------

;; 関数名の表示
(which-function-mode 1)

;; フレーム情報は不要
(setq-default mode-line-frame-identification "")

;; タブバーに出ているのでバッファ名は不要
(setq-default mode-line-buffer-identification "")

;; タブバーに出ているのでバッファの変更状態も不要
(setq-default mode-line-modified "")

;;; 前に行番号、総行数、桁番号を表示
;;; 総行数の計する%記法がないので遅延で計算させる
;; thx rubikitch
(defvar-local mode-line-last-line-number 0)
(defvar-local clnaw-last-tick 0)
(defun calculate-total-line-numbers ()
  "Calculate total line numbers."
  (unless (eq clnaw-last-tick (buffer-modified-tick))
    (setq mode-line-last-line-number (line-number-at-pos (point-max)))
    (setq clnaw-last-tick (buffer-modified-tick))
    (force-mode-line-update)))
(run-with-idle-timer 1 t 'calculate-total-line-numbers)

;; 現在行、総行、文字位置、選択範囲の文字数など
(setq mode-line-position
      '(:eval (format "%d/%d %d/%d %s"
                      (line-number-at-pos)
                      mode-line-last-line-number
                      (point)
                      (point-max)
                      (if mark-active
                          (concat "[" (format "%s" (- (region-end) (region-beginning))) "]")
                        ""))))

;; 改行の種類表示の変更
;; thx https://github.com/moriyamahiroshi/hm-dot-emacs-files/blob/master/init.el
(setq-default eol-mnemonic-unix "(LF)")
(setq-default eol-mnemonic-dos  "(CRLF)")
(setq-default eol-mnemonic-mac  "(CR)")


;;; ------------------------------------------------------------
;;; マウス設定
;;; ------------------------------------------------------------

;; shift+clickでregion作成
;; thx http://superuser.com/questions/521223/shift-click-to-extend-marked-region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)


;;; ------------------------------------------------------------
;;; 編集支援
;;; ------------------------------------------------------------

;;; 行／選択範囲の複製 (cmd+d)
;; gist-description: Emacs(Elisp): duplicate region or line. 選択範囲がある場合は選択範囲を、選択範囲がない場合は、行を複製します。
;; gist-id: 297fe973cde66b384fa1
;; gist-name: duplicate-region-or-line.el
;; gist-private: nil

(defun duplicate-region-or-line ()
  "Duplicate region or line."
  (interactive)
  (let (selected
        (is-line nil))
    (if (not (region-active-p))
        (progn
          (setq is-line t)
          (beginning-of-line)
          (set-mark-command nil)
          (end-of-line)
          (setq deactivate-mark nil)))
    (setq selected (buffer-substring-no-properties (region-beginning) (region-end)))
    (if is-line (progn
                  (insert "\n" selected)
                  (beginning-of-line))
      (insert selected))))

(global-set-key (kbd "s-d") 'duplicate-region-or-line)

;;; ------------------------------------------------------------
;;; 選択範囲を[大文字|小文字|キャピタライズ]に

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "s-U") 'upcase-region)
(global-set-key (kbd "s-L") 'downcase-region)
(global-set-key (kbd "s-C") 'capitalize-region)

;;; ------------------------------------------------------------
;;; 全角英数字を半角英数字に、半角カナを全角に、UTF-8の濁点分離を直す
;; http://d.hatena.ne.jp/nakamura001/20120529/1338305696
;; http://www.sakito.com/2010/05/mac-os-x-normalization.html
;; gist-description: Emacs(Elisp): 全角英数字を半角英数字に、半角カナを全角に、UTF-8の濁点分離を直す。
;; gist-id: 08a752b04107dbc50ef5
;; gist-name: normalize-chars.el
;; gist-private: nil

(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(defun normalize-chars ()
  "Normarize chars."
  (interactive)
  ;; 選択範囲があればそこを対象にする
  (let (type
        beg
        end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (progn
        (setq type (read-string "normalize whole buffer?(y, n): " nil))
        (if (string= type "y")
            (progn
              (setq beg (point-min))
              (setq end (point-max)))
          (error "Error: no target region"))))
    (japanese-zenkaku-region beg end t)
    (japanese-hankaku-region beg end t)
    (ucs-normalize-NFC-region beg end)))

;; 音引、句読点等を除外
;; thx http://d.hatena.ne.jp/khiker/20061014/1160861915
(put-char-code-property ?ー 'ascii nil)
(put-char-code-property ?〜 'ascii nil)
(put-char-code-property ?、 'ascii nil)
(put-char-code-property ?。 'ascii nil)
(put-char-code-property ?＆ 'ascii nil)
(put-char-code-property ?？ 'ascii nil)
(put-char-code-property ?！ 'ascii nil)

(global-set-key (kbd "s-u") 'normalize-chars)

;;; ------------------------------------------------------------
;;; 選択範囲を1行にする。最初のインデントは残す。
;; gist-description: Emacs(Elisp): Join multi lines to one. 選択範囲を1行にまとめます。
;; gist-id: ee6b2f8ef659ed58605d
;; gist-name: join-multi-lines-to-one.el
;; gist-private: nil

(defun join-multi-lines-to-one ()
  "Join multi lines."
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char beg)
    (back-to-indentation)
    (setq beg (point))
    (goto-char end)
    (goto-char (- (point) 1))
    (end-of-line)
    (setq end (point))
    (perform-replace "\n\\|^>+ *\\|^[\t　 ]+" "" nil t nil nil nil beg end)
    (goto-char beg)))

(global-set-key (kbd "<s-kp-divide>") 'join-multi-lines-to-one) ; cmd+/
(global-set-key (kbd "s-/") 'join-multi-lines-to-one) ; cmd+/

;;; ------------------------------------------------------------
;;; align-regexpが、indent-tabs-modeがtでも、スペースを詰めるように

(defadvice align-regexp (around advise-align-regexp activate)
  "Let ALIGN-REGEXP indent by spaces."
  (when indent-tabs-mode (setq indent-tabs-mode nil))
  ad-do-it
  (my-set-indent-tabs-mode))

;;; ------------------------------------------------------------
;;; 定型句挿入

(defun insert-function-header (type author copyright link)
  "TYPE, AUTHOR, COPYRIGHT, LINK."
  (interactive)
  (let* (ret
         cursor)
    (cond
     ;; file-head
     ((eq type 1)
      (setq ret (concat "/**\n * Classname or Filename.\n *\n * @package    ex: FuelPHP, WordPress\n * @version    0.0\n * @author     " author "\n * @license    ex: FuelPHP: MIT License, Wordpress: GPL\n * @copyright  " copyright "\n * @link       " link "\n */\n\n")
            cursor 35))
     ;; function-head
     ((eq type 2)
      (setq ret (concat "/**\n * Explanation.  After dot needs double space.\n *\n * @param   string     $str\n * @param   int|string $int\n * @param   array      $arr\n * @return  void|bool\n */\n")
            cursor 35)))
    ;; put val
    (insert ret)
    (goto-char (- (point) cursor))))

;; php without ip
(global-set-key (kbd "s-M-C")
                (lambda (type author copyright link)
                  (interactive
                   "nType 1:file header, 2:function header:\nsAuthor:\nsCopyright:\nsLink:")
                  (insert-function-header type author copyright link)))


;;; ------------------------------------------------------------
;;; ユーティリティ
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; 選択範囲を計算してバッファに出力
;; gist-description: Emacs(Elisp): calculate region and insert. 選択範囲の数式を計算して、次の行にinsertします。数字が羅列されている場合は、加算します。数字や式と自然な文章が混在している場合は、数式のみを計算します。
;; gist-id: b967d6a7441f85aa541d
;; gist-name: calculate-region-and-insert.el
;; gist-private: nil

(defun add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.
It defaults to a comma."
  (let ((num (number-to-string number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat
                 (match-string 1 num) op
                 (match-string 2 num))))
    num))

(defun calculate-region-and-insert (beg end)
  "Calculate natural text of region and insert to current buffer.  BEG, END."
  (interactive "r")
  (let* ((strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string " Expression: " "")))
         (is_num_format (string-match "," (buffer-substring-no-properties beg end)))
         result)
    ;; 余計なものを取り払って計算の準備
    (when mark-active
      (with-temp-buffer
        (insert strings)
        (perform-replace "[\t,　 ]+" "" nil t nil nil nil (point-min) (point-max))
        (perform-replace "\n" "+" nil t nil nil nil (point-min) (point-max))
        (perform-replace "[^0-9\\+\\*/\\(\\)^\\.-]" "+" nil t nil nil nil (point-min) (point-max))
        (perform-replace "\\++" "+" nil t nil nil nil (point-min) (point-max))
        (perform-replace "\\+$" "" nil t nil nil nil (point-min) (point-max))
        (perform-replace "^\\++" "" nil t nil nil nil (point-min) (point-max))
        (setq strings (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char end)
      (end-of-line)
      (newline))
    (setq result (calc-eval strings))
    ;; カンマ整形されている計算式だったらカンマ区切りで返す
    (when is_num_format (setq result (add-number-grouping (string-to-number result) ",")))
    ;; (calc-eval)は、小数点を含んだ式の場合、整数でも末尾にピリオドをつけるので抑止
    (when (string-match "\\.$" result)
      (setq result (substring result 0 (match-beginning 0))))
    (insert result)))
(global-set-key (kbd "M-c") 'calculate-region-and-insert)

;;; ------------------------------------------------------------
;;; 選択範囲の言語を確認して翻訳 (C-c t)

;; google-translate
;; http://rubikitch.com/2014/12/07/google-translate/
(require 'google-translate)
(defvar google-translate-english-chars "[:ascii:]"
  "Ascii means English.")
(defun google-translate-enja-or-jaen (&optional string)
  "Google translate enja or jaen.  STRING in region."
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)

;;; ------------------------------------------------------------
;; gist-description: Emacs(Elisp): create or update gist by using yagist. yagistでregionのgistをupdateする。
;; gist-id: a20cd2d106edba225115
;; gist-name: yagist-region-create-or-update.el
;; gist-private: nil

(require 'yagist)

(defun yagist-region-create-or-update (beg end)
  "Post the current region as a create or update at gist.github.com.
If gist-id exists update gist.  BEG END."
  (interactive "r")
  (let* ((raw (buffer-substring-no-properties beg end))
         (lines (split-string raw "\n"))
         (description nil)
         (id nil)
         (name nil)
         (private nil))
    ;; attributes
    ;; use concat to avoide synonym :-(
    (while lines
      (cond ((and (string-match (concat "gist" "-description: ") (car lines))
                  (not description))
             (setq description (substring (car lines) (match-end 0))))
            ((and (string-match (concat "gist" "-id: ") (car lines))
                  (not id))
             (setq id (substring (car lines) (match-end 0))))
            ((and (string-match (concat "gist" "-name: ") (car lines))
                  (not name))
             (setq name (substring (car lines) (match-end 0))))
            ((and (string-match (concat "gist" "-private: ") (car lines))
                  (not private))
             (setq private (substring (car lines) (match-end 0)))))
      (setq lines (cdr lines)))
    ;; tab to space
    (when raw
      (setq raw (replace-regexp-in-string "\t" "  " raw)))
    ;; update
    (when (and id name raw description)
      (yagist-request
       "PATCH"
       (format "https://api.github.com/gists/%s" id)
       (yagist-simple-receiver "Update")
       `(("description" . ,description)
         ("files" . ((,name . (("content" . ,raw))))))))
    ;; create
    (if (and name raw description (not id))
        (progn (yagist-request
                "POST"
                "https://api.github.com/gists"
                'yagist-created-callback
                `(("description" . ,description)
                  ("public" . ,(if private :json-false 't))
                  ("files" . ((,name . (("content" . ,raw))))))))
      (error "Lack of parameters"))))

(global-set-key (kbd "C-M-g") 'yagist-region-create-or-update)

;;; ------------------------------------------------------------
;;; web-beautify

(require 'web-beautify)
(setq-default web-beautify-args
              '("-f"
                "-"
                "--indent_with_tabs"
                "--indent-size 2"
                "--end-with-newline"))


;;; ------------------------------------------------------------
;;; Elisp
;;; ------------------------------------------------------------

;;; Elispのimenuをカスタマイズ
(add-hook
 'emacs-lisp-mode-hook
 '(lambda ()
    (setq imenu-generic-expression
          '(("defun" "^\\s-*(defun\\s-+\\([-A-Za-z0-9/+]+\\)" 1)
            ("defadvice" "^\\s-*(defadvice\\s-+\\([-A-Za-z0-9/+]+\\)" 1)))))

;;; 釣り合いのとれる括弧のハイライト
;; 少々大袈裟だけれど、括弧同士のハイライトがカーソルの邪魔なのでアンダーラインにする
(require 'mic-paren)
(paren-activate)
(setq paren-match-face 'underline paren-sexp-mode t)
(setq paren-sexp-mode t)

;;; 自動バイトコンパイル
;; thx http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "init.el\\|wl.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; s+RETでeval-bufferかeval-region
(global-set-key (kbd "<s-return>")
                (lambda () (interactive)
                  (if (region-active-p)
                      (eval-region (region-beginning) (region-end))
                    (eval-buffer))
                  (message "eval done.")))

;;; Elispの関数名をコメント状態に（ものぐさ……）
(global-set-key (kbd "C-.")
                (lambda () (interactive)
                  (let* (
                         (beg (when (region-active-p) (region-beginning)))
                         (end (when (region-active-p) (region-end)))
                         (str (when (region-active-p) (buffer-substring-no-properties beg end))))
                    (when str
                      (setq str (replace-regexp-in-string "-" " " str))
                      (setq str (replace-regexp-in-string "$" "." str)))
                    (delete-region beg end)
                    (insert str))))

;;; *Messages*バッファを自動スクロール
;; thx http://stackoverflow.com/questions/4682033/in-emacs-can-i-set-up-the-messages-buffer-so-that-it-tails
(defun modi/messages-auto-tail (&rest _)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name "*Messages*")
         (buf (get-buffer-create buf-name)))
    (when (not (string= buf-name (buffer-name)))
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      (with-current-buffer buf
        (goto-char (point-max))))))
(advice-add 'message :after #'modi/messages-auto-tail)


;;; ------------------------------------------------------------
;;; 各種モード
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; sh-script-mode

(setq auto-mode-alist
      (append '(("^\\." . sh-script-mode))
              auto-mode-alist))

(add-hook 'sh-script-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2)
             (setq indent-tabs-mode nil)
             (setq sh-indentation 2)))

;;; ------------------------------------------------------------
;;; text-mode

(add-hook 'text-mode-hook
          '(lambda()
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
                              nil
                              " Kontiki-Draft")

(add-hook 'kontiki-mode-hook
          '(lambda()
             (font-lock-add-keywords nil '(("^//.+" . font-lock-comment-face)))
             (font-lock-add-keywords nil '(("<.+?>" . font-lock-keyword-face)))
             (font-lock-add-keywords nil '(("\\[memo:.+?\\]" . font-lock-builtin-face)))
             (font-lock-add-keywords nil '(("^[a-zA-Z_]+?:" . font-lock-function-name-face)))
             (font-lock-add-keywords nil '(("^\\*.+" . font-lock-function-name-face)))))

;;; ------------------------------------------------------------
;;; mail-mode

(add-hook 'mail-mode-hook
          '(lambda()
             (font-lock-add-keywords nil '(("^> .+" . font-lock-keyword-face)))
             (font-lock-add-keywords nil '(("^>> .+" .font-lock-type-face)))
             (font-lock-add-keywords nil '(("^>>>.+" . font-lock-string-face)))))

;;; ------------------------------------------------------------
;;; grep-mode

(add-hook 'grep-mode-hook
          '(lambda()
             (define-key grep-mode-map (kbd "C-o")
               (lambda () (interactive) (other-window 1)))
             (define-key grep-mode-map (kbd "C-S-o")
               (lambda () (interactive) (other-window -1)))))

;;; ------------------------------------------------------------
;;; html-mode

(add-hook 'html-mode-hook
          '(lambda()
             (setq-local syntax-propertize-function
                         (syntax-propertize-rules
                          ;; xoops smarty comment-out
                          ("\\(<\\){\\*" (1 "< c"))
                          ("\\*}\\(>\\)" (1 "> c"))))
             ;; xoops smarty
             (font-lock-add-keywords
              nil
              '(("<{\\(?:.\\)+?}>" . font-lock-keyword-face)))
             (define-key html-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; css-mode

(require 'css-mode)
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode) auto-mode-alist))

;; (defvar cssm-indent-function)
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq cssm-indent-function #'cssm-c-style-indenter)))

;;; ------------------------------------------------------------
;;; js-mode

(add-hook 'js-mode-hook
          '(lambda ()
             (flycheck-mode t)
             (setq js-indent-level 2)
             (setq indent-tabs-mode t)))

;;; ------------------------------------------------------------
;;; php-mode

;; (require 'my-php-mode)
;; (setq auto-mode-alist
;;        (append '(("\\.php$" . my-php-mode))
;;            auto-mode-alist))

(require 'php-mode)

(setq auto-mode-alist
       (append '(("\\.php$" . php-mode))
           auto-mode-alist))

(add-hook 'php-mode-hook
          '(lambda()

             (setq-local syntax-propertize-function
                         (syntax-propertize-rules
                          ;; html
                          ("\\(<\\)!--" (1 "< c"))
                          ("--[ \t\n]*\\(>\\)" (1 "> c"))))

             (setq tab-width 2)
             (setq c-basic-offset 2)
             ;; (setq indent-tabs-mode t)
             (setq php-speedbar-config nil)
             (setq php-template-compatibility nil)
             (setq php-mode-warn-if-mumamo-off nil)
             (setq php-mode-coding-style 'default)
             (setq php-manual-url "http://jp2.php.net/manual/ja/")
             (define-key php-mode-map ")" 'self-insert-command)
             (define-key php-mode-map "(" 'self-insert-command)
             (define-key php-mode-map "{" 'self-insert-command)
             (define-key php-mode-map "}" 'self-insert-command)
             (define-key php-mode-map "/" 'self-insert-command)
             (define-key php-mode-map "#" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; rainbow-mode
;; thx http://qiita.com/ironsand/items/cf8c582da3ec20715677

(require 'rainbow-mode)
(add-hook 'fundamental-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'lisp-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)


;;; ------------------------------------------------------------
;;; flycheck
;;; ------------------------------------------------------------

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; Flycheckのwindowは単独で表示
(add-to-list 'same-window-buffer-names "*Flycheck errors*")

;; キーバインド
(global-set-key (kbd "C-M-c") 'flycheck-buffer)
(global-set-key (kbd "C-M-l") 'flycheck-list-errors)
(global-set-key (kbd "<C-M-up>") 'flycheck-previous-error)
(global-set-key (kbd "<C-M-down>") 'flycheck-next-error)
(define-key flycheck-error-list-mode-map (kbd "C-g") 'quit-window)
(define-key flycheck-error-list-mode-map [escape] 'quit-window)

;; enable
(add-hook 'php-mode-hook 'flycheck-mode)
(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;;; ------------------------------------------------------------
;; 印刷設定
;; thx https://tamosblog.wordpress.com/2013/12/11/cocoa-emacs24_print/
;; see http://club.jidaikobo.com/knowledge/129.html

;; 普通の印刷
(global-set-key (kbd "s-p") 'print-buffer)

;; 文字化け対応
(defvar ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)


;;; ------------------------------------------------------------
;;; 起動時には最後に作業していたファイルを開く
;; gist-description: Emacs(Elisp): Preserve last buffers and its each point to reopen. 終了時のバッファとポイントを記憶して、起動時に同じ状態で開くelispです。
;; gist-id: 35b4d739a149f70e86298f71e5b1f9e7
;; gist-name: preserve-last-buffers-and-point.el
;; gist-private: nil

(defvar my-hist-dir (expand-file-name "~/.emacs.d/histories/"))
(defvar my-hist-last-files (concat my-hist-dir "last-files"))

(add-hook 'kill-emacs-hook
          (lambda ()
            (let ((strings "")
                  buf-path)
              (with-temp-buffer
                (dolist (buf (buffer-list))
                  (save-current-buffer
                    (setq buf-path (buffer-file-name buf))
                    (when (and buf-path (file-exists-p buf-path))
                      (set-buffer buf)
                      (setq strings
                            (concat strings "\n" buf-path ":" (number-to-string (point)))))))
                (insert (string-trim strings))
                (write-file my-hist-last-files)))))

(when (file-exists-p my-hist-last-files)
  (let (tmp
        path
        pt
        (files (split-string
                (with-temp-buffer
                  (insert-file-contents my-hist-last-files)
                  (buffer-string))
                "\n")))
    (when files
      (dolist (file files)
        (setq tmp (split-string file ":"))
        (setq path (car tmp))
        (setq pt (string-to-number(car (reverse tmp))))
        (when (file-exists-p path)
          (find-file path)
          (goto-char  pt))))))


;;; ------------------------------------------------------------
;;; Todo:
;;; ------------------------------------------------------------

;; portのEmacsを試してみる？
;; 自動クリーンアップをendlineのwhitespacesのみにする

;; search-centerの履歴？ でもあまり必要性を感じない……。むしろ検索セットか。

;; thx http://lioon.net/how-to-customize-face-emacs
;; M-x list-faces-display

;; 宝庫！ https://github.com/zk-phi/dotfiles/blob/master/emacs/init.el
;; https://github.com/zk-phi/indent-guide ためしたい

;;; ------------------------------------------------------------
;;; experimental area
;;; ------------------------------------------------------------

;; (global-set-key (kbd "C--") 'func)
;; (message "this-event: %s this-command: %s" last-input-event this-command)
;; (message "initial: %s point: %s" initial-point (point))

;;; jidaikobo.init.el ends here
