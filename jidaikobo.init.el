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
;; emacs --batch -l ~/.emacs.d/jdiaikobo/jidaikobo.init.el

;; cd ~/.emacs.d
;; mkdir elisp
;; wget https://www.emacswiki.org/emacs/download/ac-anything.el ac-anything.el

;;; Code:

;;; ------------------------------------------------------------
;;; 最小限設定 - 以降でエラーがあっても、最小限保証される設定

;; 設定ファイルのパス
(setq jidaikobo-dir (file-name-directory
										 (or (buffer-file-name) load-file-name)))
(setq dotfiles-dir (expand-file-name (concat jidaikobo-dir "../")))

;; ディレクトリ類
(defvar my-work-dir (expand-file-name "~/"))
(defvar my-fetch-app-dir (expand-file-name "~/"))

;; リージョンを上書きできるようにする
(delete-selection-mode t)

;; 選択範囲を可視化
(setq transient-mark-mode t)

;; C-kで行全体を対象にする
(setq kill-whole-line t)

;; スクロールを一行ずつにする
(setq scroll-step 1)

;; クリップボードを他のアプリケーションと共用にする
(setq x-select-enable-clipboard t)

;; font-lock-mode
(global-font-lock-mode t)

;; optキーをMetaキーに
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; yes/noをy/nへ
(fset 'yes-or-no-p 'y-or-n-p)

;; 起動画面を抑止
(setq inhibit-startup-message t)

;; スクラッチメッセージを抑止
(setq initial-scratch-message nil)

;; オートインデント無効
(ignore-errors (electric-indent-mode -1))

;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 自動保存を無効
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; ツールバーを非表示
(ignore-errors (tool-bar-mode -1))

;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f %%* Emacs@%s" (system-name)))

;; ミニバッファ履歴を保存
(savehist-mode 1)

;; ミニバッファでは半角英数で
(ignore-errors (mac-auto-ascii-mode 1))

;; タブキー
(setq-default tab-width 2)
(setq-default indent-tabs-mode t)

;; cua-modeの設定
(cua-mode t) ; cua-modeをオン。C-RETで矩形選択モードに
(setq-default cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;; 複数フレームを開かないようにする
(setq-default ns-pop-up-frames nil)

;; ファイルが #! から始まる場合、+xを付けて保存する
(add-hook 'after-save-hook
					'executable-make-buffer-file-executable-if-script-p)

;;; 関数名の表示
(which-func-mode 1)

;; emacsclient（使いたいのだけど、今のところうまくいかない）
(if (eq window-system 'ns) (server-start))

;; キーストロークのミニバッファへの表示を早く
(setq echo-keystrokes 0.1)

;; backward-delete-char-untabifyは、タブをバラさない
(setq backward-delete-char-untabify-method nil)

;; 原則画面分割しない
(setq display-buffer-function nil)

;; Helpバッファを常に選択する
(setq help-window-select t)

;; エラーがあったら*Backtrace*を開く
;; (add-hook 'after-init-hook
;; 					'(lambda () (setq debug-on-error t)))

;; 機能の有効化
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'delete-region 'disabled nil)

;;; ------------------------------------------------------------
;;; フレーム初期値

(add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
(ignore-errors (add-to-list 'default-frame-alist '(font . "ricty-16")))

;;; ------------------------------------------------------------
;;; 1日1回のチェック
;; gist-description: Emacs(Elisp): return t once in a day. 1日一回tを返すelispです。
;; gist-id: 33e072cea6aa96a19f58
;; gist-name: is-once-in-a-day.el
;; gist-private: nil

(defun is-once-in-a-day ()
	"Is once in a day."
	(interactive)
	(let* ((target-file (concat dotfiles-dir ".is-once-in-a-day"))
				 (target-update-at (if (file-exists-p target-file)
															 (nth 5 (file-attributes target-file))
														 (append-to-file "" nil target-file)))
				 (criteria-time (encode-time 0 0 0 (nth 3 (decode-time)) (nth 4 (decode-time)) (nth 5 (decode-time)) (nth 6 (decode-time)) (nth 7 (decode-time)) (nth 8 (decode-time))))
				 (ftime (if target-update-at
										(float-time (time-subtract criteria-time target-update-at))
									nil)))
		(cond ((and target-update-at ftime (> ftime 0))
					 (delete-file target-file)
					 (append-to-file "." nil target-file)
					 t)
					((and target-update-at ftime (< ftime 0))
					 nil)
					((not target-update-at)
					 (delete-file target-file)
					 (append-to-file "." nil target-file)
					 t))))

;;; ------------------------------------------------------------
;;; Packages

;; load-pathの追加
(add-to-list 'load-path jidaikobo-dir)
(add-to-list 'load-path (concat dotfiles-dir "elisp"))

;; package.override.el
(setq override-el (concat dotfiles-dir "package.override.el"))

;; 1日一回 load packages
(if (file-exists-p override-el)
		(load override-el)

	;; Packages
	(require 'package)
	(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
	;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
	(package-initialize)
	(when (or noninteractive (is-once-in-a-day)) (package-refresh-contents))

	;; my-packages
	(defvar my-packages
		'(anything
			descbinds-anything
			auto-complete
			undohist
			undo-tree
			recentf-ext
			cursor-chg
			smart-tab
			google-translate
			auto-async-byte-compile
			multiple-cursors
			smartrep
			flycheck
			php-mode
			web-mode
			mic-paren
			gtags
			tabbar
			foreign-regexp
			rainbow-mode
			popwin
			yagist
			web-beautify
;;			key-chord
			elscreen))

	;; my-packagesからインストールしていないパッケージをインストール
	(dolist (package my-packages)
		(unless (package-installed-p package)
			(package-install package))))

;;; ------------------------------------------------------------
;;; jidaikobo's elisp.

;; 検索センター
;; search-center
(custom-set-variables
 '(sc/is-use-super t)
 '(sc/split-direction "vertical"))
(require 'search-center)
(search-center-mode t)

;; HTMLのマークアップのキーバインド集
;; web-authoring-set
(require 'web-authoring-set)

;;; ------------------------------------------------------------
;;; jidaikobo's theme

(add-to-list 'custom-theme-load-path
						 (file-name-as-directory (concat jidaikobo-dir "themes/")))
(load-theme 'jidaikobo-dark t)

;;; ------------------------------------------------------------
;;; localize

(add-to-list 'Info-directory-list "~/info")

;;; ------------------------------------------------------------
;;; キーボード操作

;; mac-likeなcmd関係
;; thx http://d.hatena.ne.jp/gan2/20080109/1199887209
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(global-set-key (kbd "s-a") 'mark-whole-buffer) ; select all (cmd+a)
(global-set-key (kbd "s-c") 'kill-ring-save) ; copy (cmd+c)
(global-set-key (kbd "s-x") 'kill-region) ; cut (cmd+x)
(global-set-key (kbd "s-v") 'yank) ; paste (cmd+v)
(global-set-key (kbd "s-s") 'save-buffer) ; save (cmd+s)
(global-set-key (kbd "s-S") 'write-file) ; save as (cmd+shift+s)
(global-set-key (kbd "s-o") 'find-file) ; open (cmd+o)
(global-set-key (kbd "s-z") 'undo-tree-undo) ; undo (cmd+z)
(global-set-key (kbd "s-Z") 'undo-tree-redo) ; redo (cmd+shift+z)
(global-set-key (kbd "s-+") 'text-scale-increase) ; resize increase (cmd++)
(global-set-key (kbd "<s-kp-add>") 'text-scale-increase) ; resize increase (cmd++)
(global-set-key (kbd "s--") 'text-scale-decrease) ; resize decrease (cmd+-)
(global-set-key (kbd "<s-kp-subtract>") 'text-scale-decrease) ; resize decrease (cmd+-)
(global-set-key (kbd "<s-kp-equal>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "s-=") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "<s-kp-0>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "<s-0>") (lambda () (interactive) (text-scale-mode 0)))
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ; quit (cmd+q)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer) ; cmd+up
(global-set-key (kbd "<s-down>") 'end-of-buffer) ; cmd+down
(global-set-key (kbd "<s-left>") 'beginning-of-line) ; cmd+left
(global-set-key (kbd "<s-right>") 'end-of-line) ; cmd+right
(global-set-key (kbd "<backspace>") 'delete-backward-char) ; delete
(global-set-key (kbd "<backtab>") 'indent-for-tab-command)
(global-set-key (kbd "<C-tab>") 'indent-for-tab-command)
;; (global-set-key (kbd "<C-up>") 'backward-paragraph) ; Control-down
;; (global-set-key (kbd "<C-down>") 'forward-paragraph) ; Control-down
;; (global-set-key (kbd "M-right") 'forward-symbol)
;; (global-set-key (kbd "M-left") (lambda () (interactive) (forward-symbol -1)))

;; 日本語入力のときだけM-/で、"／"を入力したい
(global-set-key (kbd "M-/") (lambda () (interactive)
															(if (fboundp 'mac-input-source)
																	(let ((mac-input-source (mac-input-source)))
																		(if (string-match
																				 "com.apple.inputmethod.Kotoeri.japanese"
																				 mac-input-source)
																				(insert "／")
																			'dabbrev-expand))
																'dabbrev-expand)))

;; ミニバッファでは英数字入力に
(when (functionp 'mac-auto-ascii-mode)
    (mac-auto-ascii-mode 1))

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

;; escでM-g
;; http://emacswiki.org/emacs/CancelingInEmacs
(setq-default normal-escape-enabled t)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(global-set-key (kbd "<escape>") 'keyboard-quit) ; everywhere else
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] 'keyboard-quit) ; minibuffer

;; M-g or cmd+opt+j で指定行へジャンプ
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s-j") 'goto-line)

;; opt+¥でバックスラッシュを入力
(global-set-key (kbd "M-¥") "\\")

;;; ------------------------------------------------------------
;;; 自分好みのタブの振る舞い
;; ewww/read-onlyバッファではリンクの移動
;; ミニバッファだったらミニバッファ補完
;; 直前の動作が改行か選択範囲があったらインデント
;; あとは\tを挿入

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
	 ;; Anythingのときにはアクション選択
	 (anything-alive-p
		(anything-select-action))
	 ;; ミニバッファだったらミニバッファ補完
	 ((minibufferp (current-buffer))
		(minibuffer-complete))
	 ;; 選択範囲に改行を含んでいるか、直前がエンターだったらインデント
	 ((or (memq last-command '(newline))
				(and mark-active
						 (string-match
							"\n$"
							(buffer-substring-no-properties (region-beginning)
																							(region-end)))))
		(indent-for-tab-command))
	 ;; タブを挿入
	 (t
		(when mark-active (delete-region (region-beginning) (region-end)))
		(insert "\t"))))

(global-set-key (kbd "<tab>") 'my-tab-dwim)

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

;;; ------------------------------------------------------------
;;; align-regexpが、スペースを詰めるように

(defadvice align-regexp (around advise-align-regexp activate)
  "Let align-regexp indent by spaces."
  (setq indent-tabs-mode nil)
  ad-do-it
  (setq indent-tabs-mode t))

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
			(setq target "^;;; ----+$"))
		 ((string= major-mode "php-mode")
			(setq target "^\t*function\\|^\t*class\\|^\t*private\\|^\t*public"))
		 ((string= major-mode "web-mode")
			(setq target "^\t*<h"))
		 (t
			(setq target "^;;; ----+$\\|^■\\|^///")))
		(if (string= direction "prev")
				(re-search-backward target)
			(re-search-forward target))))

(global-set-key (kbd "<M-s-down>") (lambda () (interactive) (next-block "next")))
(global-set-key (kbd "<M-s-up>") (lambda () (interactive) (next-block "prev")))

;;; ------------------------------------------------------------
;;; 釣り合いのとれる括弧のハイライト
;; 少々大袈裟だけれど、括弧同士のハイライトがカーソルの邪魔なのでアンダーラインにする

(require 'mic-paren)
(paren-activate)
(setq paren-match-face 'underline paren-sexp-mode t)
(setq paren-sexp-mode t)

;;; ------------------------------------------------------------
;;; 一行目と最終行での上下キーの振る舞い（行末と行頭へ）

(defvar prev-line-num (line-number-at-pos))
(add-hook 'post-command-hook 'my-goto-the-edge)
(defun my-goto-the-edge ()
	"Go to the edge of the line."
	;; (message "this-event: %s\nthis-command: %s" last-input-event this-command)
	(when (and (eq prev-line-num 1) (memq last-input-event '(up S-up)))
		(beginning-of-line))
	(when (and (eq prev-line-num (count-lines 1 (point-max)))
						 (memq last-input-event '(down S-down)))
		(end-of-line))
	(setq prev-line-num (line-number-at-pos)))

;;; ------------------------------------------------------------
;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
;; macふうの挙動だが、Emacsふうでないので、ちょっと様子見しつつ運用
;; C-@とどちらをとるか悩ましい

(defvar is-deactivate-region nil)
(when is-deactivate-region
	;; regionの解除advice版 - Hookよりこちらのほうが軽い!?
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

		(when (and (use-region-p)
							 (not (memq last-input-event '(S-left S-right S-down S-up))))
			;; (message "r: %s" (memq last-input-event '(S-left S-right S-down S-up)))
			(cond
			 ((memq last-input-event '(right down))
				(goto-char (region-end)))
			 ((memq this-command '(left-char previous-line))
				(goto-char (region-beginning))))
			(deactivate-mark))))

;;; ------------------------------------------------------------
;;; popwin
;; thx http://d.hatena.ne.jp/m2ym/20110228/1298868721

(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)

;; anything
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)

;; Messages buffer
(push '("*Messages*" :height 10 :stick t :position bottom :tail t :noselect t) popwin:special-display-config)

;; Backtrace buffer
(push '("*Backtrace*" :height 10) popwin:special-display-config)

;; grep/rgrep buffer
(push '("*grep*" :height 10 :stick t :position bottom :noselect nil) popwin:special-display-config)

;; key-binds
(global-set-key (kbd "M-p p") 'popwin:display-last-buffer)
(global-set-key (kbd "M-p m") (lambda () (interactive)
																(display-buffer "*Messages*")
																(auto-revert-mode 1)))

;;; ------------------------------------------------------------
;;; 複数箇所選択と編集

;; multiple-cursors and smartrep
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

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
;;; undo関連

;; undohist
;; ファイルを閉じてもundoの履歴を残す
(require 'undohist)
(undohist-initialize)

;; undo-tree
;; redo (cmd+shft+z)
(require 'undo-tree)
(global-undo-tree-mode t)

;; undo in regionしない
(defadvice undo-tree-undo (before deactivate-region activate)
	"Deactivate Region when attempt to undo."
	(deactivate-mark))

;;; ------------------------------------------------------------
;;; recentf

(require 'recentf-ext)
(recentf-mode 1)
(setq recentf-exclude '("/TAGS$"
												"/var/tmp/"
												".recentf"
												"^/[^/:]+:" ; TRAMP
												".+Fetch Temporary Folder.+"))
(setq recentf-max-saved-items 1000)

;;; ------------------------------------------------------------
;;; Anything関連

;; Anything
(require 'anything)
(require 'anything-config)

(setq alist-anything-for-files
			'((anything-c-source-emacs-commands
				 anything-c-source-gtags-select)
				;; anything-c-source-emacs-commands
				;; anything-c-source-files-in-current-dir+
				;; anything-c-source-buffers-list ;; *のバッファでAnythingを止めることがある
				anything-c-source-find-by-gtags
				anything-c-source-bookmarks
				anything-c-source-recentf))

;; M-xによる補完をAnythingで行なう
(require 'anything-complete)
(anything-read-string-mode 1)

;; Anythingでファイルを開く方法をFind file as rootにしたときにsudoで開くように
(setq anything-su-or-sudo "sudo")

;;; ------------------------------------------------------------
;;; あればgtagsを起点にしてfindし、なければカレントディレクトリを対象にした情報源

(defvar anything-c-source-find-by-gtags
	'((name . "Find by gtags or ls")
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
;;; FTP by Fetch

(defun func-anything-c-source-my-fetch ()
	(let (ret)
		(with-temp-buffer
			(insert
			 (shell-command-to-string
				(concat "find " my-fetch-app-dir " -name \"*_NON_*\" -prune -o -name \"*.app\"")))
			(ucs-normalize-NFC-region (point-min) (point-max))
			(setq ret (split-string (buffer-string) "\n")))
		ret))

;; 結果がなければたさない
(when (func-anything-c-source-my-fetch)
	(defvar anything-c-source-my-fetch
		'((name . "open Fetch.app")
			(candidates . (lambda () (interactive) (func-anything-c-source-my-fetch)))
			(type . file)
			(action . (("open Fetch" . anything-fetch-open)))))

	(defun anything-fetch-open (app)
		"Fetch open.  APP is path."
		(shell-command (concat "open " app)))

	(add-to-list 'alist-anything-for-files 'anything-c-source-my-fetch t))

;;; ------------------------------------------------------------
;;; よく使うプロジェクトに対する操作

(defun func-anything-c-source-cd-to-projects ()
	(let (ret)
		(with-temp-buffer
			(insert
			 (shell-command-to-string (concat "find " my-work-dir " -maxdepth 1 -type d")) "\n")
			(ucs-normalize-NFC-region (point-min) (point-max))
			(setq ret (split-string (buffer-string) "\n")))
		ret))

;; 結果がなければたさない
(when (func-anything-c-source-cd-to-projects)
	(defvar anything-c-source-cd-to-projects
		'((name . "cd to projects")
			(candidates . (lambda () (interactive) (func-anything-c-source-cd-to-projects)))
			(action . (("Change internal directory" . anything-change-internal-directory)
								 ("Dired" . anything-project-dired)
								 ("Generate gtags at project" . anything-generate-gtags-at-project)))))

	(defun anything-change-internal-directory (dir)
		"Change internal directory at Sites.  DIR is path."
		(cd dir))

	(defun anything-project-dired (dir)
		"Dired.  DIR is path."
		(dired dir))

	(defun anything-generate-gtags-at-project (dir)
		"Generate gtags at project.  DIR is path."
		(shell-command-to-string (concat "cd " dir " ; gtags -v")))

	(add-to-list 'alist-anything-for-files 'anything-c-source-cd-to-projects))

;;; ------------------------------------------------------------
;;; 編集対象でないバッファを除外(必要な場合、switch-to-buffer)
;; thx https://github.com/skkzsh/.emacs.d/blob/master/conf/anything-init.el

(setq anything-c-boring-buffer-regexp
			(rx "*" (+ not-newline) "*"))

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
;;; Encode and Line folding

(defvar anything-c-source-coding-system
	'((name . "Encode and Line Folding")
		(candidates . (lambda () '("set UTF-8"
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
;;; my-anything-for-functions

(defun my-anything-for-functions ()
	"Anything command for program."
	(interactive)
	(anything-other-buffer
	 '(anything-c-source-imenu
		 anything-c-source-emacs-commands
		 anything-c-source-emacs-functions)
	 "*my-anything-for-functions*"))

(global-set-key (kbd "C-,") 'my-anything-for-functions)

;;; ------------------------------------------------------------
;;; descbinds-anythingの乗っ取り
;; thx http://d.hatena.ne.jp/buzztaiki/20081115/1226760184

(require 'descbinds-anything)
(descbinds-anything-install)
(global-set-key (kbd "C-.") 'descbinds-anything)

;;; ------------------------------------------------------------
;;; gtags

(require 'gtags)
(setq gtags-path-style 'relative)

;; キーバインド
(setq-default gtags-mode-hook
							'(lambda ()
								 (local-set-key "\M-t" 'gtags-find-tag)
								 (local-set-key "\M-r" 'gtags-find-rtag)
								 (local-set-key "\M-s" 'gtags-find-symbol)
								 (local-set-key "\M-T" 'gtags-pop-stack)))

(setq-default gtags-select-mode-hook
							'(lambda ()
								 (local-set-key (kbd "RET") 'gtags-select-tag)))

;; gtags-mode を使いたい mode の hook に追加する
(add-hook 'php-mode-hook
					'(lambda()
						 ;; (gtags-make-complete-list)
						 (gtags-mode 1)))

;; update GTAGS
;; thx http://qiita.com/yewton/items/d9e686d2f2a092321e34
(defun update-gtags (&optional prefix)
	"Update gtags.  PREFIX."
	(interactive "P")
	(let ((rootdir (gtags-get-rootpath))
				(args (if prefix "-v" "-iv")))
		(when rootdir
			(let* ((default-directory rootdir)
						 (buffer (get-buffer-create "*update GTAGS*")))
				(save-excursion
					(set-buffer buffer)
					(erase-buffer)
					(let ((result (process-file "gtags" nil buffer nil args)))
						(if (= 0 result)
								(message "GTAGS successfully updated.")
							(message "update GTAGS error with exit status %d" result))))))))

(add-hook 'after-save-hook 'update-gtags)

;;; ------------------------------------------------------------
;;; タブ関連 - elscreen or tabbar

;; elscreen or tabbar
(defvar is-use-tabbar nil)
(defvar is-use-elscreen nil)

;;; ------------------------------------------------------------
;;; tabbar

(when is-use-tabbar
	(require 'tabbar)
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

	;; new tab
	(defun my-new-tab ()
		"My new tab"
		(interactive)
		(let ((bufname (format-time-string "%y%m%d%H%M%S" (current-time))))
			(get-buffer-create bufname)
			(switch-to-buffer bufname)
			(text-mode)))

	;; move-current-tab-to-top
	;; gist-description: Emacs(Elisp): move current tab (buffer) to top at tabbar-mode. tabbarで選択中のタブ（バッファ）を左端に移動します。
	;; gist-id: 54dab2fc5f2e278833f5
	;; gist-name: move-current-tab-to-top.el
	;; gist-private: nil

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
	(global-set-key (kbd "s-t") 'my-new-tab)
	(global-set-key (kbd "C-s-t") 'move-current-tab-to-top)

	;; 幾つかのウィンドウでは、タブ移動しない
	(defadvice tabbar-forward-tab (around advise-tabbar-forward-tab activate)
		"Do not forward at specified baffers."
		(if (member (buffer-name) '("*RE-Builder*" "*Messages*" "*grep*"))
			nil
			ad-do-it))
	(defadvice tabbar-backward-tab (around advise-tabbar-backward-tab activate)
		"Do not backward at specified baffers."
		(if (member (buffer-name) '("*RE-Builder*" "*Messages*" "*grep*"))
			nil
			ad-do-it)))

;;; ------------------------------------------------------------
;;; elscreen

(when is-use-elscreen
	(require 'elscreen)
	(elscreen-start)

	;; タブの先頭に[X]を表示しない
	(setq-default elscreen-tab-display-kill-screen nil)

	;; header-lineの先頭に[<->]を表示しない
	(setq-default elscreen-tab-display-control nil)

	;; キーバインド
	(global-set-key (kbd "<M-s-right>") 'elscreen-next)
	(global-set-key (kbd "<M-s-left>") 'elscreen-previous)

	;; 新しいスクリーン
	(global-set-key (kbd "s-t") (lambda () (interactive)
																(elscreen-create)
																(switch-to-buffer "*scratch*")
																(text-mode))))

;;; ------------------------------------------------------------
;;; ウィンドウ/スクリーンを閉じる

(defun my-delete-windows ()
	"Contexual delete windows."
	(interactive)
	(cond
	 ;; ウィンドウ構成が多ければまず他のウィンドウを消してから、ウィンドウを消す
	 ((not (one-window-p)) (delete-other-windows) (my-delete-windows))
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
	 ;; screenが複数だったらelscreen-kill
	 ((and is-use-elscreen (not (elscreen-one-screen-p))) (elscreen-kill))
	 ;; screenがひとつだったらkill-buffer
	 ((and is-use-elscreen (elscreen-one-screen-p)) (kill-buffer))
	 ;; kill-buffer for is-use-tabbar and other situation
	 (t (kill-buffer))))

(global-set-key (kbd "s-w") 'my-delete-windows)

;;; ------------------------------------------------------------
;;; カーソル関連

;; cursor-chg
;; カーソルの色と形状を変更（ブロックカーソルが苦手なので）
(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 0)
(setq curchg-default-cursor-color "White")
(setq curchg-input-method-cursor-color "firebrick")
(setq curchg-change-cursor-on-input-method-flag t)

;; C-aで、開始場所と先頭をトグル
;; thx http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
(defun my-goto-line-beginning-or-indent (&optional position)
	"Goto line beginning or indent.  POSITION is optical."
	(interactive)
	(or position (setq position (point)))
	(let (($starting-position (progn (back-to-indentation) (point))))
		(if (eq $starting-position position)
				(move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'my-goto-line-beginning-or-indent)

;;; ------------------------------------------------------------
;;; 行設定

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
	(global-linum-mode t)
	(setq-default linum-format "%5d: "))

(add-hook 'emacs-lisp-mode-hook 'show-line-number)
(add-hook 'lisp-mode-hook 'show-line-number)
(add-hook 'js-mode-hook 'show-line-number)
(add-hook 'text-mode-hook 'show-line-number)
(add-hook 'fundamental-mode-hook 'show-line-number)
(add-hook 'php-mode-hook 'show-line-number)
(add-hook 'css-mode-hook 'show-line-number)
(add-hook 'web-mode-hook 'show-line-number)
(add-hook 'html-mode-hook 'show-line-number)

;;; ------------------------------------------------------------
;;; モードライン設定

;; 何文字目にいるか表示
(column-number-mode 1)

;; 改行の種類表示の変更
;; thx https://github.com/moriyamahiroshi/hm-dot-emacs-files/blob/master/init.el
(setq-default eol-mnemonic-unix "(LF)")
(setq-default eol-mnemonic-dos "(CRLF)")
(setq-default eol-mnemonic-mac "(CR)")

;;; ------------------------------------------------------------
;;; マウス設定

;; shift+clickでregion作成
;; thx http://superuser.com/questions/521223/shift-click-to-extend-marked-region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;;; ------------------------------------------------------------
;;; 自動バイトコンパイル

;; auto-async-byte-compile
;; thx http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "init.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; ------------------------------------------------------------
;;; s+RETでeval-bufferかeval-region

(global-set-key (kbd "<s-return>") (lambda () (interactive)
																		 (if (region-active-p)
																				 (eval-region (region-beginning) (region-end))
																			 (eval-buffer))
																		 (message "eval done.")))

;;; ------------------------------------------------------------
;;; ファイラ (dired)

;; diredでファイル編集（rで編集モードに）
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; C-x C-f で現在位置を開く
(ffap-bindings)

;; diredでマークをつけたファイルを開く（F）
(eval-after-load "dired"
	'(progn
		 (define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
		 (defun my-dired-find-marked-files (&optional arg)
			 "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
			 (interactive "P")
			 (let* ((fn-list (dired-get-marked-files nil arg)))
				 (mapc 'find-file fn-list)))))

;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)

;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;; diredでタブを開きすぎないようにする
;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; RET 標準の dired-find-file では dired バッファが複数作られるのでdired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;; anything in dired
;; thx http://syohex.hatenablog.com/entry/20120105/1325770778
;; pを押すと、anything
(defun my/anything-dired ()
	"Press p to into anything mode."
	(interactive)
	(let ((curbuf (current-buffer)))
		(if (anything-other-buffer
				 '(anything-c-source-files-in-current-dir+)
				 "*anything-dired*")
				(kill-buffer curbuf))))

(define-key dired-mode-map (kbd "C-;") 'my/anything-dired)

;;; ------------------------------------------------------------
;;; TRAMP

(require 'tramp)

;; TRAMPでは自動バックアップしない
(add-to-list 'backup-directory-alist
						 (cons tramp-file-name-regexp nil))

;; FTPではパッシブモードでの接続を試みる（使わないけど）
(setq-default ange-ftp-try-passive-mode t)

;; (setq explicit-shell-file-name "bash")
;; (add-to-list 'tramp-remote-path "/usr/local/bin/bash")
;; (shell-command-to-string "/usr/local/bin/bash/pwd")
;; (insert (format "%s" shell-prompt-pattern))

;;; ------------------------------------------------------------
;;; whitespace関連設定

(require 'whitespace)

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

;; whitespaceの可視化
(global-whitespace-mode 1)

;;; ------------------------------------------------------------
;;; 次/前の空行
;; gist-description: Emacs(Elisp): forward/backward-paragraphだとparagraph判定がおそらくシンタックステーブル依存になり、字義通りの「次の空行」にならないので、別途用意。選択範囲をものぐさして作りたいので、ちょっとfork。thx https://gist.github.com/jewel12/2873112
;; gist-id: ad27b19dd3779ccc1ff2
;; gist-name: move(region)-to-next(previous)-blank-line.el
;; gist-private: nil

(defun blank-line? ()
	(string-match "^\n$" (substring-no-properties (thing-at-point 'line))))

(defun move-to-next-blank-line ()
	(interactive)
	(progn (forward-line 1)
				 (unless (blank-line?) (move-to-next-blank-line))))

(defun region-to-next-blank-line ()
	(interactive)
	(when (and (not (memq last-command '(region-to-next-blank-line)))
						 (not mark-active))
		(set-mark-command nil))
	(move-to-next-blank-line))

(defun move-to-previous-blank-line ()
	(interactive)
	(progn (forward-line -1)
				 (unless (blank-line?) (move-to-previous-blank-line))))

(defun region-to-previous-blank-line ()
	(interactive)
	(when (and (not (memq last-command '(region-to-previous-blank-line)))
						 (not mark-active))
		(set-mark-command nil))
	(move-to-previous-blank-line))

(global-set-key (kbd "<C-up>") 'move-to-previous-blank-line)
(global-set-key (kbd "<C-down>") 'move-to-next-blank-line)
(global-set-key (kbd "<C-S-up>") 'region-to-previous-blank-line)
(global-set-key (kbd "<C-S-down>") 'region-to-next-blank-line)

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
	"Calculate natural text of region and insert to current buffer."
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
		(when (string-match "\.$" result) (setq result (substring result 0 (match-beginning 0))))
		(insert result)))
(global-set-key (kbd "M-c") 'calculate-region-and-insert)

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

;; 確実に変換
(put-char-code-property ?， 'jisx0208 ?,)
(put-char-code-property ?． 'jisx0208 ?.)

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
;;; インデント整形
;; gist-description: Emacs(Elisp): indent.elにそのままの機能があったので、そちらに置き換え。
;; gist-id: f941e7f365872920c7f8
;; gist-name: my-indext-region.el
;; gist-private: nil

(global-set-key (kbd "s-}") 'indent-rigidly-right)
(global-set-key (kbd "s-]") 'indent-rigidly-right)
(global-set-key (kbd "s-{") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "s-[") 'indent-rigidly-left-to-tab-stop)

;;; ------------------------------------------------------------
;;; 現在バッファのファイルのフルパスを取得

(defun get-current-path ()
	"Get current file path."
	(interactive)
	(insert (or (buffer-file-name) (expand-file-name default-directory))))

(global-set-key (kbd "M-s-k") 'get-current-path)

;;; ------------------------------------------------------------
;;; Finderで現在バッファのファイルを表示

(defun point-current-buffer-by-finder ()
	"Point current buffer by Mac Finder."
	(interactive)
	(shell-command (concat "open " (expand-file-name default-directory))))

(global-set-key (kbd "M-s-K") 'point-current-buffer-by-finder)

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
;;; flycheck

(load "flycheck")
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(global-set-key (kbd "<M-up>") 'flycheck-previous-error) ; previous error (M+up)
(global-set-key (kbd "<M-down>") 'flycheck-next-error) ; next error (M+down)
;; (add-hook 'after-init-hook #'global-flycheck-mode) ; HTMLでは抑止
(add-hook 'php-mode-hook 'flycheck-mode)

;;; ------------------------------------------------------------
;;; rainbow-mode
;; thx http://qiita.com/ironsand/items/cf8c582da3ec20715677

(require 'rainbow-mode)
(add-hook 'fundamental-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'lisp-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;;; ------------------------------------------------------------
;;; html-mode

;(defun xoops-smarty-comment-setting ()
;  (make-local-variable 'comment-start)
;  (setq comment-start "<{*")
;  (make-local-variable 'comment-end)
;  (setq comment-end "*}>")
;  (make-local-variable 'comment-multi-line)
;  (setq comment-multi-line t))
;(add-hook 'html-mode-hook 'xoops-smarty-comment-setting)

;; html-mode-hook
(add-hook 'html-mode-hook
					'(lambda()
						 ;; (font-lock-add-keywords nil '(("<{\\*\\([^^J]\\|^J\\)+?\\*}>" . font-lock-comment-face)))
						 (define-key html-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; grep-mode

(add-hook 'grep-mode-hook
					'(lambda()
						 (define-key grep-mode-map (kbd "C-o") (lambda () (interactive) (other-window 1)))
						 (define-key grep-mode-map (kbd "C-S-o") (lambda () (interactive) (other-window -1)))))

;;; ------------------------------------------------------------
;;; web-mode

(require 'web-mode)

;; web-mode-hook
(add-hook 'web-mode-hook
					'(lambda()
						 (setq web-mode-markup-indent-offset 2)
						 (define-key web-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; css-mode

(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode) auto-mode-alist))
(add-hook 'css-mode-hook
					(lambda ()
						(setq css-indent-offset 2)
						(setq cssm-indent-function #'cssm-c-style-indenter)))

;;; ------------------------------------------------------------
;;; js-mode

(add-hook 'js-mode-hook '(lambda ()
														(flycheck-mode t)
														(setq js-indent-level 2)
														(setq indent-tabs-mode t)))

;;; ------------------------------------------------------------
;;; php-mode

(require 'php-mode)

;; php-mode-hook
(add-hook 'php-mode-hook
					'(lambda()
						 (setq tab-width 2)
						 (setq indent-tabs-mode t)
						 (setq c-basic-offset 2)
						 (define-key php-mode-map ")" 'self-insert-command)
						 (define-key php-mode-map "(" 'self-insert-command)
						 (define-key php-mode-map "{" 'self-insert-command)
						 (define-key php-mode-map "}" 'self-insert-command)
						 (define-key php-mode-map "/" 'self-insert-command)
						 (define-key php-mode-map "#" 'self-insert-command)
						 (setq php-manual-url "http://jp2.php.net/manual/ja/")
						 (define-key php-mode-map (kbd "<tab>") 'my-tab-dwim)))

;; thx http://d.hatena.ne.jp/Tetsujin/20070614/1181757931
(require 'align)
(add-to-list 'align-rules-list
             '(php-assignment
               (regexp . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
               (justify .t)
               (tab-stop . nil)
               (modes . '(php-mode))))
(add-to-list 'align-dq-string-modes 'php-mode)
(add-to-list 'align-sq-string-modes 'php-mode)
(add-to-list 'align-open-comment-modes 'php-mode)
(setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
                                    "\\([({}\\(/\*\\)]$\\)\\|"
                                    "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
                                    "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
                                    "switch\\|case\\|break\\|continue\\|do\\)[ ;]\\)"
                                    ))

;;; ------------------------------------------------------------
;;; text-mode

;; テキストモードでもすこしカラーリングする
;; thx http://lioon.net/how-to-customize-face-emacs
;; M-x list-faces-display
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
;;; kontiki-mode

;; ワイアフレームモード
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

;; メールモード（mail-mode）のカラーリング
(add-hook 'mail-mode-hook
					'(lambda()
						 (font-lock-add-keywords nil '(("^> .+" . font-lock-keyword-face)))
						 (font-lock-add-keywords nil '(("^>> .+" .font-lock-type-face)))
						 (font-lock-add-keywords nil '(("^>>>.+" . font-lock-string-face)))))

;;; ------------------------------------------------------------
;;; フレームの大きさと位置を変更 (cmd+shift+w)

(defun resize-selected-frame ()
	"Resize frame to jidaikobo's default."
	(interactive)
	(set-frame-position (selected-frame) 0 0)
	(set-frame-size (selected-frame) 100 55))
;; (if (= (frame-width) 200)
;; 		(set-frame-size (selected-frame) 100 55)
;; 	(set-frame-size (selected-frame) 200 55)))

(global-set-key (kbd "s-W") 'resize-selected-frame)

;;; ------------------------------------------------------------
;;; auto-complete

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim nil)
(setq ac-ignore-case t)
(setq ac-disable-faces nil)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "<M-tab>") 'auto-complete)
(define-key ac-mode-map (kbd "M-/") 'auto-complete)

;; ac-anything
(when (require 'ac-anything nil t)
	(define-key ac-complete-mode-map (kbd "<M-tab>") 'ac-complete-with-anything)
	(define-key ac-complete-mode-map (kbd "M-/") 'ac-complete-with-anything))

;; hooks
(add-hook 'html-mode-hook
					'(lambda()
						 (auto-complete-mode t)
						 (define-key html-mode-map (kbd "<M-tab>") 'auto-complete)
						 (define-key html-mode-map (kbd "M-/") 'auto-complete)))

(add-hook 'php-mode-hook
					'(lambda()
						 (define-key php-mode-map (kbd "<M-tab>") 'auto-complete)
						 (define-key html-mode-map (kbd "M-/") 'auto-complete)))

(add-hook 'kontiki-mode-hook
					'(lambda()
						 (define-key php-mode-map (kbd "<M-tab>") 'auto-complete)
						 (define-key html-mode-map (kbd "M-/") 'auto-complete)))

;; ユーザ辞書ディレクトリ
(defvar ac-user-dict-dir (concat jidaikobo-dir "ac-dict/"))

;; 英語
;; thx http://tech.basicinc.jp/Mac/2013/08/04/linux_command/
(defvar ac-english-cache
	(ac-file-dictionary "/usr/share/dict/words"))
(defvar ac-english-dict
	'((candidates . ac-english-cache)))

;; 技術語
(defvar ac-technical-term-cache
	(ac-file-dictionary (concat ac-user-dict-dir "technical-term")))
(defvar ac-technical-term-dict
	'((candidates . ac-technical-term-cache)))

;; 条件の追加
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers
													 ;; ac-source-filename
													 ac-source-symbols
													 ac-technical-term-dict
													 ac-english-dict))

;; auto-complete の候補に日本語を含む単語が含まれないようにする
;; thx http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
(defadvice ac-word-candidates (after remove-word-contain-japanese activate)
	(let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
		(setq ad-return-value
					(remove-if contain-japanese ad-return-value))))

;;; ------------------------------------------------------------
;;; gist
;; thx http://d.hatena.ne.jp/mhayashi1120/20120920/1348144820

(require 'yagist)

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
;; gist-description: Emacs(Elisp): create or update gist by using yagist. yagistでregionのgistをupdateする。
;; gist-id: a20cd2d106edba225115
;; gist-name: yagist-region-create-or-update.el
;; gist-private: nil

(defun yagist-region-create-or-update (beg end)
  "Post the current region as a create or update at gist.github.com
After create copies the URL into the kill ring.
If gist-id exists update gist."
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
			(error "lack of parameters"))))

(global-set-key (kbd "C-M-g") 'yagist-region-create-or-update)

;;; ------------------------------------------------------------
;;; jump-match-paren
;; thx https://gist.github.com/donghee/3937661

(defun jump-match-paren (arg)
	"Go to the matching parenthesis."
	(interactive "p")
	(cond ((looking-at "\\s\(\\|\\s\[") (forward-list 1) (backward-char 1))
				((looking-at "\\s\)\\|\\s\]") (forward-char 1) (backward-list 1))
				(t (back-to-indentation))))

(global-set-key (kbd "C-b") 'jump-match-paren)

;;; ------------------------------------------------------------
;; 印刷設定
;; thx https://tamosblog.wordpress.com/2013/12/11/cocoa-emacs24_print/
;; see http://club.jidaikobo.com/knowledge/129.html

;; 普通の印刷
(global-set-key (kbd "s-p") 'print-buffer)

;; 文字化け対応
(setq ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)

;; 印刷プレビュー
(when (load-file (concat jidaikobo-dir "pdf-preview.el"))
	(require 'pdf-preview)
	(setq pdf-preview-preview-command "open -a Preview.app")
	(global-set-key (kbd "s-P")
									(lambda ()
										(interactive)
										(when (and
													 (yes-or-no-p "Show current buffer by Preview.app?")
													 (or
														(<= (length (buffer-string)) 10000)
														(and (> (length (buffer-string)) 10000)
																 (yes-or-no-p "Current buffer is large. Preview takes quite time. Preview this?"))))
											(pdf-preview-buffer)))))

;;; ------------------------------------------------------------
;;; eww

(require 'eww)

(define-key eww-mode-map (kbd "<backtab>") 'shr-previous-link)
(define-key eww-mode-map "r" 'eww-reload)
(define-key eww-mode-map "c 0" 'eww-copy-page-url)
(define-key eww-mode-map "p" 'scroll-down)
(define-key eww-mode-map "n" 'scroll-up)
(setq eww-search-prefix "http://www.google.co.jp/search?q=")
(setq eww-download-directory "~/Desktop")

;; eww で色を反映しない
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
	(unless eww-disable-colorize
		(funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
	"eww で文字色を反映させない"
	(interactive)
	(setq-local eww-disable-colorize t)
	(eww-reload))

;; ewwを複数開く
(when (fboundp 'eww)
	(progn
		(defun xah-rename-eww-hook ()
			"Rename eww browser's buffer so sites open in new page."
			(rename-buffer "eww" t))
		(add-hook 'eww-mode-hook 'xah-rename-eww-hook)))

;;; ------------------------------------------------------------
;;; Todo:
;; doctypeを見てのbrやタグの挿入
;; 単語境界をもうちょっと細かくしたい（シンタックステーブルか？）
;; 複数の検索置換セット
;; portのEmacsを試してみる？
;; ac-anythigをいきなり起動できないか
;; ac-anythingのC--を変更。C--は、僕が開発に使うので。
;; curchg-input-method-cursor-colorの挙動を確認
;; デフォルトのinput methodを確認して、keyboard masetroとの合わせ技でIMをいじる？
;; リージョン解除関数がおかしい。C-@でマークしたら、たしかに解除するが、C-S-downのあとS-downしたりするとリージョンが解除される。ここは微調整したいところなので、なんとかしたい。S-downにadviceして、markを維持するような措置が必要？？
;; 「幾つかのウィンドウでは、タブ移動しない」popwinを判定すると良い？
;; 印刷設定を http://d.hatena.ne.jp/r_takaishi/20110304/1299203553 を参考に改良してみる

;;; ------------------------------------------------------------
;;; experimental area
;; (global-set-key (kbd "C--") 'func)
;; (message "this-event: %s this-command: %s" last-input-event this-command)
;; (message "initial: %s point: %s" initial-point (point))

;;; jidaikobo.init.el ends here
