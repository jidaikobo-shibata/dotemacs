;;; jidaikobo.init.el --- jidaikobo.init.el for jidaikobo
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; ------------------------------------------------------------
;;; Commentary:
;;; usage: emacsのインストール（要X-code Command Line Tools）
;;; thx http://masutaka.net/chalow/2015-04-12-1.html
;;; ftp://ftp.math.s.chiba-u.ac.jp/emacsを確認して、あたらしいパッチの存在を確認すると良い
;; curl -LO http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.xz
;; curl -LO ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-24.5-mac-5.15.tar.gz
;; tar xfJ emacs-24.5.tar.xz
;; tar xfz emacs-24.5-mac-5.15.tar.gz
;; cd emacs-24.5
;; patch -p 1 < ../emacs-24.5-mac-5.15/patch-mac
;; cp -r ../emacs-24.5-mac-5.15/mac mac
;; cp ../emacs-24.5-mac-5.15/src/* src
;; cp ../emacs-24.5-mac-5.15/lisp/term/mac-win.el lisp/term
;; \cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
;; ./configure --prefix=$HOME/opt/emacs-24.5 --with-mac --without-x
;; make
;; make GZIP_PROG='' install
;; cp -r mac/Emacs.app /Applications

;;; ------------------------------------------------------------
;;; Usage: 利用前の準備
;;; このjidaikobo.init.elを~/.emacs.dに入れる前に、以下手順を踏んでおくこと。
;; @ terminal
;; sudo port install global
;; emacs --batch -l ~/.emacs.d/jdiaikobo/jidaikobo.init.el

;;; Code:
;;; ------------------------------------------------------------
;;; package類のロード等
;; how to update package
;; M-x package-list-packages RET U x

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

;; emacsclient（使いたいのだけど、今のところうまくいかない）
(if (eq window-system 'ns) (server-start))

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

;; is-once-in-a-day
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
			js2-mode
			mic-paren
			gtags
			magit
			gist
			tabbar
			foreign-regexp
			rainbow-mode
			popwin
			elscreen))

	;; my-packagesからインストールしていないパッケージをインストール
	(dolist (package my-packages)
		(unless (package-installed-p package)
			(package-install package))))

;;; ------------------------------------------------------------
;;; jidaikobo's elisp.

;;; 検索センター
;;; search-center
(custom-set-variables
'(sc/is-use-super t)
'(sc/split-direction "vertical"))
(require 'search-center)
(search-center-mode t)

;;; HTMLのマークアップのキーバインド集
;;; web-authoring-set
(require 'web-authoring-set)

;;; ------------------------------------------------------------
;;; theme

(add-to-list 'custom-theme-load-path
						 (file-name-as-directory (concat jidaikobo-dir "themes/")))
(load-theme 'jidaikobo-dark t)

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
(global-set-key (kbd "<C-up>") 'backward-paragraph) ; Control-down
(global-set-key (kbd "<C-down>") 'forward-paragraph) ; Control-down
(global-set-key (kbd "<backspace>") 'delete-backward-char) ; delete
(global-set-key (kbd "<backtab>") 'indent-for-tab-command)
(global-set-key (kbd "<C-tab>") 'indent-for-tab-command)
;; (global-set-key (kbd "M-right") 'forward-symbol)
;; (global-set-key (kbd "M-left") (lambda () (interactive) (forward-symbol -1)))

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

;; ウィンドウ切り替え (opt+tab)
(global-set-key (kbd "<M-tab>") 'other-window)

;; M-g or cmd+opt+j で指定行へジャンプ
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s-j") 'goto-line)

;;; ------------------------------------------------------------
;;; 自分好みのタブの振る舞い
;;; ewww/read-onlyバッファではリンクの移動
;;; ミニバッファだったらミニバッファ補完
;;; 直前の動作が改行か選択範囲があったらインデント
;;; あとは\tを挿入

(defun my-tab-dwim ()
	"Insert tab or jump to link."
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
	 ;; 選択範囲があるか、直前がエンターだったらインデント
	 ((or (memq last-command '(newline))
				mark-active)
		(indent-for-tab-command))
	 ;; タブを挿入
	 (t
		(insert "\t"))))
(global-set-key (kbd "<tab>") 'my-tab-dwim)

;;; ------------------------------------------------------------
;;; 文字入力

;; opt+¥でバックスラッシュを入力
(global-set-key (kbd "M-¥") "\\")

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
;;; 少々大袈裟だけれど、括弧同士のハイライトがカーソルの邪魔なのでアンダーラインにする
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
	;; (message "this-event:  %s\nthis-command:%s" last-input-event this-command)
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
		(when (and (region-active-p)
							 (not (memq last-input-event '(S-left S-right S-down S-up))))
			(cond
			 ((memq last-input-event '(right down))
				(goto-char (region-end)))
			 ((memq this-command '(left-char previous-line))
				(goto-char (region-beginning))))
			(deactivate-mark))))

;;; ------------------------------------------------------------
;;; popwin
;; thx http://d.hatena.ne.jp/m2ym/20110228/1298868721
;; thx http://valvallow.blogspot.jp/2011/03/emacs-popwinel.html

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; key-binds
(global-set-key (kbd "M-p p") 'popwin:display-last-buffer)
(global-set-key (kbd "M-p m") 'popwin:messages)
;; popwin:popup-buffer-tail

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
	'(("C-t"  . 'mc/mark-next-like-this)
		("n"    . 'mc/mark-next-like-this)
		("p"    . 'mc/mark-previous-like-this)
		("m"    . 'mc/mark-more-like-this-extended)
		("u"    . 'mc/unmark-next-like-this)
		("U"    . 'mc/unmark-previous-like-this)
		("s"    . 'mc/skip-to-next-like-this)
		("S"    . 'mc/skip-to-previous-like-this)
		("*"    . 'mc/mark-all-like-this)
		("d"    . 'mc/mark-all-like-this-dwim)
		("i"    . 'mc/insert-numbers)
		("o"    . 'mc/sort-regions)
		("O"    . 'mc/reverse-regions)))

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
;;; 最近開いたファイルの履歴

(require 'recentf-ext)
(recentf-mode 1)
(setq recentf-exclude '("/TAGS$"
												"/var/tmp/"
												".recentf"
												"^/[^/:]+:" ; TRAMP
												".+Fetch Temporary Folder.+"))
(setq recentf-max-saved-items 100)

;;; ------------------------------------------------------------
;;; Anything関連

;; Anything
(require 'anything)
(require 'anything-config)

(setq alist-anything-for-files
			'((anything-c-source-emacs-commands
				 anything-c-source-gtags-select)
				;; anything-c-source-files-in-current-dir+
				;; anything-c-source-buffers-list ;; *のバッファでAnythingを止めることがある
				anything-c-source-find-by-gtags
				anything-c-source-bookmarks
				anything-c-source-recentf))

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
													(concat "find " (directory-file-name (gtags-get-rootpath)) find-opt)) "\n"))
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
		(insert (shell-command-to-string (concat "find " my-fetch-app-dir " -name \"*_NON_*\" -prune -o -name \"*.app\"")))
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
		(insert (shell-command-to-string (concat "find " my-work-dir " -maxdepth 1 -type d")) "\n")
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
;;; thx https://github.com/skkzsh/.emacs.d/blob/master/conf/anything-init.el
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
;;; thx http://d.hatena.ne.jp/buzztaiki/20081115/1226760184
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
											 ((char-equal ?\  (aref (buffer-name b) 0)) nil)
											 ;; *scratch*バッファは表示する
											 ((equal "*scratch*" (buffer-name b)) b)
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
	(global-set-key (kbd "M-s-t") 'move-current-tab-to-top))

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
		(kill-buffer)
		;; screenが複数だったらelscreen-kill
		(unless (and is-use-elscreen (elscreen-one-screen-p)) (elscreen-kill)))
	 ;; screenがひとつだったらkill-buffer
	 ((and is-use-elscreen (elscreen-one-screen-p)) (kill-buffer))
	 ;; とりあえずkill
	 (is-use-elscreen (elscreen-kill-screen-and-buffers))
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
(add-hook 'js2-mode-hook 'show-line-number)
(add-hook 'text-mode-hook 'show-line-number)
(add-hook 'fundamental-mode-hook 'show-line-number)
(add-hook 'php-mode-hook 'show-line-number)
(add-hook 'web-mode-hook 'show-line-number)
(add-hook 'html-mode-hook 'show-line-number)

;;; ------------------------------------------------------------
;;; モードライン設定

;; 何文字目にいるか表示
(column-number-mode 1)

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
(define-key dired-mode-map (kbd "p") 'my/anything-dired)

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
;;; 行／選択範囲の複製 (cmd+d)

;; duplicate-region-or-line
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
;;; 選択範囲を計算

(defun my-calc ()
	"My-Calc."
	(interactive)
	(let* ((beg (if mark-active (region-beginning)))
				 (end (if mark-active (region-end)))
				 (strings (if mark-active
											(buffer-substring-no-properties beg end)
										(read-string " Expression: " ""))))
		(when mark-active
			(end-of-line)
			(newline))
		(insert (calc-eval strings))))
(global-set-key (kbd "M-c") 'my-calc)

;;; ------------------------------------------------------------
;;; 選択範囲を[大文字|小文字|キャピタライズ]に

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "s-U") 'upcase-region)
(global-set-key (kbd "s-L") 'downcase-region)
(global-set-key (kbd "s-C") 'capitalize-region)

;;; ------------------------------------------------------------
;;; 全角英数字を半角英数字に、半角カナを全角に、
;;; ucs-normalize-NFC-region で濁点分離を直す
;;; http://d.hatena.ne.jp/nakamura001/20120529/1338305696
;;; http://www.sakito.com/2010/05/mac-os-x-normalization.html

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
(global-set-key (kbd "s-u") 'normalize-chars)

;; 音引、句読点等を除外
;; thx http://d.hatena.ne.jp/khiker/20061014/1160861915
(put-char-code-property ?ー 'ascii nil)
(put-char-code-property ?～ 'ascii nil)
(put-char-code-property ?、 'ascii nil)
(put-char-code-property ?。 'ascii nil)

;; 確実に変換
(put-char-code-property ?， 'jisx0208 ?,)
(put-char-code-property ?． 'jisx0208 ?.)

;;; ------------------------------------------------------------
;;; 選択範囲を1行にする

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

(defvar my-indent-region-beg 0)
(defvar my-indent-region-end 0)
(defun my-indext-region (is-inc)
	"Increase/decrease (mail) indentation.  IS-INC is direction."
	(interactive)
	(let* (search-str
				 replace-str
				 (beg (if (use-region-p) (region-beginning) (point)))
				 (end (if (use-region-p) (region-end) (point)))
				 lines)

		;; dwim
		(goto-char end)
		(goto-char (- (point) 1))
		(end-of-line)
		(setq end (point))

		;; single line
		(when (and (not (use-region-p))
							 (not (memq last-command '(my-inc-region my-dec-region))))
			(beginning-of-line)
			(setq beg (point))
			(end-of-line)
			(setq end (point)))

		;; regenerate region
		(when (and
					 (memq last-command '(my-inc-region my-dec-region)))
			(setq beg my-indent-region-beg
						end my-indent-region-end))

		;; lines
		(setq lines (count-lines beg end))

		;; set search and replace words
		(if is-inc
				;; increase
				(if (eq major-mode 'mail-mode)
						(if (string-match "^>" (buffer-substring-no-properties beg end))
								(setq search-str "^" replace-str ">")
							(setq search-str "^" replace-str "> "
										lines (* lines 2)))
					(setq search-str "^" replace-str "\t"))
			;; decrease
			(if (eq major-mode 'mail-mode)
					(setq search-str "^>" replace-str "")
				(setq search-str "^\t" replace-str "")))

		;; perform-replace
		(perform-replace search-str replace-str nil t nil nil nil beg end)

		(setq end (if is-inc (+ end lines) (- end lines)))

		;; redefine region
		(setq my-indent-region-beg beg
					my-indent-region-end end)))

(defun my-inc-region ()
	"Increase region."
	(interactive)
	(my-indext-region t))

(defun my-dec-region ()
	"Decrease region."
	(interactive)
	(my-indext-region nil))

(global-set-key (kbd "s-}") 'my-inc-region)
(global-set-key (kbd "s-{") 'my-dec-region)

;;; ------------------------------------------------------------
;;; 現在バッファのファイルのフルパスを取得

(defun get-current-path ()
"Get current file path."
(interactive)
(insert (or (buffer-file-name) (expand-file-name default-directory))))
(global-set-key (kbd "M-s-k") 'get-current-path)

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
;;; thx http://qiita.com/ironsand/items/cf8c582da3ec20715677

(require 'rainbow-mode)
(add-hook 'fundamental-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
(add-hook 'lisp-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;;; ------------------------------------------------------------
;;; html-mode

;; html-mode-hook
(add-hook 'html-mode-hook
				'(lambda()
					 (define-key html-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; web-mode

(require 'web-mode)

;; web-mode-hook
(add-hook 'web-mode-hook
				'(lambda()
					 (setq web-mode-markup-indent-offset 2)
					 (define-key web-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; js2-mode

(require 'js2-mode)
(add-hook 'js2-mode-hook '(flycheck-mode t))
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (define-key js2-mode-map (kbd "M-up") 'previous-error))
;; (define-key js2-mode-map (kbd "M-down") 'next-error)

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
					 (define-key php-mode-map "/" 'self-insert-command)))

;;; ------------------------------------------------------------
;;; text-mode

;; テキストモードでもすこしカラーリングする
;; thx http://lioon.net/how-to-customize-face-emacs
;; M-x list-faces-display
(add-hook 'text-mode-hook
				'(lambda()
					 (font-lock-add-keywords nil '(("^■.+" . font-lock-comment-face)))))

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
(setq ac-dwim t)
(setq ac-disable-faces nil)
(setq ac-auto-start t)

;; ユーザ辞書ディレクトリ
(defvar ac-user-dict-dir (concat jidaikobo-dir "ac-dict/"))

;; 辞書追加
;; 英語
(defvar ac-english-cache
(ac-file-dictionary (concat ac-user-dict-dir "english")))
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
												 ac-technical-term-dict
												 ;; ac-source-symbols
												 ac-english-dict))

;;; ------------------------------------------------------------
;;; magit

;; (setq my-emacsclient "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
;; (set-variable 'magit-emacsclient-executable (lambda () (if (file-exists-p my-emacsclient) nil)))
(set-variable 'magit-push-always-verify nil)
(set-variable 'with-editor-show-usage nil)
(global-set-key (kbd "M-s-m") 'magit-status)

;;; ------------------------------------------------------------
;;; gist

(require 'gist)

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
;; 単語境界をもうちょっと細かくしたい
;; 複数の検索置換セット
;; 複数ファイルの検索置換
;; portのEmacsを試してみる？
;; なるべく余計なことをしないphpモード。シンタックステーブルだけ持ってきて、用語とタブキーの振る舞いは自分で設定する
;; auto-completeの技術語辞書をもうちょっと厳選
;; auto-completeはハイフンがあっても機能して欲しい（けど、シンタックステーブルか？）
;; curchg-input-method-cursor-colorの挙動を確認
;; デフォルトのinput methodを確認して、keyboard masetroとの合わせ技でIMをいじる。

;;; ------------------------------------------------------------
;;; experimental area

;;; ------------------------------------------------------------
;; 単語境界を細かく。どうもシンタックステーブルの問題らしい。
;; 文字カテゴリの作成
;; http://smallsteps.seesaa.net/article/123661899.html
;; (define-category ?U "Upper case")
;; (define-category ?L "Lower case")
;; ;; 文字の登録。とりあえずはAからZまでの英字のみ。
;; (modify-category-entry (cons ?A ?Z) ?U)
;; (modify-category-entry (cons ?a ?z) ?L)
;; 小文字に大文字が続く場合を単語境界とする。
;; (add-to-list 'word-separating-categories (cons ?L ?U))

;;; ------------------------------------------------------------
;; my-anything-c-source-buffers-list
;; (defvar my-anything-c-source-buffers-list
;; 	`((name . "Buffers")
;; 		(candidates . (lambda ()
;; 										(buffer-list)))))
;; my-anything-c-source-buffers-list
;; (buffer-list)
;; anything-c-source-buffers-list
;; (anything-c-buffer-list)
;; (anything-c-highlight-buffers)

;; http://d.hatena.ne.jp/tomoya/20101213/1292166026
;; (defun create-hyper-link-at-point-url ()
;;   "カーソル位置のURLを HTML でマークアップする"
;;   (interactive)
;;   (let* ((bounds (bounds-of-thing-at-point 'url))
;;          (start (car bounds))
;;          (end (cdr bounds))
;;          (link (format "<a href=\"%s\"></a>" (thing-at-point 'url))))
;;     (delete-region start end)
;;     (insert link)))

;;; ------------------------------------------------------------
;;; よくつかう
;; (message "%s" (concat (format "%s" last-input-event) " - " (format "%s" last-command)))
;; (message "%s" (concat (format "%s" initial-point) "-" (format "%s" (point))))

;;; jidaikobo.init.el ends here
