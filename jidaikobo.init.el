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
;; curl -LO ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-24.5-mac-5.11.tar.gz
;; tar xfJ emacs-24.5.tar.xz
;; tar xfz emacs-24.5-mac-5.11.tar.gz
;; cd emacs-24.5
;; patch -p 1 < ../emacs-24.5-mac-5.11/patch-mac
;; cp -r ../emacs-24.5-mac-5.11/mac mac
;; cp ../emacs-24.5-mac-5.11/src/* src
;; cp ../emacs-24.5-mac-5.11/lisp/term/mac-win.el lisp/term
;; \cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
;; ./configure --prefix=$HOME/opt/emacs-24.5 --with-mac --without-x
;; make
;; make GZIP_PROG='' install
;; cp -r mac/Emacs.app ~/Applications

;;; ------------------------------------------------------------
;;; Usage: 利用前の準備
;;; このjidaikobo.init.elを~/.emacs.dに入れる前に、以下手順を踏んでおくこと。
;; @ terminal
;; sudo port install global
;; mkdir -p ~/.emacs.d/elisp
;; emacs --batch -l ~/.emacs.d/jidaikobo.init.el

;;; Code:
;;; ------------------------------------------------------------
;;; package類のロード等
;; how to update package
;; M-x package-list-packages RET U x

;; is-once-in-a-day
(defun is-once-in-a-day ()
	"Check is once in a day depend on a recentf."
	(interactive)
	(let ((ftime (float-time
								(time-subtract
								 ;; 今日の0時
								 (encode-time 0 0 0 (nth 3 (decode-time)) (nth 4 (decode-time)) (nth 5 (decode-time)) (nth 6 (decode-time)) (nth 7 (decode-time)) (nth 8 (decode-time)))
								 ;; recentfファイルの最終更新時
								 (nth 5 (file-attributes "~/.emacs.d/recentf"))))))
		(if (> ftime 0) t nil)))

;; load-pathの追加
(add-to-list 'load-path "~/.emacs.d/jidaikobo")

;;; load packages
(if (file-exists-p "~/.emacs.d/package.override.el")
		(load "~/.emacs.d/package.override.el")

	;; Packages
	(require 'package)
	(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
	;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
	;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
	(package-initialize)

	;; 1日一回package-refresh-contentsする
	(when (is-once-in-a-day) (package-refresh-contents))

	;; my-packages
	(defvar my-packages
		'(anything
			descbinds-anything
			auto-complete
			undohist
			undo-tree
			point-undo
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
			bind-key
			magit
			gist
			elscreen))

	;; my-packagesからインストールしていないパッケージをインストール
	(dolist (package my-packages)
		(unless (package-installed-p package)
			(package-install package))))

;;; ------------------------------------------------------------
;;; jidaikobo's elisp.

;;; 編集可能な検索置換仕組み
;;; editable-search
(custom-set-variables
 '(es-is-use-super t))
(require 'editable-search)

;;; HTMLのマークアップのキーバインド集
;;; web-authoring-set
(require 'web-authoring-set)

;;; ------------------------------------------------------------
;;; キーバインド用

(require 'bind-key)

;;; ------------------------------------------------------------
;;; 全体設定

;;; リージョンを上書きできるようにする
(delete-selection-mode t)

;;; 選択範囲を可視化
(setq transient-mark-mode t)

;;; 自動分割は原則左右で
(setq split-height-threshold nil)

;;; inline patchを有効に
(setq default-input-method "MacOSX")

;;; optキーをMetaキーに
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; yes/noをy/nへ
(fset 'yes-or-no-p 'y-or-n-p)

;;; 起動画面を抑止
(setq inhibit-startup-message t)

;;; スクラッチメッセージ（起動時注意書き）を抑止
(setq initial-scratch-message nil)

;;; オートインデント無効
(electric-indent-mode -1)

;;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;;; 自動保存を無効
(setq auto-save-default nil)
(setq delete-auto-save-files t)

;; ファイルが #! から始まる場合、+xを付けて保存する
(add-hook 'after-save-hook
					'executable-make-buffer-file-executable-if-script-p)

;;; ツールバーを非表示
(tool-bar-mode -1)

;;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f %%* Emacs@%s" (system-name)))

;;; ミニバッファ履歴を保存
(savehist-mode 1)

;;; タブ幅
(setq-default tab-width 2)
(setq-default indent-tabs-mode t)

;;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq-default cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;;複数フレームを開かないようにする
(setq-default ns-pop-up-frames nil)

;;; emacsclient（使いたいのだけど、今のところうまくいかない）
(if (eq window-system 'ns) (server-start))

;;; ------------------------------------------------------------
;;; キーボード操作

;;; mac likeなcmd関係
;; thx http://d.hatena.ne.jp/gan2/20080109/1199887209
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(bind-key* "s-a" 'mark-whole-buffer) ; select all (cmd+a)
(bind-key* "s-c" 'kill-ring-save) ; copy (cmd+c)
(bind-key* "s-x" 'kill-region) ; cut (cmd+x)
(bind-key* "s-v" 'yank) ; paste (cmd+v)
(bind-key* "s-s" 'save-buffer) ; save (cmd+s)
(bind-key* "s-S" 'write-file) ; save as (cmd+shift+s)
(bind-key* "s-o" 'find-file) ; open (cmd+o)
(bind-key* "s-z" 'undo-tree-undo) ; undo (cmd+z)
(bind-key* "s-Z" 'undo-tree-redo) ; redo (cmd+shift+z)
(bind-key* "s-+" 'text-scale-increase) ; resize increase (cmd++)
(bind-key* "<s-kp-add>" 'text-scale-increase) ; resize increase (cmd++)
(bind-key* "s--" 'text-scale-decrease) ; resize decrease (cmd+-)
(bind-key* "<s-kp-subtract>" 'text-scale-decrease) ; resize decrease (cmd+-)
;; (bind-key* "s-kp-equal" (text-scale-mode 0))
;; (bind-key* "s-kp-equal" (text-scale-mode 0))
;; (bind-key* "s-=" (text-scale-mode 0))
;; (bind-key* "s-kp-0" (text-scale-mode 0))
;; (bind-key* "s-0" (text-scale-mode 0))
(bind-key* "s-q" 'save-buffers-kill-emacs) ; quit (cmd+q)
(bind-key* "<s-up>" 'beginning-of-buffer) ; cmd+up
(bind-key* "<s-down>" 'end-of-buffer) ; cmd+down
(bind-key* "<s-left>" 'beginning-of-line) ; cmd+left
(bind-key* "<s-right>" 'end-of-line) ; cmd+right
(bind-key* "<C-up>" 'backward-paragraph) ; Control-down
(bind-key* "<C-down>" 'forward-paragraph) ; Control-down
;; (bind-key* "M-right" 'forward-symbol)
;; (bind-key* "M-left" (lambda () (interactive) (forward-symbol -1)))

;;; escでM-g
;; http://emacswiki.org/emacs/CancelingInEmacs
(setq-default normal-escape-enabled t)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(bind-key* "<escape>" 'keyboard-quit) ; everywhere else
(bind-key* "M-ESC ESC" 'keyboard-quit)
(define-key minibuffer-inactive-mode-map [escape] 'keyboard-quit) ; minibuffer

;;; opt+¥でバックスラッシュを入力
(bind-key* "M-¥" "\\")

;;; ウィンドウ切り替え (opt+tab)
(bind-key* "<M-tab>" 'other-window)

;;; M-g or cmd+opt+j で指定行へジャンプ
(bind-key* "M-g" 'goto-line)
(bind-key* "M-s-j" 'goto-line)

;;; smart-tab
;; コンテキストに応じたtabキー。auto-completeと共存
(require 'smart-tab)
(global-smart-tab-mode)

;;; ------------------------------------------------------------
;;; control+shift+cursorでウィンドウ内バッファ履歴
;; thx http://pc12.2ch.net/test/read.cgi/unix/1261307488/

(bind-key* "<C-S-left>" 'my-switch-to-prev-buffer)
(bind-key* "<C-S-right>" 'my-switch-to-next-buffer)

(defun my-switch-to-prev-buffer ()
	"Previous working buffer history."
	(interactive)
	(let ((blist (buffer-list))
				(prev-buffer)
				(buffer))
		(while blist
			(unless (or (string= (substring (buffer-name (car blist)) 0 1) " ")
							(string= (substring (buffer-name (car blist)) 0 1) "*")
							(string= (substring (buffer-name (car blist)) 0 1) "+"))
				(setq prev-buffer (car blist)))
			(setq blist (cdr blist))
			(setq buffer (car blist))
			(if (eq (current-buffer) buffer)
					(progn (switch-to-buffer prev-buffer t)
								 (setq blist nil))))))

(defun my-switch-to-next-buffer ()
	"Next working buffer history."
	(interactive)
	(let ((blist (buffer-list))
				(buffer))
		(while blist
			(setq buffer (car blist))
			(setq blist (cdr blist))
			(if (eq (current-buffer) buffer)
					(progn (while (and blist (or (string= (substring (buffer-name (car blist)) 0 1) " ")
																			 (string= (substring (buffer-name (car blist)) 0 1) "*")
																			 (string= (substring (buffer-name (car blist)) 0 1) "+")))
									 (setq blist (cdr blist)))
								 (switch-to-buffer (car blist) t)
								 (setq blist nil))))))

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
			(setq target "^\t*<h2"))
		 (t
			(setq target "^;;; ----+$\\|^■\\|^///")))
		(if (string= direction "prev")
				(re-search-backward target)
			(re-search-forward target))))
(bind-key* "<M-s-down>" (lambda () (interactive) (next-block "next")))
(bind-key* "<M-s-up>" (lambda () (interactive) (next-block "prev")))

;;; ------------------------------------------------------------
;;; 一行目と最終行での上下キーの振る舞い（行末と行頭へ）

(defvar prev-line-num (line-number-at-pos))
(add-hook 'post-command-hook 'es-goto-the-edge)
(defun es-goto-the-edge ()
	"Go to the edge of the line."
	;; (message "this-event:  %s\nthis-command:%s" last-input-event this-command)
	(when (and (eq prev-line-num 1) (memq last-input-event '(up S-up)))
		(beginning-of-line))
	(when (and (eq prev-line-num (count-lines 1 (point-max)))
						 (memq last-input-event '(down S-down)))
		(end-of-line))
	(setq prev-line-num (line-number-at-pos)))

;;; ------------------------------------------------------------
;;; 複数箇所選択と編集

;;; multiple-cursors and smartrep
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(bind-key* "C-M-c" 'mc/edit-lines)
(bind-key* "C-M-r" 'mc/mark-all-in-region)

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

;;; undohist
;; ファイルを閉じてもundoの履歴を残す
(require 'undohist)
(undohist-initialize)

;;; undo-tree
;; redo (cmd+shft+z)
(require 'undo-tree)
(global-undo-tree-mode t)

;;; point-undo
;; カーソル位置履歴 (undo: M-s-left, redo: M-s-right)
(require 'point-undo)
(bind-key* "<S-s-left>" 'point-undo)
(bind-key* "<S-s-right>" 'point-redo)

;; undo in regionしない
(defadvice undo-tree-undo (before deactivate-region activate)
	"Deactivate Region when attempt to undo."
	(deactivate-mark))

;;; ------------------------------------------------------------
;;; recentf
;; 最近開いたファイルの履歴

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

;;; Anything
(require 'anything)
(require 'anything-config)
(require 'anything-gtags)
(require 'anything-grep)

;;; あればgtagsを起点にしてfindし、なければカレントディレクトリを対象にした情報源
(defvar anything-c-source-find-by-gtags
	'((name . "Find by gtags or ls")
		(candidates . (lambda ()
										(let
												((default-directory
													 (with-current-buffer anything-current-buffer default-directory))
												 (find-opt " -type f ! -name \"*.png\" ! -name \"*.ico\" ! -name \"*.gif\" ! -name \"*.jpg\""))
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

;;; FTP by Fetch
(defvar anything-c-source-my-fetch
	'((name . "open Fetch.app")
		(candidates . (lambda ()
										(with-temp-buffer
											(insert (shell-command-to-string "find /Users/jidaikobo/FTP -name \"*_NON_*\" -prune -o -name \"*.app\""))
											(ucs-normalize-NFC-region (point-min) (point-max))
											(split-string (buffer-string) "\n"))))
		(type . file)
		(action . (("open Fetch" . anything-fetch-open)))))

(defun anything-fetch-open (app)
	"Fetch open.  APP is path."
	(shell-command (concat "open " app)))

;;; 接続先Hostを書いた情報源を探して、tramp接続
(defvar anything-c-source-my-hosts
	'((name . "hosts")
		(candidates . (lambda ()
										(split-string
										 (with-temp-buffer
											 (insert-file-contents "~/.emacs.d/anything/hosts.txt")
											 (buffer-string)))))
										;; (split-string (shell-command-to-string "find /Users/jidaikobo/FTP/ -name \"*_NON*\" -prune -o -type f -name \"destination.txt\""))))
		(type . file)
		(action . (("Tramp" . anything-tramp-open)))))

(defun anything-tramp-open (path)
	"Tramp open.  PATH is path."
	(find-file path))

;; (defun anything-tramp-close (path)
;; 	"Tramp close.  PATH is path."
;; 	(find-file path))

;;; よく使うプロジェクトに対する操作
(defvar anything-c-source-cd-to-projects
	'((name . "cd to projects")
		(candidates . (lambda () (split-string (shell-command-to-string "find ~/Sites -maxdepth 1 -type d") "\n")))
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

;; 編集対象でないバッファを除外(必要な場合、switch-to-buffer)
;; thx https://github.com/skkzsh/.emacs.d/blob/master/conf/anything-init.el
(setq anything-c-boring-buffer-regexp
			(rx "*" (+ not-newline) "*"))

;;; my-anything-for-files
(defun my-anything-for-files ()
	"Anything command included find by gtags."
	(interactive)
	(anything-other-buffer
	 '((anything-c-source-emacs-commands
			anything-c-source-gtags-select)
		 ;; anything-c-source-files-in-current-dir+
		 anything-c-source-buffers-list
		 anything-c-source-find-by-gtags
		 ;; anything-c-source-my-hosts
		 anything-c-source-cd-to-projects
		 anything-c-source-bookmarks
		 anything-c-source-recentf
		 anything-c-source-my-fetch)
	 "*my-anything-for-files*"))
(bind-key* "C-;" (lambda ()
									 (interactive)
									 (when (< (frame-width) 110)
										 (set-frame-size (selected-frame) (+ (frame-width) 100) (frame-height)))
									 (my-anything-for-files)))

;;; my-anything-for-functions
(defun my-anything-for-functions ()
	"Anything command for program."
	(interactive)
	(anything-other-buffer
	 '(anything-c-source-imenu
		 anything-c-source-emacs-commands
		 anything-c-source-emacs-functions)
	 "*my-anything-for-functions*"))
(bind-key* "C-," (lambda ()
									 (interactive)
									 (when (< (frame-width) 110)
										 (set-frame-size (selected-frame) (+ (frame-width) 100) (frame-height)))
									 (my-anything-for-functions)))

;;; descbinds-anythingの乗っ取り
;; thx http://d.hatena.ne.jp/buzztaiki/20081115/1226760184
(require 'descbinds-anything)
(descbinds-anything-install)
(bind-key* "C-." (lambda ()
									 (interactive)
									 (when (< (frame-width) 110)
										 (set-frame-size (selected-frame) (+ (frame-width) 100) (frame-height)))
									 (descbinds-anything)))

;;; ------------------------------------------------------------
;;; gtags

(require 'gtags)
(setq gtags-path-style 'relative)

;;; キーバインド
(setq-default gtags-mode-hook
			'(lambda ()
				 (local-set-key "\M-t" 'gtags-find-tag)
				 (local-set-key "\M-r" 'gtags-find-rtag)
				 (local-set-key "\M-s" 'gtags-find-symbol)
				 (local-set-key "\M-T" 'gtags-pop-stack)))

;;; gtags-mode を使いたい mode の hook に追加する
(add-hook 'php-mode-hook
					'(lambda()
						 (gtags-mode 1)
						 (gtags-make-complete-list)))

;;; update GTAGS
;; thx http://qiita.com/yewton/items/d9e686d2f2a092321e34
(defun update-gtags (&optional prefix)
	"Update gtags.  PREFIX."
	(interactive "P")
	(let ((rootdir (gtags-get-rootpath))
				(args (if prefix "-v" "-iv")))
		(when rootdir
			(let* ((default-directory rootdir)
						 (buffer (get-buffer-create "*update GTAGS*")))
				(with-current-buffer
					(erase-buffer)
					(let ((result (process-file "gtags" nil buffer nil args)))
						(if (= 0 result)
								(message "GTAGS successfully updated.")
							(message "update GTAGS error with exit status %d" result))))))))
(add-hook 'after-save-hook 'update-gtags)

;;; ------------------------------------------------------------
;;; タブ関連 - elscreen

(require 'elscreen)
(elscreen-start)

;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)

;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)

;;; キーバインド
(bind-key* "<M-s-right>" 'elscreen-next)
(bind-key* "<M-s-left>" 'elscreen-previous)

;;; 新しいスクリーン
(bind-key* "s-t" (lambda () (interactive)
									 (elscreen-create)
									 ;; (switch-to-buffer (generate-new-buffer "new"))
									 (switch-to-buffer "*scratch*")
									 (text-mode)))

;;; ウィンドウ/スクリーンを閉じる
(defun my-delete-windows ()
	"Contexual delete windows."
	(interactive)
	(cond
	 ;; ウィンドウ構成が多ければまず他のウィンドウを消す
	 ((not (one-window-p)) (delete-other-windows))

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
		(unless (elscreen-one-screen-p) (elscreen-kill)))

	 ;; screenがひとつだったらkill-buffer
	 ((elscreen-one-screen-p) (kill-buffer))

	 ;; ここまで来る条件てあるのかしら。とりあえずkill
	 (t (elscreen-kill-screen-and-buffers))))
(bind-key* "s-w" 'my-delete-windows)

;;; ------------------------------------------------------------
;;; カーソル関連

;;; cursor-chg
;; カーソルの形状を変更（ブロックカーソルが苦手なので）
(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 0)
(setq curchg-default-cursor-color "White")
;; 残念ながら日本語キーボードで「英数」「かな」キーを使っている限りは、これは効かない。
(setq curchg-input-method-cursor-color "Orange")

;;; C-aで、開始場所と先頭をトグル
;; thx http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
(defun my-goto-line-beginning-or-indent (&optional position)
	"Goto line beginning or indent.  POSITION is optical."
	(interactive)
	(or position (setq position (point)))
	(let (($starting-position (progn (back-to-indentation) (point))))
		(if (eq $starting-position position)
				(move-beginning-of-line 1))))
(bind-key* "C-a" 'my-goto-line-beginning-or-indent)

;;; ------------------------------------------------------------
;;; 行設定

;;; 行カーソル
;; thx http://rubikitch.com/tag/emacs-post-command-hook-timer/
;; (setq hl-line-face 'underline)
;; (global-hl-line-mode)
;; 下線だと、日本語入力時の候補領域がわかりづらいのでやめる。
(require 'hl-line)
(defun global-hl-line-timer-function ()
	"Line cursor."
	(global-hl-line-unhighlight-all)
	(let ((global-hl-line-mode t))
		(set-face-attribute 'hl-line nil :foreground nil :background "#3e3e3e" :underline nil)
		(global-hl-line-highlight)))
(setq-default global-hl-line-timer
			(run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; 行間隔を少し広げる
(set-default 'line-spacing 3)

;;; 行番号を表示する
;; 表示切替はM-x wb-line-number-toggleと入力。
(defun show-line-number ()
	"Show line number."
	(interactive)
	(require 'linum)
	(setq-default linum-delay t)
	(defadvice linum-schedule (around my-linum-schedule () activate)
		(run-with-idle-timer 0.2 nil #'linum-update-current))
	(global-linum-mode t)
	(setq-default linum-format "%5d: "))
(show-line-number)

;;; ------------------------------------------------------------
;;; モードライン設定

;;; 何文字目にいるか表示
(column-number-mode 1)

;;; ------------------------------------------------------------
;;; マウス設定

;;; shift+clickでregion作成
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

(bind-key* "<s-return>" (lambda () (interactive)
														 (if (region-active-p)
																 (eval-region (region-beginning) (region-end))
															 (eval-buffer))
														 (message "eval done.")))

;;; ------------------------------------------------------------
;;; 釣り合いのとれる括弧のハイライト

;; 少々大袈裟だけれど、括弧同士のハイライトがカーソルの邪魔なのでアンダーラインにする
(require 'mic-paren)
(paren-activate)
(setq-default paren-face '(underline paren-match-face))
(setq paren-match-face '(underline paren-face))
(setq paren-sexp-mode t)

;;; ------------------------------------------------------------
;;; ファイラ (dired)

;; diredでファイル編集（rで編集モードに）
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;; C-x C-f で現在位置を開く
(ffap-bindings)

;;; diredでマークをつけたファイルを開く（F）
(eval-after-load "dired"
	'(progn
		 (define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
		 (defun my-dired-find-marked-files (&optional arg)
			 "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
			 (interactive "P")
			 (let* ((fn-list (dired-get-marked-files nil arg)))
				 (mapc 'find-file fn-list)))))

;;; diredでタブを開きすぎないようにする
;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; RET 標準の dired-find-file では dired バッファが複数作られるのでdired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;;; anything in dired
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

;;; タブの可視化
;;; thx http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(require 'whitespace)
(setq whitespace-style '(face          ; faceで可視化
												trailing       ; 行末
												tabs           ; タブ
												spaces         ; スペース
												empty          ; 先頭/末尾の空行
												space-mark     ; 表示のマッピング
												tab-mark
												))
(setq whitespace-display-mappings
			'((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
				;;(space-mark ?\u3000 [?\u25a1])
				))

;;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

;;; whitespaceの見栄え
(global-whitespace-mode 1)

(set-face-attribute 'whitespace-trailing nil
										:background "#201f1f"
										:underline t)
(set-face-attribute 'whitespace-tab nil
										:background "#201f1f"
										:foreground "Gray30"
										:underline nil)
(set-face-attribute 'whitespace-space nil
										:background "#201f1f"
										:underline nil)
(set-face-attribute 'whitespace-empty nil
										:background "#201f1f"
										:underline nil)

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
(bind-key* "s-d" 'duplicate-region-or-line)

;;; ------------------------------------------------------------
;;; 選択範囲を[大文字|小文字|キャピタライズ]に

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "s-U") 'upcase-region)
(global-set-key (kbd "s-L") 'downcase-region)
(global-set-key (kbd "s-C") 'capitalize-region)

;;; ------------------------------------------------------------
;;; 全角数字を半角数字に

(defun convert-to-single-byte-number ()
	"Convert multi-byte numbers in region into single-byte number.  replace-strings-in-region-by-list is depend on jidaikobo-web-authoring-set."
	(interactive)
	(replace-strings-in-region-by-list
	 '(("１" . "1")
		 ("２" . "2")
		 ("３" . "3")
		 ("４" . "4")
		 ("５" . "5")
		 ("６" . "6")
		 ("７" . "7")
		 ("８" . "8")
		 ("９" . "9")
		 ("０" . "0"))))
(bind-key* "s-u" 'convert-to-single-byte-number)

;;; ------------------------------------------------------------
;; ucs-normalize-NFC-region で濁点分離を直す
;; http://d.hatena.ne.jp/nakamura001/20120529/1338305696
;; http://www.sakito.com/2010/05/mac-os-x-normalization.html

(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(defun ucs-normalize-NFC-buffer ()
	"Normarize UTF-8."
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
		(ucs-normalize-NFC-region beg end)))
(bind-key* "C-s-u" 'ucs-normalize-NFC-buffer)

;;; ------------------------------------------------------------
;;; 選択範囲を1行にする

(defun join-multi-lines-to-one ()
	"Join multi lines.  es-replace-region is depend on editable-search."
	(interactive)
	(let ((beg (region-beginning))
				(end (region-end)))
		(goto-char beg)
		(back-to-indentation)
		(set-mark-command nil)
		(goto-char end)
		(goto-char (- (point) 1))
		(end-of-line)
		(es-replace-region "\n\\|^>+ \\|\t" "" t)))
(bind-key* "<s-kp-divide>" 'join-multi-lines-to-one) ; cmd+/
(bind-key* "s-/" 'join-multi-lines-to-one) ; cmd+/

;;; ------------------------------------------------------------
;;; メール整形

(defun add-mail-quotation ()
	"Add mail quotation.  es-replace-region is depend on editable-search."
	(interactive)
	(mail-mode)
	(if (string-match "^>" (buffer-substring-no-properties (region-beginning) (region-end)))
			(es-replace-region "^" ">" t)
		(es-replace-region "^" "> " t)))
(defun remove-mail-quotation ()
	"Add mail quotation.  es-replace-region is depend on editable-search."
	(interactive)
	(mail-mode)
	(es-replace-region "^>+ +" "" t))
(bind-key* "s-}" 'add-mail-quotation)
(bind-key* "s-{" 'remove-mail-quotation)

;;; ------------------------------------------------------------
;;現在バッファのファイルのフルパスを取得

(defun get-current-path ()
	"Get current file path."
	(interactive)
	(insert (or (buffer-file-name) (expand-file-name default-directory))))
(global-set-key (kbd "M-s-k") 'get-current-path)

;;; ------------------------------------------------------------
;;; 選択範囲の言語を確認して翻訳 (C-c t)

;;; google-translate
;;; http://rubikitch.com/2014/12/07/google-translate/
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
(bind-key* "C-c t" 'google-translate-enja-or-jaen)

;;; ------------------------------------------------------------
;;; flycheck

(load "flycheck")
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'php-mode-hook 'flycheck-mode)
(bind-key* "<M-up>" 'flycheck-previous-error) ; previous error (M+up)
(bind-key* "<M-down>" 'flycheck-next-error) ; next error (M+down)

;;; ------------------------------------------------------------
;;; web-mode

(require 'web-mode)

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

;;; php-modeのタブ幅
(add-hook 'php-mode-hook
					'(lambda()
						 (setq tab-width 2)
						 (setq indent-tabs-mode t)
						 (setq c-basic-offset 2)))

;;; ------------------------------------------------------------
;;; text-mode

;;; テキストモードでもすこしカラーリングする
;; thx http://lioon.net/how-to-customize-face-emacs
;; M-x list-faces-display
(add-hook 'text-mode-hook
					'(lambda()
						 (font-lock-add-keywords nil '(("^■.+" . font-lock-comment-face)))))

;;; ------------------------------------------------------------
;;; kontiki-mode

;;; ワイアフレームモード
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

;;; メールモード（mail-mode）のカラーリング
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
	;; フレームを適当なサイズに変更
	(if (= (frame-width) 200)
			(set-frame-size (selected-frame) 100 55)
		(set-frame-size (selected-frame) 200 55)))
(bind-key* "s-W" 'resize-selected-frame)

;;; ------------------------------------------------------------
;;; auto-complete

;; オートコンプリート
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim t)
(setq ac-disable-faces nil)

;;; ユーザ辞書ディレクトリ
(defvar ac-user-dict-dir (expand-file-name "~/.emacs.d/jidaikobo/ac-dict/"))

;;; 辞書追加
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

;;; 条件の追加
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers
													 ;; ac-source-filename
													 ac-technical-term-dict
													 ;; ac-source-symbols
													 ac-english-dict))

;;; ------------------------------------------------------------
;; magit

;; (setq my-emacsclient "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
;; (set-variable 'magit-emacsclient-executable (lambda () (if (file-exists-p my-emacsclient) nil)))
(set-variable 'magit-push-always-verify nil)
(set-variable 'with-editor-show-usage nil)
(bind-key* "M-s-m" 'magit-status)

;;; ------------------------------------------------------------
;; gist

(require 'gist)










;;; ------------------------------------------------------------
;;; フレーム初期値

(add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
(add-to-list 'default-frame-alist '(font . "ricty-16"))
(add-to-list 'default-frame-alist '(background-color . "#201f1f"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(cursor-color . "gray"))

;;; ------------------------------------------------------------
;;; Todo:
;; doctypeを見てのbrやタグの挿入
;; 単語境界をもうちょっと細かくしたい

;;; ------------------------------------------------------------
;;; experimental area
;; (thing-at-point)

;;; ------------------------------------------------------------
;; 単語境界を細かく
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

;;; jidaikobo.init.el ends here
