;;; editable-search.el --- search by other window buffer
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; 編集窓で試行錯誤して検索置換できるマイナーモード。

;;; Code:

;;; ------------------------------------------------------------
;;; defgroup

(defgroup editable-search nil
	"Provides editable search."
	:group 'Convenience)

(defcustom es-is-use-super nil
	"*Mac-like behavior."
	:group 'editable-search
	:type 'boolean)

(defcustom es-is-next-window-by-tab nil
	"*Mac-like behavior."
	:group 'editable-search
	:type 'boolean)

;;; ------------------------------------------------------------
;;; defvar

(defvar es-target-window (selected-window))
(defvar es-target-buffer (get-buffer (buffer-name)))
(defvar es-search-str-buffer "*search string*")
(defvar es-replace-str-buffer "*replace string*")
(defvar es-previous-searched-str)
(defvar es-previous-replaced-str)
(defvar es-previous-searced-direction nil)
(defvar es-previous-direction)
(defvar es-ignore-delete-window-hook nil)
(defvar editable-search-mode-map (make-keymap))
(defvar editable-re-search-mode-map (make-keymap))
(defvar es-modeline-saved nil)
(defvar es-modeline-background)
(defvar es-modeline-foreground)
(defvar es-re-modeline-background "orange")
(defvar es-re-modeline-foreground "black")
(defvar es-is-foregin-regexp nil)
(defvar es-is-sync-isearch t)
(defvar es-split-direction "horizontal")

;;; ------------------------------------------------------------
;;; dependencies

(declare-function package-installed-p "package")

;; foreign-regexp
(when (package-installed-p 'foreign-regexp)
	(require 'foreign-regexp)
	(eval-after-load "foreign-regexp"
		(progn
			(custom-set-variables
			 '(foreign-regexp/regexp-type 'perl)
			 '(reb-re-syntax 'foreign-regexp))
			(setq es-is-foregin-regexp t)))
	(declare-function foreign-regexp/search/forward  "foreign-regexp")
	(declare-function foreign-regexp/search/backward "foreign-regexp")
	(declare-function foreign-regexp/replace/perform-replace "foreign-regexp"))

;;; ------------------------------------------------------------
;;; minor-mode

;;; 検索置換用のマイナーモード
(define-minor-mode editable-search-mode
	"Provide editable search/replace environment."
	:init-value
	nil
	:lighter
	" Search"
	:keymap
	editable-search-mode-map)

;;; 正規表現のマイナーモード
(define-minor-mode editable-re-search-mode
	"Provide editable regular expression search/replace environment."
	:init-value
	nil
	:lighter
	" re-Search"
	:keymap
	editable-re-search-mode-map
	:after-hook
	(progn
		;; 現在のモードラインを配色を保存しておく
		(unless es-modeline-saved
			(setq es-modeline-background (face-attribute 'mode-line :background)
						es-modeline-foreground (face-attribute 'mode-line :foreground)
						es-modeline-saved t))
		(if (eq editable-re-search-mode nil)
				;; 保存されていたモードラインに戻す
				(set-face-attribute 'mode-line nil
														:foreground es-modeline-foreground
														:background es-modeline-background)
			;; モードラインを変更し、正規表現モードであることを明示する
			(set-face-attribute 'mode-line nil
													:foreground es-re-modeline-foreground
													:background es-re-modeline-background))))

;;; ------------------------------------------------------------
;;; hook

;; isearchコマンドとの同期
(when es-is-sync-isearch
	(add-hook 'isearch-update-post-hook 'es-isearch-update-string)
	(defun es-isearch-update-string ()
		(when (and (not (or isearch-regexp isearch-word))
							 (eq this-command 'isearch-printing-char))
			(unless (get-buffer es-search-str-buffer) (get-buffer-create es-search-str-buffer))
			(es-keep-target-buffer)
			;; 文字列をセット
			(with-current-buffer es-search-str-buffer
				(delete-region (point-min) (point-max))
				(insert isearch-string))
			;; 次回用に文字列を保存
			(setq es-previous-searched-str isearch-string))))

;;; ------------------------------------------------------------
;;; function alias for key-binds

(defun es-alias-search-next ()
	"Alias."
	(interactive)
	(if (eq editable-re-search-mode nil)
			(es-search-replace "next")
		(es-search-replace "re-next")))

(defun es-alias-search-prev ()
	"Alias."
	(interactive)
	(if (eq editable-re-search-mode nil)
			(es-search-replace "prev")
		(es-search-replace "re-prev")))

(defun es-alias-replace-next ()
	"Alias."
	(interactive)
	(if (eq editable-re-search-mode nil)
			(es-search-replace "rep-next")
		(es-search-replace "re-rep-next")))

(defun es-alias-replace-here ()
	"Alias."
	(interactive)
	(if (eq editable-re-search-mode nil)
			(es-search-replace "rep-here")
		(es-search-replace "re-rep-here")))

(defun es-alias-replace-region ()
	"Alias."
	(interactive)
	(if (eq editable-re-search-mode nil)
			(es-replace-all "")
		(es-replace-all "re")))

;;; ------------------------------------------------------------
;;; key-binds - s-key

(when es-is-use-super
	;; global-map
	(define-key global-map (kbd "s-f") 'es-show-windows)
	(define-key global-map (kbd "s-F") 'es-toggle-search-mode)
	(define-key global-map (kbd "s-e") (lambda () (interactive) (es-set-strings "search")))
	(define-key global-map (kbd "s-E") (lambda () (interactive) (es-set-strings "replace")))
	(define-key global-map (kbd "s-g") 'es-alias-search-next)
	(define-key global-map (kbd "s-G") 'es-alias-search-prev)
	(define-key global-map (kbd "s-l") 'es-alias-replace-next)
	(define-key global-map (kbd "s-r") 'es-alias-replace-here)
	(define-key global-map (kbd "s-R") 'es-alias-replace-region)
	(define-key global-map (kbd "s-p") 'es-grep)
	(define-key global-map (kbd "s-h") (lambda () (interactive) (select-window es-target-window)))
	(define-key editable-search-mode-map [escape] 'keyboard-quit))

;;; ------------------------------------------------------------
;;; key-binds - emacs（要検討）

;; set prefix-key to C-F
(define-key global-map (kbd "C-F") nil)
(defvar es-keybind-map (make-sparse-keymap) "Set editable-search keymap.")
(define-key global-map (kbd "C-F") es-keybind-map)

(define-key es-keybind-map (kbd "f") 'es-show-windows)
(define-key es-keybind-map (kbd "F") 'es-toggle-search-mode)
(define-key es-keybind-map (kbd "e") (lambda () (interactive) (es-set-strings "search")))
(define-key es-keybind-map (kbd "E") (lambda () (interactive) (es-set-strings "replace")))
(define-key es-keybind-map (kbd "g") 'es-alias-search-next)
(define-key es-keybind-map (kbd "G") 'es-alias-search-prev)
(define-key es-keybind-map (kbd "l") 'es-alias-replace-next)
(define-key es-keybind-map (kbd "r") 'es-alias-replace-here)
(define-key es-keybind-map (kbd "R") 'es-alias-replace-region)
(define-key es-keybind-map (kbd "p") 'es-grep)
(define-key es-keybind-map (kbd "h") (lambda () (interactive) (select-window es-target-window)))

;;; smartrep
(when (package-installed-p 'smartrep)
		(require 'smartrep)
		(declare-function smartrep-define-key "smartrep")
		(smartrep-define-key global-map "C-F"
			'(("g" . 'es-alias-search-next)
				("G" . 'es-alias-search-prev)
				("l" . 'es-alias-replace-next)
				("r" . 'es-alias-replace-here)
				("R" . 'es-alias-replace-region))))

;;; ------------------------------------------------------------
;;; keep target-buffer

(defun es-keep-target-buffer ()
	"Keep target buffer."
	(unless (or (equal (selected-window) (get-buffer-window es-search-str-buffer))
							(equal (selected-window) (get-buffer-window es-replace-str-buffer)))
		(setq es-target-window (selected-window)
					es-target-buffer (buffer-name))))

;;; ------------------------------------------------------------
;;; toggle re/search mode

(defun es-toggle-search-mode ()
	"Toggle search mode."
	(interactive)
	(es-keep-target-buffer)
	(if (eq editable-re-search-mode nil)
			;; 正規表現モードをオン
			(progn
				(with-current-buffer es-target-buffer
					(editable-re-search-mode t))
				(with-current-buffer es-search-str-buffer
					(editable-re-search-mode t))
				(with-current-buffer es-replace-str-buffer
					(editable-re-search-mode t))
				(message (concat "turned into RE with " es-target-buffer)))
		;; 正規表現モードをオフ
		(with-current-buffer es-target-buffer
			(editable-re-search-mode -1))
		(with-current-buffer es-search-str-buffer
			(editable-re-search-mode -1))
		(with-current-buffer es-replace-str-buffer
			(editable-re-search-mode -1))
		(message (concat "turned off RE with " es-target-buffer))))

;;; ------------------------------------------------------------
;;; 検索・置換文字列をセット

(defun es-set-strings (mode)
	"Set strings to search or replace buffer.  MODE [search|replace]."
	(interactive)
	(let* ((beg (if mark-active (region-beginning)))
				 (end (if mark-active (region-end)))
				 (strings (if mark-active
											(buffer-substring-no-properties beg end)
										(read-string (concat mode " word: ") "")))
				 (target (if (string= mode "search")
										 es-search-str-buffer
									 es-replace-str-buffer)))

		(unless (get-buffer target) (get-buffer-create target))

		(es-keep-target-buffer)

		;; 文字列をセット
		(with-current-buffer target
			(delete-region (point-min) (point-max))
			(insert strings)
			(message (concat "set " mode " strings: " strings)))

		;; 次回用に文字列を保存
		(if (string= mode "search")
				(setq es-previous-searched-str strings)
			(setq es-previous-replaced-str strings))))

;;; ------------------------------------------------------------
;;; 検索・置換文字列を取得

(defun es-get-strings (mode)
	"Get strings from search or replace buffer.  MODE [search|replace]."
	(interactive)
	(let* ((target (if (string= mode "search")
										 es-search-str-buffer
									 es-replace-str-buffer))
				 ret)

		;; strings from buffer
		(with-current-buffer target
			(setq ret (buffer-substring-no-properties (point-min) (point-max))))

		;; ask global
		(if (and (not ret) (string= mode "search"))
			(setq ret (if (boundp 'es-previous-searched-str) es-previous-searched-str nil)))

		;; no search string error
		(when (and (not ret) (string= mode "search"))
				(error "Error: search word is empty"))
		ret))

;;; ------------------------------------------------------------
;;; 検索・置換窓を表示

(defun es-show-windows ()
	"Show search/replace window."
	(interactive)
	(let* ((is-search-window-exist (windowp (get-buffer-window es-search-str-buffer)))
				 (is-replace-window-exist (windowp (get-buffer-window es-replace-str-buffer))))

		(setq es-ignore-delete-window-hook t)
		(es-keep-target-buffer)

		;; どちらかだけ開いていたら、もう片方を閉じる
		(when (and is-search-window-exist (not is-replace-window-exist))
			(delete-window (get-buffer-window es-search-str-buffer)))
		(when (and is-replace-window-exist (not is-search-window-exist))
			(delete-window (get-buffer-window es-replace-str-buffer)))

		;; 上下分割か左右分割を選択できるようにする
		(unless (or is-search-window-exist is-replace-window-exist)
			(if (string= es-split-direction "horizontal")
					(split-window-horizontally)
				(split-window-vertically)))

		;; 検索窓を準備
		(unless is-search-window-exist
			(select-window (next-window))
			(switch-to-buffer es-search-str-buffer)
			(set-window-dedicated-p (selected-window) t) ;変更を許さないウィンドウにする
			(linum-mode -1)
			(editable-search-mode t))

		;; 置換窓を用意
		(unless is-replace-window-exist
			(if (string= es-split-direction "vertical")
					(split-window-horizontally)
				(split-window-vertically))
			(select-window (next-window))
			(switch-to-buffer es-replace-str-buffer)
			(set-window-dedicated-p (selected-window) t) ;変更を許さないウィンドウにする
			(linum-mode -1)
			(editable-search-mode t))

		;; 上下分割ではあまりスペースを取らないように
		(unless is-search-window-exist
			(if (string= es-split-direction "vertical")
					(shrink-window 20)
				;; 左右分割で検索置換窓を出せないくらい狭い時には拡張
				(when (< (frame-width) 110)
					(set-frame-size (selected-frame) (+ (frame-width) 100) (frame-height)))))

		;; キャレットを検索窓にセットして選択
		(select-window (get-buffer-window es-search-str-buffer))
		(mark-whole-buffer)

		(setq es-ignore-delete-window-hook t)))

;;; ------------------------------------------------------------
;;; 共通関数

;; 選択範囲の置換用関数
(defun es-replace-region (search-str replace-str is-re)
	"(string)SEARCH-STR, (string)REPLACE-STR, (bool)IS-RE."
	(when mark-active
		(let ((beg (region-beginning))
					(end (region-end))
					is-replaced)
			(progn
				;; prepare replace string or replace by foreign-regexp
				(when is-re
					(if es-is-foregin-regexp
							;; perform repalce by foreign-regexp
							(progn
								(foreign-regexp/replace/perform-replace
								 search-str replace-str nil nil nil nil nil beg end)
								(setq is-replaced t))
						;; prepare replace string
						(setq replace-str (replace-regexp-in-string
															 search-str
															 replace-str
															 (buffer-substring-no-properties beg end)))))
				;; perform replace (normal and regexp)
				(unless is-replaced
					(delete-region beg end)
					(insert replace-str))))))

;; 選択範囲の作成用関数
;; 検索方向に応じて、キャレットの位置を適切にする
(defun es-generate-region (direction len-search-string)
	"(string)DIRECTION, (int)LEN-SEARCH-STRING."
	(let
			(beg
			 end)
		(progn
		 (cond
			((string= direction "next")
			 (setq beg (- (point) len-search-string))
			 (goto-char beg)
			 (setq end (+ (point) len-search-string))
			 (set-mark (point)) ;; not into mark-ring
			 (goto-char end))
			((string= direction "prev")
			 (setq end (+ (point) len-search-string))
			 (goto-char end)
			 (setq beg (- (point) len-search-string))
			 (set-mark (point))
			 (goto-char beg))) ;; not into mark-ring
		 (setq deactivate-mark nil))))

;; 文字列の取得
(defun es-get-str-from-window (type)
	"Get str from window.  TYPE[search|replace]."
	(interactive)
	(let ((window-obj (if (string= type "search")
												(get-buffer-window es-search-str-buffer)
											(get-buffer-window es-replace-str-buffer)))
				(ret ""))

		;; コンテクストに応じた文字列の取得
		(setq ret (if (windowp window-obj)
									(with-selected-window window-obj (buffer-string))
								nil))

		;; グローバルな変数にも尋ねる
		(if (and (not ret) (string= type "search"))
			(setq ret (if (boundp 'es-previous-searched-str) es-previous-searched-str nil)))

		;; search stringがなければerror
		(when (and (not ret) (string= type "search"))
				(error "Error: search word is empty"))

		;; 戻り値
		ret))

;; 空行の検索を特別扱い
(defun es-replace-empty-line-regex (str)
	"Get str from window.  STR is regex."
	(interactive)
	(if (string= str "^$") "^
" str))

;;; ------------------------------------------------------------
;;; 検索用バッファの文字列で検索・置換する

(declare-function es-move-region "es-move-region" ())
(defun es-search-replace (mode)
	"Do search from other window string.  MODE [next|prev|re-next|re-prev|rep-here|rep-next|rep-prev|re-rep-next|re-rep-prev]."
	(interactive)
	(let* ((search-str "")
				 (replace-str "")
				 (beg 0)
				 (end 0)
				 (len-search-string 0)
				 (direction (substring mode -4))
				 (is-re (if (string= (substring mode 0 3) "re-") t nil))
				 (is-next (if (string= direction "next") t nil))
				 (is-prev (if (string= direction "prev") t nil))
				 (replace-flag (if (>= (length mode) 6) (substring mode 0 6) ""))
				 (is-replace (if (or (string= replace-flag "rep-ne")
														 (string= replace-flag "rep-pr")
														 (string= replace-flag "rep-he")
														 (string= replace-flag "re-rep")) t nil))
				 (is-replace-here (if (or (string= mode "rep-here")
																	(string= mode "re-rep-here")) t nil))
				 target-str)

		;; 検索方向が変わったら向きを変える
		(when (and mark-active (not (string= es-previous-searced-direction direction)))
			(exchange-point-and-mark))
		(setq es-previous-searced-direction direction)

		;; 現在のウィンドウが検索・置換編集用ウィンドウだったら、主たるウィンドウに移動する
		(unless (eq (selected-window) es-target-window)
			(select-window es-target-window))

		;; 検索用文字列の取得（必須）
		(setq search-str (es-get-str-from-window "search"))
		(unless search-str
			(setq search-str (if (boundp 'es-previous-searched-str) es-previous-searched-str nil)))
		(unless search-str (error "Error: search word is empty"))

		;; 空行表現（^$）を特別扱い
		(when is-re (setq search-str (es-replace-empty-line-regex search-str)))

		;; 置換用文字列の取得（置換時必須）
		(setq replace-str (es-get-str-from-window "replace"))
		(unless replace-str
			(setq replace-str (if (boundp 'es-previous-replaced-str) es-previous-replaced-str nil)))
		(if (and is-replace (not replace-str)) (error "Error: replace word is empty"))

		;; 検索文字列にキャレットを移動しリージョンにする関数
		(defun es-move-region ()
			(cond
			 ((and (not is-re) is-next)
				(search-forward search-str))
			 ((and (not is-re) is-prev)
				(search-backward search-str))
			 ((and is-re is-next)
				(if es-is-foregin-regexp
						(foreign-regexp/search/forward search-str)
					(re-search-forward search-str)))
			 ((and is-re is-prev)
				(if es-is-foregin-regexp
						(foreign-regexp/search/backward search-str)
					(re-search-backward search-str))))
			;; ヒットした文字長の取得
			(if is-re
					(setq len-search-string (length (match-string-no-properties 0)))
				(setq len-search-string (length search-str)))
			;; リージョン作成
			(es-generate-region (if is-next "next" "prev") len-search-string))

		;; 処理本体
		(cond
		 ;; rep-nextやrep-prevは、いまの選択範囲を置換してから次に行くようにする
		 ((or (and is-replace is-next) (and is-replace is-prev))
			(progn (when mark-active (es-replace-region search-str replace-str is-re))
						 (es-move-region)))
		 ;; その場を置換
		 (is-replace-here (es-replace-region search-str replace-str is-re))
		 ;; 通常はただのキャレット移動
		 (t (es-move-region)))

		;; 今回検索・置換した文字を次回用に保存
		(setq es-previous-searched-str search-str)
		(setq es-previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;;; すべて置換

(defun es-replace-all (mode &optional opt-search-str opt-replace-str)
	"Replace All.  MODE [re].  OPT-SEARCH-STR and OPT-REPLACE-STR are optional."
	(let
			((search-str "")
			 (replace-str "")
			 (beg 1)
			 (end 0)
			 (cnt 0)
			 beg-each
			 end-each
			 (len-search-string 0)
			 (is-re (if (string= mode "re") t nil))
			 target-str
			 type)

		;; 現在のウィンドウが検索・置換編集用ウィンドウだったら、主たるウィンドウに移動する
		(unless (eq (selected-window) es-target-window)
			(select-window es-target-window))

		;; 検索用文字列の取得
		(setq search-str (if opt-search-str
												 opt-search-str
											 (es-get-str-from-window "search")))
		(unless search-str
			(setq search-str (if (boundp 'es-previous-searched-str) es-previous-searched-str nil)))
		(unless search-str (error "Error: search word is empty"))

		;; 空行表現（^$）を特別扱い
		(when is-re (setq search-str (es-replace-empty-line-regex search-str)))

		;; 置換用文字列の取得

		(setq replace-str (if opt-replace-str
												 opt-replace-str
											 (es-get-str-from-window "replace")))
		(unless replace-str
			(setq replace-str (if (boundp 'es-previous-replaced-str) es-previous-replaced-str nil)))
		(unless replace-str (error "Error: replace word is empty"))

		;; 選択範囲があればそこを対象とする、なければ、すべてを対象にして良いか尋ねる
		(if (region-active-p)
				(progn
					(setq beg (region-beginning))
					(setq end (region-end)))
			(progn
				(setq type (read-string "replace whole buffer?(y, n): " nil))
				(if (string= type "y")
						(progn
							(setq beg (point-min))
							(setq end (point-max)))
					(error "Error: no target region"))))

		;; 選択範囲内を置換する
		(ignore-errors
			(save-excursion
				(save-restriction
					(narrow-to-region beg end)
					(cond ((and is-re es-is-foregin-regexp)
								 (foreign-regexp/replace/perform-replace
									search-str replace-str nil nil nil nil nil beg end))
								(is-re
								 (perform-replace
									search-str replace-str nil t nil nil nil beg end))
								(t
								 (perform-replace
									search-str replace-str nil nil nil nil nil beg end))))))

		;; 今回検索・置換した文字を次回用に保存
		(setq es-previous-searched-str search-str)
		(setq es-previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;;; マルチファイル検索
;;; 対象ディレクトリと拡張子を指定したら検索する
;;; ただのrgrepのwrapper
;; thx http://d.hatena.ne.jp/IMAKADO/20090225/1235526604

(require 'grep)
(require 'gtags)
(setq gtags-path-style 'relative)

;; ;; 日本語の自動判別
;; ;; thx http://dev.ariel-networks.com/articles/emacs/part1/
;; (setq grep-host-defaults-alist nil)
;; (when mac-carbon-version-string
;; 	(setq grep-template "lgrep <C> -n <R> <F> <N>")
;; 	(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep <C> -n <R> <N>"))

;; ;; Windows
;; (defvar quote-argument-for-windows-p t "enables `shell-quote-argument' workaround for windows.")
;; (defadvice shell-quote-argument (around shell-quote-argument-for-win activate)
;; 	"workaround for windows."
;; 	(if quote-argument-for-windows-p
;; 			(let ((argument (ad-get-arg 0)))
;; 				(setq argument (replace-regexp-in-string "\\\\" "\\\\" argument nil t))
;; 				(setq argument (replace-regexp-in-string "'" "'\\''" argument nil t))
;; 				(setq ad-return-value (concat "'" argument "'")))
;; 		ad-do-it)
;; 	(setq grep-template "lgrep -Ks -Os <C> -n <R> <F> <N>")
;; 	(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep -Ks -Os <C> -n <R> <N>")
;; thx http://qiita.com/ybiquitous/items/2f2206ff7a557c4cbc11
;; (setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
;;       grep-program "\"C:\\Program Files\\Git\\usr\\bin\\grep.exe\""
;;       null-device "/dev/null"))

;; 除外ディレクトリ
;; 将来的には入力できるようにする
(add-to-list 'grep-find-ignored-directories "logs")
(add-to-list 'grep-find-ignored-directories "caches")

;; es-grep
(defun es-grep (string ext pwd)
  "It asks STRING and EXT for grep command line and PWD for current directory."
  (interactive
   (progn
     (let ((default (es-get-strings "search"))
					 (target-ext (concat "*." (file-name-extension (buffer-file-name))))
					 (target-dir (if (gtags-get-rootpath)
													 (directory-file-name (gtags-get-rootpath))
												 default-directory)))
			 (grep-compute-defaults)
       (list (read-from-minibuffer "Search: " default nil nil 'grep-history (if current-prefix-arg nil default))
						 (read-from-minibuffer "Extension (plural available): " target-ext)
             (read-directory-name "Directory: " target-dir target-dir t)))))
  (rgrep string ext pwd nil))

;; defadvice rgrep
;; thx http://d.hatena.ne.jp/kitokitoki/20101009/p6
(defadvice rgrep (before my-rgrep activate)
  "Confirm delete grep buffer."
  (when (get-buffer "*grep*")
    (when (y-or-n-p "Delete existing *grep* buffer?")
      (kill-buffer (get-buffer "*grep*")))))

;; (require 'grep) ;lgrepで直下をgrep、rgrepで再帰的に
;; ;;grep-edit
;; ;(install-elisp "http://www.emacswiki.org/emacs/download/grep-edit.el")
;; (require 'grep-edit) ;C-c C-e で編集を反映 C-x s ! で全部保存

;;; ------------------------------------------------------------
;;; experiment 検索履歴
;; (global-set-key (kbd "C--") 'es-anything-grep)

;;; 検索か置換をしたら、候補をファイルに保存する
;;; thx undohist
(defcustom es-history-directory
	(expand-file-name
	 (concat
		(if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d")
		"/es-hist"))
	"A directory being stored searched/replaced history files.")
(defvar es-history-filename (concat es-history-directory "/es-hist.dat"))

;; (defun es-initialize ()
;; 	"Initialize editable seacrh directory."
;; 	(interactive)
;; 	(if (not (file-directory-p es-history-directory))
;; 			(make-directory es-history-directory t))
;; 	(if (not (file-p (concat es-history-directory "/es-hist.txt"))
;; 			(make-file es-history-directory t))))
;; undohistでは、hookを使って、saveしているので、そうするのがよいか。

;;; multibyte-base64-encode-string
(defun multibyte-base64-encode-string (str)
	"Multibyte base64 encode string.  STR."
	(interactive)
	(base64-encode-string (encode-coding-string str 'raw-text) t))

;;; multibyte-base64-decode-string
(defun multibyte-base64-decode-string (str)
	"Multibyte base64 decode string.  STR."
	(interactive)
	(decode-coding-string (base64-decode-string str) 'utf-8))

;;; es-hist-load
(defun es-hist-load ()
	"Load es history."
	(interactive)
		(with-temp-buffer
			(insert-file-contents es-history-filename)
			(if (<= (point-max) 2)
					(list)
				(read (buffer-string)))))

;;; es-hist-save
(defun es-hist-save ()
	"Save es history."
	(interactive)
	(let
			(search-str
			 replace-str
			 history)
		(when (and (windowp (get-buffer-window es-search-str-buffer))
							 (windowp (get-buffer-window es-replace-str-buffer)))
			(setq search-str (multibyte-base64-encode-string (es-get-str-from-window "search")))
			(setq replace-str (multibyte-base64-encode-string (es-get-str-from-window "replace")))
			(when search-str
				(with-temp-buffer
					(insert-file-contents es-history-filename)
					(setq history (es-hist-load))
					(add-to-list 'history (list search-str replace-str))
					(insert (format "%s" history))
					(write-region (point-min) (point-max) es-history-filename nil 0))))))

;;; 履歴から検索置換文字列を復活
(defun es-hist-prev ()
	"Call previous set."
	(let (history
				current)
		(with-temp-buffer
			(insert-file-contents es-history-filename)
			(setq history (es-hist-load))
			(setq current (car history))
			;; (cdr history)
			(message "%s" current)
			)))

;; 保存すべき文字列がないときの処理
;; すでに保存されているセットの場合。古いものを削除して、一番上に
;; 保存すべき数の上限をdefvarで

;; (es-hist-save)
;; (es-hist-prev)

  ;; (if (consp buffer-undo-list)
  ;;     (let ((file (make-undohist-file-name (buffer-file-name)))
  ;;           (contents `((digest . ,(md5 (current-buffer)))
  ;;                       (undo-list . ,(undohist-encode buffer-undo-list)))))
  ;;       (with-temp-buffer
  ;;         (print contents (current-buffer))
  ;;         (write-region (point-min) (point-max) file nil 0)
  ;;         (set-file-modes file ?\600)))))

;;; es-is-next-window-by-tab
;; (when es-is-next-window-by-tab
;; 	(define-key editable-search-mode-map [tab] 'es-next-windows-dwim))
;; (defun es-next-windows-dwim ()
;; 	"Next windows dwim."
;; 	(interactive)
;; 	(when (or (equal (selected-window) (get-buffer-window es-search-str-buffer))
;; 					(equal (selected-window) (get-buffer-window es-replace-str-buffer)))
;; 			(select-window (next-window))))

;; (global-set-key (kbd "C--") 'anything-grep2)

;;; ------------------------------------------------------------
;;; Provide

(provide 'editable-search)

;;; editable-search.el ends here
