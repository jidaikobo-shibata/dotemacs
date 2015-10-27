;;; editable-search.el --- search by other window buffer
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: jidaikobo
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; あたらしいウィンドウを開いて、そこで編集した文字列で検索や置換を行います。

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

(defcustom es-is-deactivate-region-by-cursor nil
	"*Mac-like behavior."
	:group 'editable-search
	:type 'boolean)

;;; defvar
(defvar es-rearch-window-foreground-color "White" "*Default 'White'.")
(defvar es-re-rearch-window-foreground-color "White" "*Default 'White'.")
(defvar es-rearch-window-background-color "Black" "*Default 'Black'.")
(defvar es-re-rearch-window-background-color "Black" "*Default 'Black'.")
(defvar es-target-window)
(defvar es-search-str-window "*search string*")
(defvar es-replace-str-window "*replace string*")
(defvar es-previous-searched-str)
(defvar es-previous-replaced-str)
(defvar es-previous-searced-direction nil)
(defvar es-previous-direction)
(defvar editable-search-mode-map (make-keymap))
(defvar editable-re-search-mode-map (make-keymap))

;;; ------------------------------------------------------------
;;; hook and advice

;; (setq debug-on-error nil)
;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
(when es-is-deactivate-region-by-cursor
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
		(when (and (region-active-p) (not (memq last-input-event '(S-left S-right S-down S-up))))
			(message "%s" this-command)
			(cond
			 ((memq last-input-event '(right down))
				(goto-char (region-end)))
			 ((memq this-command '(left-char previous-line))
				(goto-char (region-beginning))))
			(deactivate-mark)))

	;; おまけ。一行目と最終行での上下キーの振る舞い（行末と行頭へ）
	(defvar prev-line-num (line-number-at-pos))
	(add-hook 'post-command-hook 'es-goto-the-edge)
	(defun es-goto-the-edge ()
		"Go to the edge of the line."
		;; (message "this-event:  %s\nthis-command:%s" last-input-event this-command)
		(when (and (eq prev-line-num 1) (memq last-input-event '(up)))
			(beginning-of-line))
		(when (and (eq prev-line-num (count-lines 1 (point-max)))
							 (memq last-input-event '(down)))
			(end-of-line))
		(setq prev-line-num (line-number-at-pos))))

;;; 削除によってウィンドウ構成を変えようとしたら検索置換窓を閉じる
(add-hook 'post-command-hook 'es-delete-window-fn)
(defun es-delete-window-fn ()
	"About search mode windows."
	(when (editable-search-mode)
		(let
				((search-windowp (windowp (get-buffer-window es-search-str-window)))
				 (replace-windowp (windowp (get-buffer-window es-replace-str-window))))
			;; (message "this-command:%s" this-command)
			(when (memq this-command '(delete-window
																 kill-buffer
																 kill-buffer-and-window
																 delete-other-windows
																 mouse-delete-window
																 mouse-delete-other-windows
																 contexual-close-window))
				(progn
					(message "close search windows.")
					(when search-windowp
						(progn
							(select-window (get-buffer-window es-search-str-window))
							(kill-buffer-and-window)))
					(when replace-windowp
						(progn
							(select-window (get-buffer-window es-replace-str-window))
							(kill-buffer-and-window))))))))

;;; ------------------------------------------------------------
;;; key-binds

(when es-is-use-super
	;; global-map
	(define-key global-map (kbd "s-e") (lambda () (interactive)
																			 (es-enter-mode "set-to-search")))
	(define-key global-map (kbd "s-E") (lambda () (interactive)
																			 (es-enter-mode "set-to-replace")))
	(define-key global-map (kbd "s-f") (lambda () (interactive)
																			 (es-enter-mode "")))
	(define-key global-map (kbd "s-g") (lambda () (interactive)
																			 (es-search-replace "next")))
	(define-key global-map (kbd "s-G") (lambda () (interactive)
																			 (es-search-replace "prev")))
	(define-key global-map (kbd "s-l") (lambda () (interactive)
																			 (es-search-replace "rep-next")))
	(define-key global-map (kbd "s-r") (lambda () (interactive)
																			 (es-search-replace "rep-here")))

	;; local-map editable-search-mode-map
	(define-key editable-search-mode-map (kbd "s-R") (lambda () (interactive)
																										 (es-replace-all "")))
	(define-key editable-search-mode-map [escape] 'keyboard-quit)
	(define-key editable-search-mode-map (kbd "s-F") 'es-delete-windows)
	(define-key editable-search-mode-map (kbd "C-s-f") 'es-toggle-search-mode)
	(define-key editable-search-mode-map (kbd "s-h") (lambda () (interactive)
																										 (select-window es-target-window)))

	;; local-map editable-re-search-mode-map
	(define-key editable-re-search-mode-map (kbd "s-R") (lambda () (interactive)
																												(es-replace-all "re")))
	(define-key editable-re-search-mode-map (kbd "s-g") (lambda () (interactive)
																												(es-search-replace "re-next")))
	(define-key editable-re-search-mode-map (kbd "s-G") (lambda () (interactive)
																												(es-search-replace "re-prev")))
	(define-key editable-re-search-mode-map (kbd "s-l") (lambda () (interactive)
																												(es-search-replace "re-rep-next")))
	(define-key editable-re-search-mode-map (kbd "s-r") (lambda () (interactive)
																												(es-search-replace "re-rep-here"))))

;;; ------------------------------------------------------------
;;; 検索置換用のマイナーモードを設定する
(define-minor-mode editable-search-mode
	"Provide editable search/replace environment."
	:init-value
	nil
	:lighter
	" Search"
	:keymap
	editable-search-mode-map)

;;; 正規表現モード
(define-minor-mode editable-re-search-mode
	"Provide editable regular expression search/replace environment."
	:init-value
	nil
	:lighter
	" re-Search"
	:keymap
	editable-re-search-mode-map)

;;; 正規表現モードのトグル
(defun es-toggle-search-mode ()
	"Toggle search mode."
	(interactive)
	;; es-toggle-search-modeは、local-mapなので、editable-search-modeからしか呼ばれないはず。
	;; ゆえに、es-target-windowはたぶん存在する。
	(when (windowp es-target-window)
		(progn
			(with-selected-window es-target-window
				(if (eq editable-re-search-mode nil)
						(editable-re-search-mode t)
					(editable-re-search-mode -1))))))

;;; ------------------------------------------------------------
;;; 検索と置換用のウィンドウをdeleteして、editable-seach-modeを抜ける
(defun es-delete-windows ()
	"Delete splited windows."
	(interactive)
	(if (windowp (get-buffer-window es-search-str-window))
			(delete-window (get-buffer-window es-search-str-window)))
	(if (windowp (get-buffer-window es-replace-str-window))
			(delete-window (get-buffer-window es-replace-str-window)))
	(editable-search-mode -1))

;;; 検索と置換用のウィンドウを用意してサーチモードに入る
(declare-function global-auto-complete-mode "global-auto-complete-mode" (bool))
(defun es-enter-mode (mode)
	"Split window for search and replace.  MODE [set-to-search|set-to-replace]."
	(interactive)
	(let* ((is-search-window-exist (windowp (get-buffer-window es-search-str-window)))
				 (is-replace-window-exist (windowp (get-buffer-window es-replace-str-window)))
				 (beg (if mark-active (region-beginning)))
				 (end (if mark-active (region-end)))
				 (word (if mark-active (buffer-substring-no-properties beg end) "")))
		;; 検索窓、置換窓がすでに開いている場合は、キャレットを移動
		(if (and is-search-window-exist is-replace-window-exist)
				(progn
					;; キャレットが検索置換窓にあったら、target-windowを変更しない
					(unless (or (equal (selected-window) (get-buffer-window es-search-str-window))
											(equal (selected-window) (get-buffer-window es-replace-str-window)))
						(setq es-target-window (selected-window)))
					(editable-search-mode t)
					(select-window (get-buffer-window es-search-str-window)))
			(progn
				;; どちらかだけ開いていたら、もう片方を閉じる
				(if is-search-window-exist (delete-window (get-buffer-window es-search-str-window)))
				(if is-replace-window-exist (delete-window (get-buffer-window es-replace-str-window)))
				;; 検索と置換の窓を用意して、マイナーモードを変更する
				(setq es-target-window (selected-window))
				(editable-search-mode t)
				(split-window-horizontally)
				(select-window (next-window))
				(switch-to-buffer es-search-str-window)
				(when (require 'auto-complete) (global-auto-complete-mode t))
				(set-window-dedicated-p (selected-window) t) ;変更を許さないウィンドウにする
				(editable-search-mode t)
				(split-window-vertically)
				(select-window (next-window))
				(switch-to-buffer es-replace-str-window)
				(when (require 'auto-complete) (global-auto-complete-mode t))
				(set-window-dedicated-p (selected-window) t) ;変更を許さないウィンドウにする
				(editable-search-mode t)
				(if (string= mode "") (select-window (previous-window)))))
		;; 検索文字列をセットする場合
		(if (and (windowp (get-buffer-window es-search-str-window))
						 (windowp (get-buffer-window es-replace-str-window))
						 (string= mode "set-to-search"))
				(select-window (get-buffer-window es-search-str-window)))
		;; 置換文字列をセットする場合
		(if (and (windowp (get-buffer-window es-search-str-window))
						 (windowp (get-buffer-window es-replace-str-window))
						 (string= mode "set-to-replace"))
				(select-window (get-buffer-window es-replace-str-window)))
		;; 候補文字列の置き換え
		(if (or (string= mode "set-to-search") (string= mode "set-to-replace"))
				(progn (goto-char (point-min))
							 (set-mark (point))
							 (goto-char (point-max))
							 (delete-region (point-min) (point-max))
							 (insert word)
							 (select-window es-target-window)))))

;;; ------------------------------------------------------------
;;; 共通関数

;; 選択範囲の置換用関数
(defun es-replace-region (search-str replace-str is-re)
	"(string)SEARCH-STR, (string)REPLACE-STR, (bool)IS-RE."
	(when mark-active
		(let ((beg (region-beginning))
					(end (region-end)))
			(progn
				(when is-re
					(setq replace-str (replace-regexp-in-string
														 search-str
														 replace-str
														 (buffer-substring-no-properties beg end))))
				(delete-region beg end)
				(insert replace-str)))))

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
			 (set-mark-command nil)
			 (goto-char end))
			((string= direction "prev")
			 (setq end (+ (point) len-search-string))
			 (goto-char end)
			 (setq beg (- (point) len-search-string))
			 (set-mark-command nil)
			 (goto-char beg)))
		 (setq deactivate-mark nil))))

;; 文字列の取得
(defun es-get-str-from-window (type)
	"Get str from window.  TYPE[search|replace]."
	(interactive)
	(let ((window-obj (if (string= type "search") (get-buffer-window es-search-str-window)
										 (get-buffer-window es-replace-str-window))))
		(if (windowp window-obj)
				(with-selected-window window-obj (buffer-string)) nil)))

;;; ------------------------------------------------------------
;;; 検索用バッファの文字列で検索する
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
				 (is-replace-here (if (string= mode "rep-here") t nil))
				 target-str)

		;; 検索方向が変わったら向きを変える
		(when (and mark-active (not (string= es-previous-searced-direction direction)))
			(exchange-point-and-mark))
		(setq es-previous-searced-direction direction)

		;; 現在のウィンドウが検索・置換編集用ウィンドウだったら、主たるウィンドウに移動する
		(when (not (eq (selected-window) es-target-window))
			(select-window es-target-window))

		;; 検索用文字列の取得（必須）
		(setq search-str (es-get-str-from-window "search"))
		(unless search-str
			(setq search-str (if (boundp 'es-previous-searched-str) es-previous-searched-str nil)))
		(unless search-str (error "Error: search word is empty"))

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
				(re-search-forward search-str))
			 ((and is-re is-prev)
				(re-search-backward search-str)))
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
(defun es-replace-all (mode)
	"Replace All.  MODE [re]."
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
		(if (eq (selected-window) es-target-window) ()
			(select-window es-target-window))

		;; 検索用文字列の取得
		(setq search-str (es-get-str-from-window "search"))
		(unless search-str
			(setq search-str (if (boundp 'es-previous-searched-str) es-previous-searched-str nil)))
		(unless search-str (error "Error: search word is empty"))

		;; 置換用文字列の取得
		(setq replace-str (es-get-str-from-window "replace"))
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
					(goto-char (point-min))
					(while (<= (point) (point-max))
						(progn
							;; 文字列を走査
							(if is-re
									(re-search-forward search-str)
								(search-forward search-str))
							(progn
								;; ヒットした文字長の取得
								(if is-re
										(setq len-search-string (length (match-string-no-properties 0)))
									(setq len-search-string (length search-str)))
								;; 選択範囲の設定と置換
								(es-generate-region "next" len-search-string)
								(es-replace-region search-str replace-str is-re)))
						(setq cnt (1+ cnt))))))
		(message "%s replaced." cnt)

		;; 今回検索・置換した文字を次回用に保存
		(setq es-previous-searched-str search-str)
		(setq es-previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;;; Provide

(provide 'editable-search)

;;; ------------------------------------------------------------
;;; experiment マルチファイル検索
;;; 対象ディレクトリと拡張子を指定したら検索する
(defvar es-file-list)
(setq es-file-list (shell-command-to-string "cd ~/Desktop/test/; find ./ -name \"*.txt\" -type f"))

;;; ------------------------------------------------------------
;;; experiment 検索履歴
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
		(when (and (windowp (get-buffer-window es-search-str-window))
							 (windowp (get-buffer-window es-replace-str-window)))
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

;;; Todo:
;; editable-search-mode を抜けられない？ -1 じゃないの？
;; マルチファイル検索置換
;; 検索履歴
;; 検索置換のプリセット
;; 検索時に出る（ことがある）エラーの調査
;; 検索置換において、情報エリアを作って、ターゲットウィンドウと正規表現モードかどうかを表示する
;; emacs likeなデフォルトのキーバインド

;;; editable-search.el ends here
