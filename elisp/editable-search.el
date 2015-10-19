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
(defvar es-rearch-window-foreground-color "White" "*Default 'Black'.")
(defvar es-re-rearch-window-foreground-color "White" "*Default 'Black'.")
(defvar es-rearch-window-background-color "Black" "*Default 'Black'.")
(defvar es-re-rearch-window-background-color "Black" "*Default 'Black'.")
(defvar es-target-window)
(defvar es-search-str-window "*search string*")
(defvar es-replace-str-window "*replace string*")
(defvar es-previous-searced-str)
(defvar es-previous-replaced-str)
(defvar editable-search-mode-map (make-keymap))
(defvar editable-re-search-mode-map (make-keymap))

;;; ------------------------------------------------------------
;;; hook
;;; 選択範囲がある状態でshiftなしのカーソルが打鍵されたらリージョンを解除
(when es-is-deactivate-region-by-cursor
	(progn
		(add-hook 'post-command-hook 'es-post-command-hook-fn)
		(defun es-post-command-hook-fn ()
			"Deactivate region by cursor."
			;; (message "this-event:  %s\nthis-command:%s" last-input-event this-command)
			(when (and (region-active-p) (memq last-input-event '(left right down up)))
				(progn
					(cond
					 ((memq last-input-event '(right down))
						(goto-char (region-end)))
					 ((memq last-input-event '(left up))
						(goto-char (region-beginning))))
					(deactivate-mark)))
			;; おまけ（1行目と最終行のカーソルの振る舞いをmac likeに）
			(when (and (eq (line-number-at-pos) 1) (memq last-input-event '(up)))
				(beginning-of-line))
			(when (and (eq (line-number-at-pos) (count-lines 1 (point-max)))
								 (memq last-input-event '(down)))
				(end-of-line)))))

;;; 削除によってウィンドウ構成を変えようとしたら検索置換窓を閉じる
(add-hook 'post-command-hook 'es-delete-window-fn)
(defun es-delete-window-fn ()
			 "About search mode windows."
			 (let
					 ((search-windowp (windowp (get-buffer-window es-search-str-window)))
						(replace-windowp (windowp (get-buffer-window es-replace-str-window))))
				 ;; (message "this-command:%s" this-command)
				 (when (memq this-command '(delete-window
																		kill-buffer-and-window
																		delete-other-windows
																		contexual-close-window
																		mouse-delete-window
																		mouse-delete-other-windows))
					 (progn
						 (message "close search windows.")
						 (when search-windowp
							 (progn
								 (select-window (get-buffer-window es-search-str-window))
								 (kill-buffer-and-window)))
						 (when replace-windowp
							 (progn
								 (select-window (get-buffer-window es-replace-str-window))
								 (kill-buffer-and-window)))))))

;;; ------------------------------------------------------------
;;; key-binds
(if es-is-use-super
		(progn
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
																														(es-search-replace "re-rep-here")))))

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
;;; 検索と置換用のウィンドウをdeleteする
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
					(setq es-target-window (selected-window)) ; 対象ウィンドウは毎度明示する
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
				 (re-flag-str (substring mode 0 3))
				 (direction-flag-str (substring mode -4))
				 (replace-flag-str (if (>= (length mode) 6) (substring mode 0 6) ""))
				 (is-re (if (string= re-flag-str "re-") t nil))
				 (is-next (if (string= direction-flag-str "next") t nil))
				 (is-prev (if (string= direction-flag-str "prev") t nil))
				 (is-replace (if (or (string= replace-flag-str "rep-ne")
														 (string= replace-flag-str "rep-pr")
														 (string= replace-flag-str "rep-he")
														 (string= replace-flag-str "re-rep")) t nil))
				 (is-replace-here (if (string= mode "rep-here") t nil))
				 target-str)

		;; 現在のウィンドウが検索・置換編集用ウィンドウだったら、主たるウィンドウに移動する
		(if (eq (selected-window) es-target-window) ()
			(select-window es-target-window))

		;; 検索用文字列の取得
		(if (windowp (get-buffer-window es-search-str-window))
				(with-selected-window (get-buffer-window es-search-str-window)
					(setq search-str (buffer-string)))
			(setq search-str (if (boundp 'es-previous-searced-str) es-previous-searced-str nil)))

		;; 検索文字列がなければ、エラーを返す
		(if (not search-str) (error "Error: search word is empty"))

		;; 置換用文字列の取得
		(if (windowp (get-buffer-window es-replace-str-window))
				(with-selected-window (get-buffer-window es-replace-str-window)
					(setq replace-str (buffer-string)))
			(setq replace-str (if (boundp 'es-previous-replaced-str) es-previous-replaced-str nil)))

		;; 置換モードなのに置換文字列がなければ、エラーを返す
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
		(setq es-previous-searced-str search-str)
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
		(if (windowp (get-buffer-window es-search-str-window))
				(with-selected-window (get-buffer-window es-search-str-window)
					(setq search-str (buffer-string)))
			(setq search-str (if (boundp 'es-previous-searced-str) es-previous-searced-str nil)))
		(if (not search-str) (error "Error: search word is empty"))

		;; 置換用文字列の取得
		(if (windowp (get-buffer-window es-replace-str-window))
				(with-selected-window (get-buffer-window es-replace-str-window)
					(setq replace-str (buffer-string)))
			(setq replace-str (if (boundp 'es-previous-replaced-str) es-previous-replaced-str nil)))
		(if (not replace-str) (error "Error: replace word is empty"))

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
							(es-replace-region search-str replace-str is-re))))
				(setq cnt (1+ cnt))))
		(message "%s replaced." cnt)

		;; 今回検索・置換した文字を次回用に保存
		(setq es-previous-searced-str search-str)
		(setq es-previous-replaced-str replace-str)))

;;; ------------------------------------------------------------
;;; Provide

(provide 'editable-search)

;;; editable-search.el ends here
