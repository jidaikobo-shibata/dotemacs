;;; web-authoring-set.el --- jidaikobo web authoring
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: jidaikobo
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;;; ------------------------------------------------------------
;; 時代工房のウェブ制作用キーバインド集
;; 基本的にはcmd+opt+(M-s-)となにか一文字で操作する

;; cmd+opt+v: 任意のタグ
;; -cmd+opt+a: anchor
;; -cmd+opt+shift+5: anchor self
;; -cmd+opt+1: h1
;; -cmd+opt+2: h2
;; -cmd+opt+3: h3
;; -cmd+opt+4: h4
;; -cmd+opt+5: h5
;; -cmd+opt+6: h6
;; -cmd+opt+t: table intaractive
;; -cmd+opt+s: span
;; -cmd+opt+g: strong
;; -cmd+opt+u: ul
;; -cmd+opt+o: ol
;; -cmd+opt+d: dl
;; -cmd+opt+q: blockquote
;; -cmd+opt+p: p
;; -cmd+opt+shift+p: p each lines
;;
;; cmd+shift+a: タグを選択
;; cmd+RET: <br />を入力
;; cmd+opt+r: タグを削除
;; cmd+opt+shift+r: タグを実体参照（or タグ）に
;;
;; cmd+opt+z: var_dump()
;; cmd+opt+shift+z: var_dump()でipを指定
;; cmd+opt+k: 編集中のファイルのカレントパス
;;
;; cmd+/: 選択範囲を一行に
;; cmd+u: 選択範囲の全角数字を半角に
;; cmd+shift+u: 選択範囲をuppper case
;; cmd+shift+l: 選択範囲をlower case
;; cmd+shift+c: 選択範囲をcapitalize

;;; Code:

;;; ------------------------------------------------------------
;;; 選択範囲内の文字列置換
;; thx http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
(defun replace-strings-in-region-by-list (list)
	"Replace strings in a region according to LIST."
	(if mark-active
			(let* ((beg (region-beginning))
						 (end (region-end))
						 (word (buffer-substring-no-properties beg end)))
				(mapc (lambda (r)
								(setq word (replace-regexp-in-string (car r) (cdr r) word)))
							list)
				(delete-region beg end)
				(insert word))
		(error "Need to make region")))

;;; ------------------------------------------------------------
;;; dump-values
;;; phpでvar_dump()するためのキーバインド
(declare-function find "find" (arg1 arg2 arg3 arg4))
(defun dump-values (type ip)
	"Insert html intaractive.  TYPE is language.  IP is Global ip."
	(interactive)
	(let* ((beg (if mark-active (region-beginning) 0))
				 (end (if mark-active (region-end) 0))
				 (word (buffer-substring-no-properties beg end))
				 ret)
		(when (string= word "") (setq word "$vals"))
		(cond
		 ;; php
		 ((string-equal type "php")
			(setq ret (concat "echo '<textarea style=\"width:100%;height:200px;background-color:#fff;color:#111;font-size:90%;font-family:monospace;position:relative;z-index:9999\">';\nvar_dump(" word ");\necho '</textarea>';\ndie();"))))

		;; ip
		(if (string-equal ip "") nil
			(setq ret (concat "if( $_SERVER['REMOTE_ADDR'] == '" ip "' ){\n" ret "\n}\n")))

		;; put val
		(if mark-active (delete-region beg end) nil)
		(insert ret)))

;; php without ip
(global-set-key (kbd "s-M-z") (lambda () (interactive)
																(dump-values "php" ""))) ; cmd+opt+z
;; php with ip
(global-set-key (kbd "s-M-Z") (lambda (ip) (interactive "sIP:")
																(dump-values "php" ip))) ; cmd+opt+shift+z
;; JavaScript
;;(global-set-key (kbd "s-M-z") (lambda () (interactive)
;;																(dump-values "javascript" ""))) ; cmd+opt+z

;;; ------------------------------------------------------------
;;; php open and close
(defun put-php-opener-closer (type)
	"Put php opner and closer.  choose TYPE."
	(interactive "nType 1:<?php ?>, 2:<?php(RET)?>, 3:?><?php:")
	(let* ((beg (if mark-active (region-beginning) (point)))
				 (end (if mark-active (region-end) (point)))
				 (word (if mark-active (buffer-substring-no-properties beg end) ""))
				 (ret))
		(cond
		 ((eq type 2)
			(setq ret (concat "<?php\n" word "\n?>")))
		 ((eq type 3)
			(setq ret (concat "?>" word "<?php")))
		 (t
			(if (/= (length word) 0) (setq word (concat word " ")) word)
			(setq ret (concat "<?php " word "?>"))))
		(if mark-active (delete-region beg end) nil)
		(insert ret)))
(global-set-key (kbd "s-M-h") 'put-php-opener-closer) ; cmd+shift+h

;;; ------------------------------------------------------------
;;; 任意のタグ
;;; ミニバッファにタグを入れると基本的には選択範囲を囲むタグを生成する
;;; タグに応じて、いくらか振る舞いが変わる
(declare-function convert-to-th "convert-to-th" (arg))
(declare-function convert-to-td "convert-to-td" (arg))
(defun any-html-tag (tag)
	"Insert html intaractive.  TAG."
	(interactive "sTag (default \"div\"): ")
	(let* ((beg (when (region-active-p) (region-beginning)))
				 (end (when (region-active-p) (region-end)))
				 (word (if (region-active-p) (buffer-substring-no-properties beg end) ""))
				 url
				 type
				 html
				 lines
				 line
				 cnt)
		(cond
		 ;; anchor
		 ((string-equal tag "a")
			(setq url (read-string "url: " nil 'my-history))
			(setq tag (concat "<a href=\"" url "\">" word "</a>")))
		 ;; anchor-url
		 ((string-equal tag "a-url")
			(if (string-match "@" word)
					(setq tag (concat "<a href=\"mailto:" word "\">" word "</a>"))
				(setq tag (concat "<a href=\"" word "\">" word "</a>"))))
		 ;; input
		 ((string-equal tag "input")
			(setq type (read-number "type (1:text, 2:hidden, 3:radio, 4:checkbox, 5:submit, 6:password, 7:image, 8:file): " nil))
			(cond
			 ((eq type 1)
				(setq tag "<input type=\"text\" name=\"nameStr\" id=\"idStr\" size=\"20\" value=\"\" />"))
			 ((eq type 2)
				(setq tag "<input type=\"hidden\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))
			 ((eq type 3)
				(setq tag "<input type=\"radio\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))
			 ((eq type 4)
				(setq tag "<input type=\"checkbox\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))
			 ((eq type 5)
				(setq tag "<input type=\"submit\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))
			 ((eq type 6)
				(setq tag "<input type=\"password\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))
			 ((eq type 7)
				(setq tag "<input type=\"image\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))
			 ((eq type 8)
				(setq tag "<input type=\"file\" name=\"nameStr\" id=\"idStr\" value=\"\" />"))))

		 ;; singular tag - hr, br
		 ((find tag '("hr" "br") :test #'string=)
			(setq tag (concat "<" tag " />")))

		 ;; singular tag - img
		 ((string-equal tag "img")
			(setq tag (concat "<img src=\"" word "\" alt=\"\" />")))

		 ;; ul-li, ol-li
		 ((find tag '("ul-li" "ol-li") :test #'string=)
			(setq html "")
			(setq lines (split-string word "\n"))
			(while lines
				(if (string-equal (car lines) "") nil
					(progn (setq html (concat html "\t<li>" (car lines) "</li>\n"))))
				(setq lines (cdr lines)))
			(if (string-equal tag "ul-li")
					(setq tag (concat "<ul>\n" html "</ul>\n"))
				(setq tag (concat "<ol>\n" html "</ol>\n"))))

		 ;; select
		 ((string-equal tag "select")
			(setq html "")
			(setq lines (split-string word "\n"))
			(while lines
				(if (string-equal (car lines) "") nil
					(progn (setq html (concat html "\t<option>" (car lines) "</select>\n"))))
				(setq lines (cdr lines)))
			(setq tag (concat "<select name=\"nameStr\" id=\"idStr\">\n" html "</select>\n")))

		 ;; p-each
		 ((string-equal tag "p-each")
			(setq html "")
			(setq lines (split-string word "\n"))
			(while lines
				(if (string-equal (car lines) "") nil
					(progn (setq html (concat html "<p>" (car lines) "</p>\n"))))
				(setq lines (cdr lines)))
			(setq tag html))

		 ;; table
		 ((string-equal tag "table")
			(setq type (read-string "type (1:th, 2:thead, 3:th and thead, 4:no headers): " nil 'my-history))
			(setq html "")
			(setq cnt 1)
			(setq lines (split-string word "\n"))
			(while lines
				(if (string-equal (car lines) "") nil
					(progn
						(defun convert-to-th (each-line) (concat "<thead>\n<tr>\n\t<th>" (replace-regexp-in-string "\t" "</th>\n\t<th>" each-line) "</th>\n</tr>\n</thead>\n"))
						(defun convert-to-td (each-line) (concat "<tr>\n\t<td>" (replace-regexp-in-string "\t" "</td>\n\t<td>" each-line) "</td>\n</tr>\n"))
						;; (defun add-tr-thead (whole each) )
						(if (eq cnt 1)
								(if (find type '("2" "3") :test #'string=)
										(setq line (convert-to-th (car lines)))
									(setq line (convert-to-td (car lines))))
							(setq line (convert-to-td (car lines))))
						(setq html (concat html line))))
				(setq cnt 2)
				(setq lines (cdr lines)))
			(setq tag (concat "<table>\n" html "</table>\n"))
			(if (find type '("1" "3") :test #'string=)
					(setq tag (replace-regexp-in-string "<tr>\n\t<td>\\(.+?\\)</td>" "<tr>\n\t<th>\\1</th>" tag))))

		 ;; dl
		 ((string-equal tag "dl-dt-dd")
			(setq lines (split-string word "\n"))
			(while lines
				(if (string-equal (car lines) "") nil
					(progn
						(if (string-match "\t" word)
								(setq line (concat "<dt>" (replace-regexp-in-string "\t" "</dt>\n\t<dd>" (car lines)) "</dd>\n"))
							(setq line (concat "<dt>" (car lines) "</dd>\n")))))
				(setq html (concat html line))
				(setq lines (cdr lines)))
			(setq tag (concat "<dl>\n" html "</dl>\n")))

		 ;; comment out
		 ((string-equal tag "comment-out")
			(setq tag (concat "<!-- " word " -->")))

		 ;; script
		 ((string-equal tag "script")
			(setq tag (concat "<script type=\"text/javascript\">\n<!--" word "// -->\n</script>")))

		 ;; style
		 ((string-equal tag "style")
			(setq tag (concat "<style type=\"text/css\">\n" word "\n</style>")))

		 ;; form
		 ((string-equal tag "form")
			(setq tag (concat "<form action=\"actionStr\" method=\"POST\" enctype=\"multipart/form-data\">\n" word "\n</form>\n")))

		 ;; textarea
		 ((string-equal tag "textarea")
			(setq tag (concat "<textarea name=\"nameStr\" id=\"isStr\" cols=\"35\" rows=\"7\">\n" word "\n</textarea>\n")))

		 ;; label
		 ((string-equal tag "label")
			(setq tag (concat "<label for=\"forStr\">\n" word "\n</label>\n")))

		 ;; specify tag
		 (t (when (string-equal tag "") (setq tag "div"))
				(setq tag (concat "<" tag ">" word "</" tag ">"))))

		;; put tags
		(if (region-active-p) (delete-region beg end) nil)
		(insert tag)))
(global-set-key (kbd "s-M-v") 'any-html-tag) ; cmd+shift+v

;;; headings
(defun h1-tag ()
	"Add h1."
	(interactive)
	(any-html-tag "h1"))
(defun h2-tag ()
	"Add h2."
	(interactive)
	(any-html-tag "h2"))
(defun h3-tag ()
	"Add h3."
	(interactive)
	(any-html-tag "h3"))
(defun h4-tag ()
	"Add h4."
	(interactive)
	(any-html-tag "h4"))
(defun h5-tag ()
	"Add h4."
	(interactive)
	(any-html-tag "h5"))
(defun h6-tag ()
	"Add h6."
	(interactive)
	(any-html-tag "h6"))
(global-set-key (kbd "s-M-1") 'h1-tag) ; opt+cmd+1
(global-set-key (kbd "s-M-2") 'h2-tag) ; opt+cmd+2
(global-set-key (kbd "s-M-3") 'h3-tag) ; opt+cmd+3
(global-set-key (kbd "s-M-4") 'h4-tag) ; opt+cmd+4
(global-set-key (kbd "s-M-5") 'h5-tag) ; opt+cmd+5
(global-set-key (kbd "s-M-6") 'h6-tag) ; opt+cmd+6
(global-set-key [M-s-kp-1] 'h1-tag) ; opt+cmd+1
(global-set-key [M-s-kp-2] 'h2-tag) ; opt+cmd+2
(global-set-key [M-s-kp-3] 'h3-tag) ; opt+cmd+3
(global-set-key [M-s-kp-4] 'h4-tag) ; opt+cmd+4
(global-set-key [M-s-kp-5] 'h5-tag) ; opt+cmd+5
(global-set-key [M-s-kp-6] 'h6-tag) ; opt+cmd+6

;;; a
(global-set-key (kbd "s-M-a") '(lambda ()
																 (interactive)
																 (any-html-tag "a"))) ; opt+cmd+a

;;; a-url
(global-set-key (kbd "s-M-A") '(lambda ()
																 (interactive)
																 (any-html-tag "a-url"))) ; opt+cmd+shif+a
;;; strong
(global-set-key (kbd "s-M-g") '(lambda ()
																 (interactive)
																 (any-html-tag "strong"))) ; opt+cmd+g

;;; span
(global-set-key (kbd "s-M-s") '(lambda ()
																 (interactive)
																 (any-html-tag "span"))) ; opt+cmd+s

;;; comment-out
(global-set-key (kbd "s-M-c") '(lambda ()
																 (interactive)
																 (any-html-tag "comment-out"))) ; opt+cmd+c

;;; blockquote
(global-set-key (kbd "s-M-q") '(lambda ()
																 (interactive)
																 (any-html-tag "blockquote"))) ; opt+cmd+q

;;; p
(global-set-key (kbd "s-M-p") '(lambda ()
																 (interactive)
																 (any-html-tag "p-each"))) ; opt+cmd+p

;;; ul-li
(global-set-key (kbd "s-M-u") '(lambda ()
																 (interactive)
																 (any-html-tag "ul-li"))) ; opt+cmd+u

;;; ol-li
(global-set-key (kbd "s-M-o") '(lambda ()
																 (interactive)
																 (any-html-tag "ol-li"))) ; opt+cmd+o

;;; dl-dt-dd
(global-set-key (kbd "s-M-d") '(lambda ()
																 (interactive)
																 (any-html-tag "dl-dt-dd"))) ; opt+cmd+d

;;; table
(global-set-key (kbd "s-M-t") '(lambda ()
																 (interactive)
																 (any-html-tag "table"))) ; opt+cmd+t

;;; remove-html-tags
(defun remove-html-tags (tag)
	"Remove html tags.  TAG is given interactivelly."
	(interactive "sTag (1:all, 2:famous block, 3:form not text, 4:img not text, 5:ruby, tag:specify tag): ")
	(cond
	 ;; all
	 ((string-equal tag "1") (progn
														 (replace-strings-in-region-by-list
															'(("<.+?>" . "")))
														 (message "remove all tags")))
	 ;; famous block tags
	 ((string-equal tag "2") (progn
														 (replace-strings-in-region-by-list
															'(("</*p.*?>\\|</*h[1-6].*?>\\|</*ul.*?>\\|</*ol.*?>\\|</*li.*?>\\|</*pre.*?>\\|</*dl.*?>\\|</*dt.*?>\\|</*dd.*?>\\|</*div.*?>\\|</*center.*?>\\|</*blockquote.*?>\\|</*address.*?>\\|</*table.*?>\\|</*tr.*?>\\|</*td.*?>\\|</*th.*?>\\|</*thead.*?>\\|</*section.*?>\\|</*header.*?>\\|</*footer.*?>\\|</*article.*?>" . "")))
														 (message "remove famous block tags")))
	 ;; form elements tag except for text
	 ((string-equal tag "3") (progn
														 (replace-strings-in-region-by-list
															'(("<label.*?>\\(.+?\\)</label>" . "\\1")))
														 (replace-strings-in-region-by-list
															'(("<option.*?>\\(.+?\\)</option>" . "\\1")))
														 (replace-strings-in-region-by-list
															'(("<input.*?value=\"\\(.+?\\)\".*?>" . "\\1")))
														 (replace-strings-in-region-by-list
															'(("<textarea.*?>\\(.+?\\)</textarea>" . "\\1")))
														 (message "remove form elements tag except for text")))
	 ;; img tag except for alt
	 ((string-equal tag "4") (progn
														 (replace-strings-in-region-by-list
															'(("<img.*?alt=\"\\(.+?\\)\".*?>" . "\\1")))
														 (message "remove form elements tag except for text")))
	 ;; ruby tag and ruby text
	 ;; "<ruby>(?:<rb>)*(.*?)(?:</rb>)*(?:<rp>.*?</rp>)*<rt>.+?</rt>(?:<rp>.*?</rp>)*</ruby>"
	 ((string-equal tag "5") (progn
														 (replace-strings-in-region-by-list
															'(("<ruby>\\(.+?\\)<rt>.+?<rt></ruby>" . "\\1")))
														 (message "remove ruby tag and ruby text")))
	 ;; specify tag
	 (t (progn
				;; (replace-strings-in-region-by-list
				;;  '(((concat "</*" "span" ".*?>") . "")))
				(let* ((beg (region-beginning))
							 (end (region-end))
							 (word (buffer-substring-no-properties beg end)))
					(setq word (replace-regexp-in-string (concat "</*" tag ".*?>") "" word))
					(delete-region beg end)
					(insert word))
				(message "remove specified tag")))))
(global-set-key (kbd "M-s-r") 'remove-html-tags) ; opt+cmd+r

;;; ------------------------------------------------------------
;;; Shift+Returnで<br />を入力
(global-set-key [S-return] "<br />")

;;; ------------------------------------------------------------
;;; HTML:タグとタグの間、またはタグ内を一気に選択
(defun region-angle-brackets ()
	"Select region angle brackets."
	(interactive)
	(let (pt)
		(skip-chars-backward "^<>")
		(setq pt (point))
		(skip-chars-forward "^<>")
		(set-mark pt)))
(global-set-key (kbd "s-A") 'region-angle-brackets) ; cmd+shift+a

;;; ------------------------------------------------------------
;;; Provide

(provide 'web-authoring-set)

;;; web-authoring-set.el ends here
