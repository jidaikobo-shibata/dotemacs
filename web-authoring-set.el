;;; web-authoring-set.el --- jidaikobo web authoring
;; Copyright (C) 2016 by jidaikobo-shibata
;; Author: jidaikobo
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;;; ------------------------------------------------------------
;; 時代工房のウェブ制作用キーバインド集
;; 基本的にはcmd+opt+(M-s-)となにか一文字で操作する

;; cmd+opt+v: 任意のタグ
;; -cmd+opt+1: h1
;; -cmd+opt+2: h2
;; -cmd+opt+3: h3
;; -cmd+opt+4: h4
;; -cmd+opt+5: h5
;; -cmd+opt+6: h6
;; -cmd+opt+d: dl
;; -cmd+opt+a: anchor
;; -cmd+opt+shift+5: anchor self
;; -cmd+opt+c: comment out
;; -cmd+opt+d: dl
;; -cmd+opt+e: em
;; -cmd+opt+g: strong
;; -cmd+opt+l: li each
;; -cmd+opt+p: p each lines (semi-auto)
;; -cmd+opt+shift+p: p
;; -cmd+opt+q: blockquote
;; -cmd+opt+o: ol
;; -cmd+opt+s: span
;; -cmd+opt+t: table intaractive
;; -cmd+opt+u: ul
;; -cmd+opt+y: ruby intaractive
;;
;; cmd+shift+a: タグを選択
;; cmd+RET: <br />を入力
;; cmd+opt+r: タグを削除 intaractive
;; cmd+opt+shift+r: タグを実体参照（or タグ）に
;;
;; cmd+opt+z: var_dump()
;; cmd+opt+shift+z: var_dump()でipを指定
;; cmd+opt+k: 編集中のファイルのカレントパス

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
         (word (if mark-active (buffer-substring-no-properties beg end) ""))
         ret
         cursor)
    (when (string= word "") (setq word "$vals"))
    (cond
     ;; php
     ((string-equal type "php")
      (setq ret (concat "echo '<textarea style=\"width:100%;height:200px;background-color:#fff;color:#111;font-size:90%;font-family:monospace;position:relative;z-index:9999\">';\nvar_dump(" word ");\necho '</textarea>';\ndie();\n")
            cursor 35)))

    ;; ip
    (if (string-equal ip "") nil
      (setq ret (concat "if( $_SERVER['REMOTE_ADDR'] == '" ip "' ){\n" ret "\n}\n")
            cursor 37))

    ;; put val
    (if mark-active (delete-region beg end) nil)
    (insert ret)
    (goto-char (- (point) cursor))))

;; php without ip
(global-set-key (kbd "s-M-z")
                (lambda () (interactive)
                  (dump-values "php" ""))) ; cmd+opt+z
(global-set-key (kbd "s-M-@")
                (lambda () (interactive)
                  (dump-values "php" "")))
;; php with ip
(global-set-key (kbd "s-M-Z")
                (lambda (ip) (interactive "sIP:")
                  (dump-values "php" ip))) ; cmd+opt+shift+z
(global-set-key (kbd "s-M-`")
                (lambda (ip) (interactive "sIP:")
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
         (cursor 0)
         (word (if mark-active (buffer-substring-no-properties beg end) ""))
         (ret))
    (cond
     ((eq type 2)
      (setq ret (concat "<?php\n" word "\n?>")
            cursor -3))
     ((eq type 3)
      (setq ret (concat "?>" word "<?php")
            cursor -5))
     (t
      (if (/= (length word) 0) (setq word (concat word " ")) word)
      (setq ret (concat "<?php " word " ?>")
            cursor -3)))
    (if mark-active (delete-region beg end) nil)
    (insert ret)
    (goto-char (+ (point) cursor))))
(global-set-key (kbd "s-M-h") 'put-php-opener-closer) ; cmd+shift+h

;;; ------------------------------------------------------------
;;; 任意のタグ
;;; ミニバッファにタグを入れると基本的には選択範囲を囲むタグを生成する
;;; タグに応じて、いくらか振る舞いが変わる
(declare-function convert-to-th "convert-to-th" (arg))
(declare-function convert-to-td "convert-to-td" (arg))
(defun any-html-tag (tag)
  "Insert html intaractive.  TAG."
  (interactive "i")
  (let* ((beg (if (region-active-p) (region-beginning) (point)))
         (end (when (region-active-p) (region-end)))
         (word (if (region-active-p) (buffer-substring-no-properties beg end) ""))
         (cursor- 0)
         (cursor+ 0)
         cursor
         url
         ruby
         type
         html
         lines
         line
         cnt)

    ;; use with completing
    (unless tag
      (setq tag (completing-read
                 "Tag (default \"div\"): "
                 '(("table", "table")
                   ("section", "section")
                   ("header", "header")
                   ("footer", "footer")
                   ("aside", "aside")
                   ("article", "article")
                   ("select", "select")
                   ("script", "script")
                   ("style", "style")
                   ("input", "input")
                   ("form", "form")
                   ("label", "label")
                   ("ruby", "ruby")
                   ("textarea", "textarea")))))

    (cond
     ;; anchor
     ((string-equal tag "a")
      (setq url (read-string "url: " nil 'my-history)
            tag (concat "<a href=\"" url "\">" word "</a>")
            cursor- -4))

     ;; anchor-url
     ((string-equal tag "a-url")
      (if (string-match "@" word)
          (setq tag (concat "<a href=\"mailto:" word "\">" word "</a>")
                cursor- -4)
        (setq tag (concat "<a href=\"" word "\">" word "</a>")
              cursor- -4)))

     ;; input
     ((string-equal tag "input")
      (setq type (read-number "type (1:text, 2:hidden, 3:radio, 4:checkbox, 5:submit, 6:password, 7:image, 8:file): " nil)
            cursor- -4)
      (cond
       ((eq type 1)
        (setq tag "<input type=\"text\" name=\"str\" id=\"str\" size=\"20\" value=\"\" />"))
       ((eq type 2)
        (setq tag "<input type=\"hidden\" name=\"str\" id=\"str\" value=\"\" />"))
       ((eq type 3)
        (setq tag "<input type=\"radio\" name=\"str\" id=\"str\" value=\"\" />"))
       ((eq type 4)
        (setq tag "<input type=\"checkbox\" name=\"str\" id=\"str\" value=\"\" />"))
       ((eq type 5)
        (setq tag "<input type=\"submit\" name=\"str\" id=\"str\" value=\"\" />"))
       ((eq type 6)
        (setq tag "<input type=\"password\" name=\"str\" id=\"str\" value=\"\" />"))
       ((eq type 7)
        (setq tag "<input type=\"image\" name=\"str\" id=\"str\" value=\"\" />"))
       ((eq type 8)
        (setq tag "<input type=\"file\" name=\"str\" id=\"str\" value=\"\" />"))))

     ;; singular tag - hr, br
     ((find tag '("hr" "br") :test #'string=)
      (setq tag (concat "<" tag " />")))

     ;; singular tag - img
     ((string-equal tag "img")
      (setq tag (concat "<img src=\"" word "\" alt=\"\" />")
            cursor- -4))

     ;; ul-li, ol-li
     ((find tag '("ul-li" "ol-li") :test #'string=)
      (setq html ""
            lines (split-string word "\n")
            cursor+ 3)
      (while lines
        (if (string-equal (car lines) "") nil
          (progn (setq html (concat html "\t<li>" (car lines) "</li>\n"))))
        (setq lines (cdr lines)))
      (if (string-equal tag "ul-li")
          (setq tag (concat "<ul>\n" html "</ul>\n"))
        (setq tag (concat "<ol>\n" html "</ol>\n"))))

     ;; select
     ((string-equal tag "select")
      (setq html ""
            lines (split-string word "\n")
            cursor+ 7)
      (while lines
        (if (string-equal (car lines) "") nil
          (progn (setq html (concat html "\t<option>" (car lines) "</select>\n"))))
        (setq lines (cdr lines)))
      (setq tag (concat "<select name=\"nameStr\" id=\"idStr\">\n" html "</select>\n")))

     ;; p-each
     ((string-equal tag "p-each")
      ;; <br>が含まれていたらeachしない
      (if (string-match
           "<br>\\|<br />\\|<br/>"
           (buffer-substring-no-properties (region-beginning)
                                           (region-end)))
          (setq tag (concat "<p>" word "</p>")
                cursor+ 2)
        ;; <br>が含まれていないのでeach
        (setq html ""
              lines (split-string word "\n"))
        (while lines
          (if (string-equal (car lines) "") nil
            (progn (setq html (concat html "<p>" (car lines) "</p>\n"))))
          (setq lines (cdr lines)))
        (setq tag html)))

     ;; li-each
     ((string-equal tag "li-each")
      (setq html ""
            lines (split-string word "\n"))
      (while lines
        (if (string-equal (car lines) "") nil
          (progn (setq html (concat html "<li>" (car lines) "</li>\n"))))
        (setq lines (cdr lines)))
      (setq tag html))

     ;; table
     ((string-equal tag "table")
      (setq type (read-string "type (1:th, 2:thead, 3:th and thead, 4:no headers): " nil 'my-history))
      (setq html ""
            cnt 1
            lines (split-string word "\n")
            cursor+ 6)
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
      (setq lines (split-string word "\n")
            cursor+ 3)
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
      (setq tag (concat "<!-- " word " -->")
            cursor+ 5))

     ;; ruby
     ((string-equal tag "ruby")
      (setq ruby (read-string "ruby: " nil 'my-history)
            tag (concat "<ruby><rb>" word "</rb><rt>" ruby "</rt></ruby>")
            cursor+ 10))

     ;; script
     ((string-equal tag "script")
      (setq tag (concat "<script type=\"text/javascript\">\n<!--\n" word "\n// -->\n</script>\n")
            cursor- -18))

     ;; style
     ((string-equal tag "style")
      (setq tag (concat "<style type=\"text/css\">\n" word "\n</style>\n")
            cursor- -10))

     ;; form
     ((string-equal tag "form")
      (setq tag (concat "<form action=\"str\" method=\"POST\" enctype=\"multipart/form-data\">\n" word "\n</form>\n")
            cursor+ 5))

     ;; textarea
     ((string-equal tag "textarea")
      (setq tag (concat "<textarea name=\"str\" id=\"str\" cols=\"35\" rows=\"7\">\n" word "\n</textarea>\n")
            cursor+ 9))

     ;; label
     ((string-equal tag "label")
      (setq tag (concat "<label for=\"str\">\n" word "\n</label>\n")
            cursor+ 13))

     ;; specify tag
     (t (when (string-equal tag "") (setq tag "div"))
        (setq cursor+ (if (region-active-p) (+ 1 (length tag)) (+ 2 (length tag))))
        (setq tag (concat "<" tag ">" word "</" tag ">"))))

    ;; put tags
    (when (region-active-p) (delete-region beg end))
    (insert tag)

    ;; goto-char
    (if (> cursor+ 1)
        (setq cursor (+ beg cursor+))
      (setq cursor (+ (point) cursor-)))
    (goto-char cursor)))
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
(global-set-key (kbd "s-M-a")
                '(lambda ()
                   (interactive)
                   (any-html-tag "a"))) ; opt+cmd+a

;;; a-url
(global-set-key (kbd "s-M-A")
                '(lambda ()
                   (interactive)
                   (any-html-tag "a-url"))) ; opt+cmd+shif+a
;;; comment-out
(global-set-key (kbd "s-M-c")
                '(lambda ()
                   (interactive)
                   (any-html-tag "comment-out"))) ; opt+cmd+c
;;; dl-dt-dd
(global-set-key (kbd "s-M-d")
                '(lambda ()
                   (interactive)
                   (any-html-tag "dl-dt-dd"))) ; opt+cmd+d

;;; em
(global-set-key (kbd "s-M-e")
                '(lambda ()
                   (interactive)
                   (any-html-tag "em"))) ; opt+cmd+e

;;; strong
(global-set-key (kbd "s-M-g")
                '(lambda ()
                   (interactive)
                   (any-html-tag "strong"))) ; opt+cmd+g

;;; li-each
(global-set-key (kbd "s-M-l")
                '(lambda ()
                   (interactive)
                   (any-html-tag "li-each"))) ; opt+cmd+l

;;; ol-li
(global-set-key (kbd "s-M-o")
                '(lambda ()
                   (interactive)
                   (any-html-tag "ol-li"))) ; opt+cmd+o

;;; p-each
(global-set-key (kbd "s-M-p")
                '(lambda ()
                   (interactive)
                   (any-html-tag "p-each"))) ; opt+cmd+p

;;; p
(global-set-key (kbd "s-M-P")
                '(lambda ()
                   (interactive)
                   (any-html-tag "p"))) ; opt+cmd+P

;;; blockquote
(global-set-key (kbd "s-M-q")
                '(lambda ()
                   (interactive)
                   (any-html-tag "blockquote"))) ; opt+cmd+q

;;; span
(global-set-key (kbd "s-M-s")
                '(lambda ()
                   (interactive)
                   (any-html-tag "span"))) ; opt+cmd+s
;;; table
(global-set-key (kbd "s-M-t")
                '(lambda ()
                   (interactive)
                   (any-html-tag "table"))) ; opt+cmd+t

;;; ul-li
(global-set-key (kbd "s-M-u")
                '(lambda ()
                   (interactive)
                   (any-html-tag "ul-li"))) ; opt+cmd+u

;;; ruby
(global-set-key (kbd "s-M-y")
                '(lambda ()
                   (interactive)
                   (any-html-tag "ruby"))) ; opt+cmd+y

;;; remove-html-tags
(defun remove-html-tags (tag)
  "Remove html tags.  TAG is given interactivelly."
  (interactive "sTag (1:all, 2:famous block, 3:form not text, 4:img not text, 5:ruby, 6:unhtmlize, tag:specify tag): ")
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
   ;; unhtmlize
   ((string-equal tag "6") (progn
                             (replace-strings-in-region-by-list
                              '(("<" . "&lt;")(">" . "&gt;")))
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
(global-set-key [S-return] "<br />")

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
;;; smartyのコメントアウトを実装
;; multiline fontlock
;; https://www.emacswiki.org/emacs/MultilineFontLock
;; MultilineRegexp
;; https://www.emacswiki.org/emacs/MultilineRegexp
;; http://d.hatena.ne.jp/osaboh/20070716/p3
;; http://emacs.stackexchange.com/questions/14887/matching-multiline-comments-in-regex

;; (defun font-lock-user-keywords (mode &optional keywords)
;;   "Add user highlighting to KEYWORDS to MODE.
;; See `font-lock-add-keywords' and `font-lock-defaults'."
;;   (unless mode
;;     (error "mode should be non-nil "))
;;   (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
;;   (font-lock-add-keywords mode keywords)
;;   (put mode 'font-lock-user-keywords keywords))

;; (font-lock-user-keywords
;;  'html-mode
;;  '(("!" . font-lock-warning-face)
;;    ;; ("<{\\*\\(.\\|\\)*?\\*}>" . font-lock-comment-face)
;;    ("<{\\*[[:ascii:][:nonascii:]\"]*?\\*}>" . font-lock-comment-face)
;; ))
;; (font-lock-user-keywords 'html-mode)

;; (add-hook 'html-mode-hook
;; 					'(lambda()
;; 						(font-lock-add-keywords nil '(("[:ascii:]" . font-lock-comment-face)))))

;; (defun xoops-smarty-comment-setting ()
;; 	(make-local-variable 'comment-start)
;; 	(setq comment-start "<{\*")
;; 	(make-local-variable 'comment-end)
;; 	(setq comment-end "\*}>")
;; 	(make-local-variable 'comment-multi-line)
;; 	(setq comment-multi-line t))
;; (add-hook 'html-mode-hook 'xoops-smarty-comment-setting)

;; (add-hook 'html-mode-hook 'html-comment-syntax)

;; (defun html-comment-syntax ()
;;   (setq-local syntax-propertize-function 'html-comment-propertize-function))

;; (defun html-comment-propertize-function (begin end)
;; 	(funcall
;; 	 (syntax-propertize-rules
;; 		("\\(<\\){\\*" (1 "< b"))
;; 		("\*}[ \t\n]*\\(>\\)" (1 "> b"))
;; 		("\"" (0 (if (prog1 (zerop (car (syntax-ppss (match-beginning 0))))
;; 									 (goto-char (match-end 0)))
;; 								 (string-to-syntax ".")))))
;; 	 begin
;; 	 end))

;;; ------------------------------------------------------------
;;; Provide

(provide 'web-authoring-set)

;;; web-authoring-set.el ends here
