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
;; -cmd+opt+f: figure + figcaption
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

;;; Code:

(require 'subr-x)

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
     ((string= type "php")
      (setq ret (concat "echo '<textarea style=\"width:100%;height:200px;background-color:#fff;color:#111;font-size:90%;font-family:monospace;position:relative;z-index:9999\">';\nvar_dump(" word ");\necho '</textarea>';\ndie();")
            cursor 34)))

    ;; ip
    (if (string= ip "") nil
      (setq ret (concat "//if ($_SERVER['HTTP_X_FORWARDED_FOR'] == '" ip "'){\nif ($_SERVER['REMOTE_ADDR'] == '" ip "'){\n" ret "\n}")
            cursor 36))

    ;; put val
    (if mark-active (delete-region beg end) nil)
    (insert ret)
    (goto-char (- (point) cursor))))

;; php without ip
(global-set-key (kbd "s-M-z")
                (lambda () (interactive)
                  (dump-values "php" ""))) ; cmd+opt+z
(global-set-key (kbd "s-M-ω")
                (lambda () (interactive)
                  (dump-values "php" "")))
(global-set-key (kbd "C-c h z")
                (lambda () (interactive)
                  (dump-values "php" ""))) ; cmd+opt+z

;; php with ip
(global-set-key (kbd "s-M-Z")
                (lambda (ip) (interactive "sIP:")
                  (dump-values "php" ip))) ; cmd+opt+shift+z
(global-set-key (kbd "s-M-`")
                (lambda (ip) (interactive "sIP:")
                  (dump-values "php" ip))) ; cmd+opt+shift+z
(global-set-key (kbd "C-c h Z")
                (lambda (ip) (interactive "sIP:")
                  (dump-values "php" ip))) ; cmd+opt+shift+z

;; JavaScript
;;(global-set-key (kbd "s-M-z") (lambda () (interactive)
;;																(dump-values "javascript" ""))) ; cmd+opt+z

;;; ------------------------------------------------------------
;;; php open and close
(defun put-php-opener-closer (type)
  "Put php opner and closer.  choose TYPE."
  (interactive "nType 1:<?php ?>, 2:<?php(RET)?>, 3:?><?php:, 4:<?= ?>:")
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
     ((eq type 4)
      (setq ret (concat "<?= " word " ?>")
            cursor -3))
     (t
      (if (/= (length word) 0) (setq word (concat word " ")) word)
      (setq ret (concat "<?php " word " ?>")
            cursor -3)))
    (if mark-active (delete-region beg end) nil)
    (insert ret)
    (goto-char (+ (point) cursor))))
(global-set-key (kbd "s-M-h") 'put-php-opener-closer) ; cmd+shift+h
(global-set-key (kbd "s-M-˙") 'put-php-opener-closer) ; cmd+shift+h
(global-set-key (kbd "C-c h h") 'put-php-opener-closer)

;;; ------------------------------------------------------------
;;; 編集中の文書がXHTMLかどうかを判定する
(defun is-xhtml ()
  "Is xhtml."
  (interactive)
  (string-match
   "\\(DTD XHTML\\|/>\\)"
   (buffer-substring-no-properties (point-min) (point-max))))

;;; ------------------------------------------------------------
;;; 任意のタグ
;;; ミニバッファにタグを入れると基本的には選択範囲を囲むタグを生成する
;;; タグに応じて、いくらか振る舞いが変わる
(declare-function convert-to-th "convert-to-th" (arg))
(declare-function convert-to-td "convert-to-td" (arg))
(declare-function anything-old-completing-read "anything-old-completing-read" (arg1 arg2))
(defun any-html-tag (tag)
  "Insert html intaractive.  TAG."
  (interactive "i")
  (let* ((beg (if (region-active-p) (region-beginning) (point)))
         (end (when (region-active-p) (region-end)))
         (word (if (region-active-p) (buffer-substring-no-properties beg end) ""))
         (cursor- 0)
         (cursor+ 0)
         (close-tag (if (is-xhtml) " />" ">"))
         (eob (if (string-match "\n$" word) "\n" ""))
         cursor
         url
         ruby
         type
         html
         lines
         line
         cnt)

    ;; use with completing
    ;; (unless tag
    ;;   (setq tag (let ((completing-read-function 'completing-read-default))
    ;;               (anything-old-completing-read
    ;;                "Tag (default \"div\"): "
    ;;                '(("table", "table")
    ;;                  ("section", "section")
    ;;                  ("header", "header")
    ;;                  ("footer", "footer")
    ;;                  ("aside", "aside")
    ;;                  ("article", "article")
    ;;                  ("select", "select")
    ;;                  ("script", "script")
    ;;                  ("style", "style")
    ;;                  ("input", "input")
    ;;                  ("form", "form")
    ;;                  ("label", "label")
    ;;                  ("ruby", "ruby")
    ;;                  ("textarea", "textarea"))))))

    (unless tag (setq tag (read-string "tag: ")))

    (cond
     ;; anchor
     ((string= tag "a")
      (setq url (read-string "url: " nil)
            tag (concat "<a href=\"" url "\">" word "</a>")
            cursor- -4))

     ;; anchor-url, mailto, markdown
     ((string= tag "a-url")
      (if (string-match "@" word)
          (setq tag (concat "<a href=\"mailto:" word "\">" word "</a>"))
        (if (string-match "\\[.+?\\]\\(.+?\\)" word)
          (setq tag (replace-regexp-in-string "\\[\\(.+?\\)\\](\\(.+?\\))" "<a href=\"\\2\">\\1</a>" word))
            (setq tag (concat "<a href=\"" word "\">" word "</a>")))))

     ;; figure
     ((string= tag "figure")
      (setq tag (concat "<figure>\n" word "\n<figcaption></figcaption>\n</figure>")
            cursor- -23))

     ;; input
     ((string= tag "input")
      (setq type (read-number "type (1:text, 2:hidden, 3:radio, 4:checkbox, 5:submit, 6:password, 7:image, 8:file): " nil)
            cursor- -4)
      (cond
       ((eq type 1)
        (setq tag (concat "<input type=\"text\" name=\"str\" id=\"str\" size=\"20\" value=\"\""
                          close-tag)))
       ((eq type 2)
        (setq tag (concat "<input type=\"hidden\" name=\"str\" id=\"str\" value=\"\""
                          close-tag)))
       ((eq type 3)
        (setq tag (concat "<input type=\"radio\" name=\"str\" id=\"str\" value=\"\""
                          close-tag)))
       ((eq type 4)
        (setq tag (concat "<input type=\"checkbox\" name=\"str\" id=\"str\" value=\"\""
                          close-tag)))
       ((eq type 5)
        (setq tag (concat "<input type=\"submit\" name=\"str\" id=\"str\" value=\"\""
                          close-tag)))
       ((eq type 6)
        (setq tag (concat "<input type=\"password\" name=\"str\" id=\"str\" value=\"\""
                          close-tag)))
       ((eq type 7)
        (setq tag (concat "<input type=\"image\" name=\"str\" id=\"str\" alt=\"\" value=\"\""
                          close-tag)))
       ((eq type 8)
        (setq tag (concat "<input type=\"file\" name=\"str\" id=\"str\" value=\"\""
                          close-tag)))))

     ;; singular tag - hr, br
     ((find tag '("hr" "br") :test #'string=)
      (setq tag (concat "<" tag close-tag)))

     ;; singular tag - img
     ((string= tag "img")
      (message "%s" close-tag)
      (setq tag (concat "<img src=\"" word "\" alt=\"\"" close-tag)
            cursor- (if (is-xhtml) -4 -2)))

     ;; ul-li, ol-li
     ((find tag '("ul-li" "ol-li") :test #'string=)
      (setq html ""
            lines (split-string word "\n")
            cursor+ 3)
      (while lines
        (if (string= (car lines) "") nil
          (progn (setq html (concat html "\t<li>" (car lines) "</li>\n"))))
        (setq lines (cdr lines)))
      (if (string= tag "ul-li")
          (setq tag (concat "<ul>\n" html "</ul>" eob))
        (setq tag (concat "<ol>\n" html "</ol>" eob))))

     ;; select
     ((string= tag "select")
      (setq html ""
            lines (split-string word "\n")
            cursor+ 7)
      (while lines
        (if (string= (car lines) "") nil
          (progn (setq html (concat html "\t<option>" (car lines) "</option>\n"))))
        (setq lines (cdr lines)))
      (setq tag (concat "<select name=\"nameStr\" id=\"idStr\">\n" html "</select>")))

     ;; p-each
     ((string= tag "p-each")
      ;; 選択範囲がなければ空のpを用意する
      (if (not mark-active)
          (setq tag "<p></p>"
                cursor+ 2)
        ;; 選択範囲があって、かつ<br>が含まれていたらeachしない
        (if (string-match "<br */*?>" (buffer-substring-no-properties beg end))
            (progn
              (when (string-equal eob "\n")
                (setq word (replace-regexp-in-string "\n+$" "" word)))
              (setq tag (concat "<p>" word "</p>" eob)
                    cursor+ 2))
          ;; <br>が含まれていないのでeach
          (setq lines (split-string word "\n"))
          (while lines
            (if (string= (car lines) "")
                (setq html (concat html "\n"))
              (setq html (concat html "<p>" (car lines) "</p>\n")))
            (setq lines (cdr lines)))
          (setq html (replace-regexp-in-string "\n+$" "" html))
          (setq tag (concat html eob)))))

     ;; li-each
     ((string= tag "li-each")
      (setq html ""
            lines (split-string word "\n"))
      (while lines
        (if (string= (car lines) "") nil
          (progn (setq html (concat html "\t<li>" (car lines) "</li>\n"))))
        (setq lines (cdr lines)))
      (setq html (replace-regexp-in-string "\n+$" "" html))
      (setq tag (concat html eob)))

     ;; table
     ((string= tag "table-intaractive")
      (setq type (read-string "type (1:th, 2:thead, 3:th and thead, 4:no headers): " nil))
      (setq html ""
            cnt 1
            lines (split-string word "\n")
            cursor+ 6)
      (while lines
        (if (string= (car lines) "") nil
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
      (setq tag (concat "<table>\n" html "</table>" eob))
      (if (find type '("1" "3") :test #'string=)
          (setq tag (replace-regexp-in-string "<tr>\n\t<td>\\(.+?\\)</td>" "<tr>\n\t<th>\\1</th>" tag))))

     ;; dl
     ((string= tag "dl-dt-dd")
      (setq lines (split-string word "\n")
            cursor+ 3)
      (while lines
        (if (string= (car lines) "") nil
          (progn
            (if (string-match "\t" word)
                (setq line (concat "<dt>"
                                   (replace-regexp-in-string "\t" "</dt>\n\t<dd>" (car lines))
                                   "</dd>\n"))
              (setq line (concat "<dt>" (car lines) "</dt>\n"))))
        (setq html (concat html line)))
        (setq lines (cdr lines)))
      (setq tag (concat "<dl>\n" html "</dl>" eob)))

     ;; comment out
     ((string= tag "comment-out")
      (setq tag (concat "<!-- " word " -->")
            cursor+ 5))

     ;; ruby
     ((string= tag "ruby-intaractive")
      (setq ruby (read-string "ruby: " nil)
            tag (concat "<ruby><rb>" word "</rb><rt>" ruby "</rt></ruby>")
            cursor+ 10))

     ;; script
     ((string= tag "script")
      (setq tag (concat "<script>\n" word "\n</script>")
            cursor- -12))

     ;; style
     ((string= tag "style")
      (setq tag (concat "<style type=\"text/css\">\n" word "\n</style>")
            cursor- -10))

     ;; form
     ((string= tag "form")
      (setq tag (concat "<form action=\"str\" method=\"POST\" enctype=\"multipart/form-data\">\n" word "\n</form>")
            cursor+ 5))

     ;; textarea
     ((string= tag "textarea")
      (setq tag (concat "<textarea name=\"str\" id=\"str\" cols=\"35\" rows=\"7\">\n" word "\n</textarea>")
            cursor+ 9))

     ;; label
     ((string= tag "label")
      (setq tag (concat "<label for=\"str\">" word "</label>")
            cursor+ 12))

     ;; specify tag
     (t (when (string= tag "") (setq tag "div"))
        (setq cursor+ (if (region-active-p) (+ 1 (length tag)) (+ 2 (length tag))))
        (when (string-equal eob "\n")
            (setq word (replace-regexp-in-string "\n+$" "" word)))
        (setq tag (concat "<" tag ">" word "</" tag ">" eob))))

    ;; put tags
    (when (region-active-p) (delete-region beg end))
    (insert tag)

    ;; goto-char
    (if (> cursor+ 1)
        (setq cursor (+ beg cursor+))
      (setq cursor (+ (point) cursor-)))
    (goto-char cursor)))
(global-set-key (kbd "s-M-v") 'any-html-tag)
(global-set-key (kbd "s-M-√") 'any-html-tag)
(global-set-key (kbd "C-c h v") 'any-html-tag)

;;; ------------------------------------------------------------
;;; ブラケット
(defun sand-brackets (tag)
  "Insert html intaractive.  TAG."
  (interactive "i")
  (let* ((beg (if (region-active-p) (region-beginning) (point)))
         (end (when (region-active-p) (region-end)))
         (word (if (region-active-p) (buffer-substring-no-properties beg end) "")))
    (unless tag (setq tag (read-string "tag: ")))
    (setq tag (concat "[" tag "]" word "[/" tag "]"))
    (when (region-active-p) (delete-region beg end))
    (insert tag)))
(global-set-key (kbd "s-M-b") 'sand-brackets)
(global-set-key (kbd "s-M-∫") 'sand-brackets)
(global-set-key (kbd "C-c h b") 'sand-brackets)

;;; ------------------------------------------------------------
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
(global-set-key (kbd "s-M-1") 'h1-tag)
(global-set-key (kbd "s-M-2") 'h2-tag)
(global-set-key (kbd "s-M-3") 'h3-tag)
(global-set-key (kbd "s-M-4") 'h4-tag)
(global-set-key (kbd "s-M-5") 'h5-tag)
(global-set-key (kbd "s-M-6") 'h6-tag)
(global-set-key [M-s-kp-1] 'h1-tag)
(global-set-key [M-s-kp-2] 'h2-tag)
(global-set-key [M-s-kp-3] 'h3-tag)
(global-set-key [M-s-kp-4] 'h4-tag)
(global-set-key [M-s-kp-5] 'h5-tag)
(global-set-key [M-s-kp-6] 'h6-tag)
(global-set-key (kbd "s-M-¡") 'h1-tag)
(global-set-key (kbd "s-M-™") 'h2-tag)
(global-set-key (kbd "s-M-£") 'h3-tag)
(global-set-key (kbd "s-M-¢") 'h4-tag)
(global-set-key (kbd "s-M-∞") 'h5-tag)
(global-set-key (kbd "s-M-§") 'h6-tag)
(global-set-key (kbd "C-c h 1") 'h1-tag)
(global-set-key (kbd "C-c h 2") 'h2-tag)
(global-set-key (kbd "C-c h 3") 'h3-tag)
(global-set-key (kbd "C-c h 4") 'h4-tag)
(global-set-key (kbd "C-c h 5") 'h5-tag)
(global-set-key (kbd "C-c h 6") 'h6-tag)

;;; a
(global-set-key (kbd "s-M-a")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "a")))
(global-set-key (kbd "s-M-å")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "a")))
(global-set-key (kbd "C-c h a")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "a")))

;;; a-url
(global-set-key (kbd "s-M-A")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "a-url")))
(global-set-key (kbd "C-c h A")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "a-url")))

;;; comment-out
(global-set-key (kbd "s-M-c")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "comment-out")))
(global-set-key (kbd "s-M-ç")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "comment-out")))
(global-set-key (kbd "C-c h c")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "comment-out")))

;;; dl-dt-dd
(global-set-key (kbd "s-M-d")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "dl-dt-dd")))
(global-set-key (kbd "s-M-∂")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "dl-dt-dd")))
(global-set-key (kbd "C-c h d")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "dl-dt-dd")))

;;; img
(global-set-key (kbd "s-M-i")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "img")))
(global-set-key (kbd "s-M-ˆ")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "img")))
(global-set-key (kbd "C-c h i")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "img")))

;;; em
(global-set-key (kbd "s-M-e")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "em")))
(global-set-key (kbd "s-M-´")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "em")))
(global-set-key (kbd "C-c h e")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "em")))

;;; figure + figcaption
(global-set-key (kbd "s-M-f")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "figure")))
(global-set-key (kbd "C-c h f")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "figure")))

;;; strong
(global-set-key (kbd "s-M-g")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "strong")))
(global-set-key (kbd "s-M-©")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "strong")))
(global-set-key (kbd "C-c h g")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "strong")))

;;; li-each
(global-set-key (kbd "s-M-l")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "li-each")))
(global-set-key (kbd "s-M-¬")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "li-each")))
(global-set-key (kbd "C-c h l")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "li-each")))

;;; ol-li
(global-set-key (kbd "s-M-o")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ol-li")))
(global-set-key (kbd "s-M-ø")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ol-li")))
(global-set-key (kbd "C-c h o")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ol-li")))

;;; p-each
(global-set-key (kbd "s-M-p")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "p-each")))
(global-set-key (kbd "s-M-0")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "p-each")))
(global-set-key (kbd "s-M-π")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "p-each")))
(global-set-key (kbd "C-c h p")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "p-each")))

;;; blockquote
(global-set-key (kbd "s-M-q")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "blockquote")))
(global-set-key (kbd "s-M-œ")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "blockquote")))
(global-set-key (kbd "C-c h q")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "blockquote")))

;;; span
(global-set-key (kbd "s-M-s")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "span")))
(global-set-key (kbd "s-M-ß")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "span")))
(global-set-key (kbd "C-c h s")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "span")))

;;; table
(global-set-key (kbd "s-M-t")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "table-intaractive")))
(global-set-key (kbd "s-M-†")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "table-intaractive")))
(global-set-key (kbd "C-c h t")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "table-intaractive")))

;;; ul-li
(global-set-key (kbd "s-M-u")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ul-li")))
(global-set-key (kbd "s-M-¨")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ul-li")))
(global-set-key (kbd "C-c h u")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ul-li")))

;;; ruby
(global-set-key (kbd "s-M-y")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ruby-intaractive")))
(global-set-key (kbd "s-M-¥")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ruby-intaractive")))
(global-set-key (kbd "C-c h y")
                #'(lambda ()
                   (interactive)
                   (any-html-tag "ruby-intaractive")))

;;; remove-html-tags
(defun remove-html-tags (tag)
  "Remove html tags.  TAG is given interactivelly."
  (interactive "sTag (1:all, 2:famous block, 3:form not text, 4:img not text, 5:ruby, 6:unhtmlize, tag:specify tag): ")
  (cond
   ;; all
   ((string= tag "1") (progn
                             (replace-strings-in-region-by-list
                              '(("<.+?>" . "")))
                             (message "remove all tags")))
   ;; famous block tags
   ((string= tag "2") (progn
                             (replace-strings-in-region-by-list

'(("</*p.*?>\\|</*h[1-6].*?>\\|</*ul.*?>\\|</*ol.*?>\\|</*li.*?>\\|</*pre.*?>\\|</*dl.*?>\\|</*dt.*?>\\|</*dd.*?>\\|</*div.*?>\\|</*center.*?>\\|</*blockquote.*?>\\|</*address.*?>\\|</*table.*?>\\|</*tr.*?>\\|</*td.*?>\\|</*th.*?>\\|</*thead.*?>\\|</*section.*?>\\|</*header.*?>\\|</*footer.*?>\\|</*article.*?>" . "")))
                             (message "remove famous block tags")))
   ;; form elements tag except for text
   ((string= tag "3") (progn
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
   ((string= tag "4") (progn
                             (replace-strings-in-region-by-list
                              '(("<img.*?alt=\"\\(.+?\\)\".*?>" . "\\1")))
                             (message "remove form elements tag except for text")))
   ;; ruby tag and ruby text
   ;; "<ruby>(?:<rb>)*(.*?)(?:</rb>)*(?:<rp>.*?</rp>)*<rt>.+?</rt>(?:<rp>.*?</rp>)*</ruby>"
   ((string= tag "5") (progn
                             (replace-strings-in-region-by-list
                              '(("<ruby>\\(.+?\\)<rt>.+?<rt></ruby>" . "\\1")))
                             (message "remove ruby tag and ruby text")))
   ;; unhtmlize
   ((string= tag "6") (progn
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
(global-set-key (kbd "M-s-®") 'remove-html-tags) ; opt+cmd+r
(global-set-key (kbd "C-c h r") 'remove-html-tags)

;;; ------------------------------------------------------------
;;; Shift+Returnで<br />を入力
(global-set-key [S-return] (lambda () (interactive) (insert (if (is-xhtml) "<br />" "<br>"))))

;;; ------------------------------------------------------------
;;; Provide

(provide 'web-authoring-set)

;;; web-authoring-set.el ends here
