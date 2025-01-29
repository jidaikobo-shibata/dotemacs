;;; markdown.init.el --- init for markdown
;;; Commentary:
;; provide markdown.init.
;;; Code:

(defvar md-mode-map (make-sparse-keymap)
  "Keymap for `md-mode`.")

(easy-mmode-define-minor-mode md-mode
  "This is a custom mode for enhanced Markdown display."
  :init-value nil
  :lighter "MD"
  :keymap md-mode-map)

;; キーバインドの設定

(defun my-insert-markdown-h1 ()
  "Insert a Markdown H1 heading."
  (interactive)
  (my-insert-markdown-heading 1))

(defun my-insert-markdown-h2 ()
  "Insert a Markdown H2 heading."
  (interactive)
  (my-insert-markdown-heading 2))

(defun my-insert-markdown-h3 ()
  "Insert a Markdown H3 heading."
  (interactive)
  (my-insert-markdown-heading 3))

(defun my-insert-markdown-h4 ()
  "Insert a Markdown H4 heading."
  (interactive)
  (my-insert-markdown-heading 4))

(defun my-insert-markdown-h5 ()
  "Insert a Markdown H5 heading."
  (interactive)
  (my-insert-markdown-heading 5))

(defun my-insert-markdown-h6 ()
  "Insert a Markdown H6 heading."
  (interactive)
  (my-insert-markdown-heading 6))

(defun my-insert-markdown-em ()
  "Insert a Markdown em."
  (interactive)
  (my-markdown-emphasis 1))

(defun my-insert-markdown-strong ()
  "Insert a Markdown strong."
  (interactive)
  (my-markdown-emphasis 2))

(defun my-insert-markdown-strong-em ()
  "Insert a Markdown strong em."
  (interactive)
  (my-markdown-emphasis 3))

(define-key md-mode-map (kbd "s-M-1") 'my-insert-markdown-h1)
(define-key md-mode-map (kbd "<s-M-kp-1>") 'my-insert-markdown-h1)
(define-key md-mode-map (kbd "s-M-2") 'my-insert-markdown-h2)
(define-key md-mode-map (kbd "<s-M-kp-2>") 'my-insert-markdown-h2)
(define-key md-mode-map (kbd "s-M-3") 'my-insert-markdown-h3)
(define-key md-mode-map (kbd "<s-M-kp-3>") 'my-insert-markdown-h3)
(define-key md-mode-map (kbd "s-M-4") 'my-insert-markdown-h4)
(define-key md-mode-map (kbd "<s-M-kp-4>") 'my-insert-markdown-h4)
(define-key md-mode-map (kbd "s-M-5") 'my-insert-markdown-h5)
(define-key md-mode-map (kbd "<s-M-kp-5>") 'my-insert-markdown-h5)
(define-key md-mode-map (kbd "s-M-6") 'my-insert-markdown-h6)
(define-key md-mode-map (kbd "<s-M-kp-6>") 'my-insert-markdown-h6)
(define-key md-mode-map (kbd "s-M-a") 'my-insert-markdown-link)
(define-key md-mode-map (kbd "s-M-i") 'my-insert-markdown-img)
(define-key md-mode-map (kbd "s-M-l") 'my-markdown-toggle-list)
(define-key md-mode-map (kbd "s-M-u") 'my-markdown-toggle-list)
(define-key md-mode-map (kbd "s-M-o") 'my-markdown-toggle-ordered-list)
(define-key md-mode-map (kbd "s-M-t") 'my-markdown-convert-table)
(define-key md-mode-map (kbd "s-M-e") 'my-insert-markdown-em)
(define-key md-mode-map (kbd "s-M-g") 'my-insert-markdown-strong)
(define-key md-mode-map (kbd "s-M-G") 'my-insert-markdown-strong-em)

;; my-insert-markdown-heading

(defun my-insert-markdown-heading (level)
  "Insert a Markdown heading of the specified LEVEL.
If a heading already exists, replace it with the specified level.
Move to the beginning of the line and adjust '#' characters.
If the line is empty, move the cursor to the end of the line."
  (interactive "p")  ; プレフィックス引数でレベルを指定
  (let ((is-empty-line (looking-at "^[[:space:]]*$")))  ; 空行かどうかを判定
    (save-excursion
      (beginning-of-line)  ; 行頭に移動
      ;; 行頭の`#+`を削除
      (when (looking-at "^#+\\s-*")  ; 行頭に`#`がある場合を確認
        (replace-match ""))          ; マッチ部分を削除
      ;; 指定レベルの`#`を挿入
      (insert (make-string level ?#))
      (insert " "))
    ;; 空行の場合は行末にカーソルを移動
    (when is-empty-line
      (end-of-line))))

;; my-markdown-toggle-list

(defun my-markdown-toggle-list ()
  "Insert `- ` at the beginning of the current line or selected lines in Markdown.
If no region is selected, move to the beginning of the line and insert `- `.
If multiple lines are selected, add `- ` at the beginning of each selected line."
  (interactive)
  (if (use-region-p)
      ;; 選択範囲がある場合
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          ;; 各行の先頭に`- `を追加
          (while (< (point) end)
            (beginning-of-line)
            (insert "- ")
            (forward-line 1))))
    ;; 選択範囲がない場合
    (progn
      (beginning-of-line) ; 行頭に移動
      (insert "- "))))    ; `- `を挿入

;; my-markdown-toggle-ordered-list

(defun my-markdown-toggle-ordered-list ()
  "Insert `1. ` at the beginning of the current line or numbered list for selected lines.
If no region is selected, move to the beginning of the line and insert `1. `.
If multiple lines are selected, number the selected lines sequentially."
  (interactive)
  (if (use-region-p)
      ;; 選択範囲がある場合
      (let ((start (region-beginning))
            (end (region-end))
            (counter 1)) ; 連番用カウンター
        (save-excursion
          (goto-char start)
          ;; 各行の先頭に連番を追加
          (while (< (point) end)
            (beginning-of-line)
            (if (not (looking-at "\\s-*$")) ; 空行でなければ
                (progn
                  (insert (format "%d. " counter)) ; 連番を挿入
                  (setq counter (1+ counter))))   ; カウンターをインクリメント
            (forward-line 1))))
    ;; 選択範囲がない場合
    (progn
      (beginning-of-line) ; 行頭に移動
      (insert "1. "))))   ; `1. `を挿入

;; my-insert-markdown-link

(defun my-insert-markdown-link ()
  "Insert a Markdown link or wrap selected text with one.
If there is a selected region, it wraps the text with a Markdown link.
If no region is selected, it inserts a default Markdown syntax."
  (interactive)
  (let ((url (read-string "Enter URL: "))) ; ミニバッファでURLを入力
    (if (use-region-p)
        ;; 選択範囲がある場合、選択範囲をアンカー記法でラップ
        (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (insert (format "[%s](%s)" selected-text url)))
      ;; 選択範囲がない場合、デフォルトのアンカー記法を挿入
      (insert (format "[linkText](%s)" url)))))

;; my-insert-markdown-img

(defun my-insert-markdown-img ()
  "Insert a Markdown image link or wrap selected text with one.
If there is a selected region, it wraps the text with a Markdown image link.
If no region is selected, it inserts a default image Markdown syntax."
  (interactive)
  (let ((url (read-string "Enter Image URL: "))) ; ミニバッファでURLを入力
    (if (use-region-p)
        ;; 選択範囲がある場合、選択範囲を画像記法でラップ
        (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (insert (format "![%s](%s)" selected-text url)))
      ;; 選択範囲がない場合、デフォルトの画像記法を挿入
      (insert (format "![alt](%s)" url)))))

;; my-markdown-convert-table

(defun my-markdown-convert-table ()
  "Convert tab-separated text into a Markdown table."
  (interactive)
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             ;; 選択範囲の最後が改行なら取り除く
             (selection (buffer-substring-no-properties start end))
             (selection (if (string-suffix-p "\n" selection)
                            (substring selection 0 -1)
                          selection))
             ;; 選択範囲を行ごとに分割
             (table-lines (split-string selection "\n"))
             ;; 各行をタブで分割して2次元リスト化
             (markdown-table (mapcar (lambda (line) (split-string line "\t")) table-lines))
             ;; 各列の最大幅を計算
             (max-column-widths
              (mapcar
               (lambda (col-index)
                 (apply #'max (mapcar
                               (lambda (row)
                                 (length (or (nth col-index row) "")))
                               markdown-table)))
               (number-sequence 0 (1- (length (car markdown-table)))))))
        (save-excursion
          ;; 選択範囲を置換
          (delete-region start end)
          ;; Markdownテーブルを生成
          (dolist (line markdown-table)
            (insert
             (concat "| "
                     (mapconcat
                      #'identity
                      (cl-mapcar
                       (lambda (col col-width)
                         (format (format "%%-%ds" col-width) (or col "")))
                       line max-column-widths)
                      " | ")
                     " |\n")))
          ;; ヘッダー用のセパレーターを挿入
          (goto-char start)
          (forward-line 1)
          (insert
           (concat "|-"
                   (mapconcat
                    (lambda (width) (make-string width ?-))
                    max-column-widths
                    "-|-")
                   "-|\n")))))
    (message "No region selected."))

;; my-markdown-emphasis

(defun my-markdown-emphasis (level)
  "Surround the active region with LEVEL asterisks (*).
If no region is active, insert the asterisks at the cursor position and place the cursor in between."
  (interactive "p") ; プレフィックス引数でLEVELを指定
  (let ((asterisks (make-string level ?*))) ; LEVELに基づいてアスタリスクを生成
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (insert asterisks)
            (goto-char start)
            (insert asterisks)))
      ;; If no region is selected
      (insert asterisks asterisks)
      (backward-char level))))

;; カラーリング

(defface my-strong-heading-face
  '((t :foreground "orange red" :weight bold))
  "Face for strong headings in md-mode.")

(add-hook 'md-mode-hook
          (lambda ()
            ;; 見出しのスタイル (それぞれのレベルに色を設定)
            (font-lock-add-keywords nil
                                    '(("^# .+" . 'font-lock-function-name-face)
                                      ("^## .+" . 'my-strong-heading-face)  ; 強い色に変更
                                      ("^### .+" . 'font-lock-keyword-face)
                                      ("^#### .+" . 'font-lock-variable-name-face) ; 弱めの色に変更
                                      ("^##### .+" . 'font-lock-string-face)
                                      ("^###### .+" . 'font-lock-constant-face)))
            ;; リスト項目（`-` または `+` で始まる行）
            (font-lock-add-keywords nil
                                    '(("^[ \t]*[-+*] " . 'font-lock-builtin-face)))
            ;; インラインコード (`code`)
            (font-lock-add-keywords nil
                                    '(("\\(`[^`\n]+`\\)" . 'font-lock-constant-face)))
            ;; コードブロック (```で囲まれた部分)
            (font-lock-add-keywords nil
                                    '(("```[a-zA-Z]*\n\\(.\\|\n\\)*?```" . 'font-lock-constant-face)))
            ;; 強調 (**bold** や *italic*)
            (font-lock-add-keywords nil
                                    '(("\\*\\*\\*\\(.*?\\)\\*\\*\\*" . 'font-lock-warning-face)
                                      ("\\*\\*\\(.*?\\)\\*\\*" . 'font-lock-warning-face)
                                      ("\\*\\(.*?\\)\\*" . 'font-lock-variable-name-face)))
            ;; リンク
            (font-lock-add-keywords nil
                                    '(("\\[\\([^]]+\\)\\](\\([^)]*\\))" . 'font-lock-type-face)))
            ;; 水平線
            (font-lock-add-keywords nil
                                    '(("^[ \t]*[-=]\\{3,\\}$" . 'font-lock-comment-face)))))

(setq auto-mode-alist (cons '("\\.md\\'" . md-mode) auto-mode-alist))

;;; ------------------------------------------------------------
;;; provides

(provide 'markdown.init)

;;; markdown.init.el ends here
