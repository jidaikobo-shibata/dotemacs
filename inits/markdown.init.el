;;; markdown.init.el --- init for markdown
;;; Commentary:
;; provide markdown.init.
;;; Code:

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\|txt\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; markdown-modeでも等幅フォントを使いたい……というか、なんでデフォルトで等幅じゃないんだ？ エディタで使うんだぞ……。

(defun my-markdown-setup ()
  "Setup font and size for markdown-mode."
  ;; フォントを等幅に設定
  (buffer-face-set 'fixed-pitch)
  ;; フォントサイズを調整
  (buffer-face-set '(:height 90))
  ;; variable-pitch-modeを無効化
  (variable-pitch-mode -1))

(add-hook 'markdown-mode-hook 'my-markdown-setup)

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

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "s-M-1") 'my-insert-markdown-h1)
  (define-key markdown-mode-map (kbd "<s-M-kp-1>") 'my-insert-markdown-h1)
  (define-key markdown-mode-map (kbd "s-M-2") 'my-insert-markdown-h2)
  (define-key markdown-mode-map (kbd "<s-M-kp-2>") 'my-insert-markdown-h2)
  (define-key markdown-mode-map (kbd "s-M-3") 'my-insert-markdown-h3)
  (define-key markdown-mode-map (kbd "<s-M-kp-3>") 'my-insert-markdown-h3)
  (define-key markdown-mode-map (kbd "s-M-4") 'my-insert-markdown-h4)
  (define-key markdown-mode-map (kbd "<s-M-kp-4>") 'my-insert-markdown-h4)
  (define-key markdown-mode-map (kbd "s-M-5") 'my-insert-markdown-h5)
  (define-key markdown-mode-map (kbd "<s-M-kp-5>") 'my-insert-markdown-h5)
  (define-key markdown-mode-map (kbd "s-M-6") 'my-insert-markdown-h6)
  (define-key markdown-mode-map (kbd "<s-M-kp-6>") 'my-insert-markdown-h6)
  (define-key markdown-mode-map (kbd "s-M-a") 'my-insert-markdown-link)
  (define-key markdown-mode-map (kbd "s-M-i") 'my-insert-markdown-img)
  (define-key markdown-mode-map (kbd "s-M-l") 'my-markdown-toggle-list)
  (define-key markdown-mode-map (kbd "s-M-u") 'my-markdown-toggle-list)
  (define-key markdown-mode-map (kbd "s-M-o") 'my-markdown-toggle-ordered-list)
  (define-key markdown-mode-map (kbd "s-M-t") 'my-markdown-convert-table))

;; my-insert-markdown-heading

(defun my-insert-markdown-heading (level)
  "Insert a Markdown heading of the specified LEVEL.
Move to the beginning of the line and insert the appropriate number of '#'."
  (interactive "p")  ; プレフィックス引数でレベルを指定
  (beginning-of-line)  ; 行頭に移動
  (insert (make-string level ?#))  ; プレフィックス引数に応じて'#'を生成
  (insert " "))       ; スペースを挿入

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

;;; ------------------------------------------------------------
;;; provides

(provide 'markdown.init)

;;; markdown.init.el ends here
