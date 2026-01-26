;;; web-select-html-ele.el --- jidaikobo web authoring
;;; Commentary:
;; for select html element

;;; Code:

(defun is-caret-inside-tag ()
  "Determine if the caret is between `<` and `>`."
  (let ((pos (point))
        (left-bracket nil))
    (save-excursion
      ;; キャレットより前の`<`を探索
      (setq left-bracket (search-backward "<" (point-min) t)))
    (and left-bracket
         ;; `left-bracket`とキャレットの間に`>`がない場合
         (not (save-excursion
                (search-backward ">" left-bracket t))))))

(defun get-html-element-at-caret ()
  "Gets the HTML element name based on the caret position.
1. inside: HTML element
2. outside: the closest closing tag"
  (interactive)
  (if (is-caret-inside-tag)
      ;; タグ内にいる場合
      (let ((element-name nil))
        (save-excursion
          ;; 前方のタグ名を取得
          (search-backward "<" (point-min) t)
          (if (looking-at "<\\(/*[a-zA-Z1-6!-]+\\)") ; タグの要素名をキャプチャ
              (setq element-name (match-string 1))))
        ;; (message "Caret is inside a tag: <%s>" element-name)
        element-name)
    ;; タグ外にいる場合
    (let ((near-close-tag nil))
      (save-excursion
        ;; キャレットより後の終了タグを探索
        (if (search-forward "</" (point-max) t)
            (if (looking-at "\\([a-zA-Z1-6]+\\)>") ; 終了タグの要素名をキャプチャ
                (setq near-close-tag (match-string 1)))))
      ;; (if near-close-tag
      ;;     (message "Caret is outside a tag, next closing tag: </%s>" near-close-tag)
      ;;   (message "Caret is outside a tag, no closing tag found."))
      near-close-tag)))

(defun select-html-element-at-caret ()
  "Select a range of HTML elements based on the caret position."
  (interactive)
  (let ((self-closing-tags '("img" "br" "hr" "!DOCTYPE" "input" "meta" "link" "!--")))
    (if (is-caret-inside-tag)
        ;; タグの内側の場合
        (let* ((tag-info (get-html-element-at-caret)) ; タグ名とスラッシュ有無を取得
               (is-closing-tag (and tag-info (string-prefix-p "/" tag-info))) ; 閉じタグか判定
               (tag-name (if is-closing-tag (substring tag-info 1) tag-info))) ; タグ名を抽出
          (message tag-name)
          ;; 単体要素の場合
          (if (member tag-name self-closing-tags)
              (select-range-for-self-closing-tag tag-name)
            ;; 通常要素の場合
            (if is-closing-tag
                ;; キャレットが閉じタグ内の場合
                (select-range-for-closing-tag tag-name)
              ;; キャレットが開始タグ内の場合
              (select-range-for-opening-tag tag-name))))
      ;; タグの外側の場合
      (let ((tag-name (get-html-element-at-caret))) ; 直近の終了タグを取得
        (if tag-name
            (progn
              ;; キャレットを終了タグ内に移す
              (search-forward (concat "</" tag-name ">") nil t)
              (goto-char (match-beginning 0))
              ;; 終了タグの末端から対応する開始タグを探して選択範囲を設定
              (select-range-for-closing-tag tag-name))
          ;; (message "No tag found to select.")
)))))

(defun select-range-for-self-closing-tag (tag-name)
  "Select a TAG-NAME."
  (interactive)
  (search-forward ">" nil t)
  (set-mark (point))
  (search-backward (concat "<" tag-name) nil t)
  (goto-char (match-beginning 0))
  ;; (message "Selected range for tag: <%s>" tag-name)
)

(defun select-range-for-opening-tag (tag-name)
  "When the caret is within an opening tag, select the range for TAG-NAME."
  (interactive)
  (let ((stack '()) ; スタックを初期化
        (start-pos nil)
        (end-pos nil))
    ;; 1. 直近の`<`に移動し、その位置を記録しスタック
    (search-backward "<" nil t)
    (setq start-pos (point))
    (push tag-name stack)
    (search-forward ">" nil t)

    ;; (message "default - Current stack size: %d" (length stack))

    ;; 関連するタグを探し、閉じタグならpop、開始タグならpushしゼロにする
    (while (and stack (re-search-forward (concat "</?" tag-name) nil t))
      ;; (message "point: %s" (point))
      (let ((is-closing-tag (string-prefix-p "</" (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
        (if is-closing-tag
            (progn
              (pop stack)
              ;; (message "pop - Current stack size: %d" (length stack))
              )
          (push tag-name stack)
          ;; (message "push - Current stack size: %d" (length stack))
          )))

    ;; スタックが空になったとき終了位置を記録
    (search-forward ">" nil t)
      (when (not stack)
        (setq end-pos (point)))

    (if (and start-pos end-pos)
        (progn
          (goto-char start-pos)
          (set-mark end-pos)
          ;; (message "Selected range for <%s>" tag-name)
          )
      ;; (message "No valid range found for <%s>" tag-name)
)))

(defun select-range-for-closing-tag (tag-name)
  "When the caret is within an closing tag, select the range for TAG-NAME."
  (interactive)
  (let ((stack '()) ; スタックを初期化
        (start-pos nil)
        (end-pos nil))
    ;; 1. 直近の`>`に移動し、その位置を記録しスタック
    (search-forward ">" nil t)
    (setq end-pos (point))
    (push tag-name stack)
    (search-backward "<" nil t)

    (message "default - Current stack size: %d" (length stack))

    ;; 関連するタグを探し、開始タグならpop、閉じタグならpushしゼロにする
    (while (and stack (re-search-backward (concat "</?" tag-name) nil t))
      (message "point: %s" (point))
      (let ((is-opening-tag (not (string-prefix-p "</" (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))
        (if is-opening-tag
            (progn
              (pop stack)
              (message "pop - Current stack size: %d" (length stack)))
          (push tag-name stack)
          (message "push - Current stack size: %d" (length stack)))))

    ;; スタックが空になったとき終了（開始）位置を記録
    (when (not stack)
      (setq start-pos (point)))

    (if (and start-pos end-pos)
        (progn
          (goto-char start-pos)
          (set-mark end-pos)
          (message "Selected range for <%s>" tag-name))
      (message "No valid range found for <%s>" tag-name))))

(global-set-key (kbd "s-A") 'select-html-element-at-caret)

;; (setq debug-on-error t)

;;; ------------------------------------------------------------
;;; Provide

(provide 'web-select-html-ele)

;;; web-select-html-ele.el ends here
