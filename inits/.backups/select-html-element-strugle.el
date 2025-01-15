(defun is-caret-inside-tag ()
  "キャレットが`<`と`>`の間に挟まれているかを判定します。
条件を満たせば`t`を返し、そうでなければ`nil`を返します。"
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
  "キャレット位置に基づいてHTMLの要素名を取得します。
1. キャレットがタグ内にいる場合、タグのHTML要素名を返します。
2. キャレットがタグ外にいる場合、直近のキャレットより後の終了タグの要素名を返します。"
  (interactive)
  (if (is-caret-inside-tag)
      ;; タグ内にいる場合
      (let ((element-name nil))
        (save-excursion
          ;; 前方のタグ名を取得
          (search-backward "<" (point-min) t)
          (if (looking-at "<\\(/*[a-zA-Z1-6]+\\)") ; タグの要素名をキャプチャ
              (setq element-name (match-string 1))))
        (message "Caret is inside a tag: <%s>" element-name)
        element-name)
    ;; タグ外にいる場合
    (let ((near-close-tag nil))
      (save-excursion
        ;; キャレットより後の終了タグを探索
        (if (search-forward "</" (point-max) t)
            (if (looking-at "\\([a-zA-Z1-6]+\\)>") ; 終了タグの要素名をキャプチャ
                (setq near-close-tag (match-string 1)))))
      (if near-close-tag
          (message "Caret is outside a tag, next closing tag: </%s>" near-close-tag)
        (message "Caret is outside a tag, no closing tag found."))
      near-close-tag)))

(defun select-html-element-at-caret ()
  "キャレット位置に基づいてHTMLの要素の範囲を選択します。
1. タグの内側か外側かを判定します。
2. タグの内側にいる場合、開始タグまたは終了タグの内側にいるかを判定します。
3. 適切な開始タグと終了タグを探索し、選択範囲を設定します。
4. 単体要素の場合はタグ全体を選択します。"
  (interactive)
  (let ((self-closing-tags '("img" "br" "hr" "!DOCTYPE" "meta" "!--")))
    (if (is-caret-inside-tag)
        ;; タグの内側の場合
        (let* ((tag-info (get-html-element-at-caret)) ; タグ名とスラッシュ有無を取得
               (is-closing-tag (and tag-info (string-prefix-p "/" tag-info))) ; 閉じタグか判定
               (tag-name (if is-closing-tag (substring tag-info 1) tag-info))) ; タグ名を抽出
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
          (message "No tag found to select."))))))

(defun select-range-for-self-closing-tag (tag-name)
  "タグを選択します"
  (interactive)
  (search-forward ">" nil t)
  (set-mark (point))
  (search-backward (concat "<" tag-name) nil t)
  (goto-char (match-beginning 0))
  (message "Selected range for tag: <%s>" tag-name))

(defun count-occurrences-in-region (start end search-string)
  "Count the number of times SEARCH-STRING appears in the region from START to END.
Returns the count as a number."
  (let ((count 0))
    (save-excursion
      (goto-char start)
      (while (search-forward search-string end t)
        (setq count (1+ count))))
    count))

(defun select-range-for-closing-tag (tag-name)
  "キャレットが閉じタグ内にある場合に、開始タグから閉じタグの範囲を選択します。
TAG-NAMEはタグの名前です。"
  (interactive)
  (let (point-a point-b point-c open-tag-count close-tag-count)
    ;; 1. 直近の`>`に移動し、その位置を変数に格納する（地点A）
    (search-forward ">" nil t)
    (setq point-a (point))
    ;; (message "point-a %s" point-a)

    ;; 2. 文書先頭から開始タグを探し、その先頭の位置を地点Bに格納
    (goto-char (point-min))
    (if (search-forward (concat "<" tag-name) nil t)
        (progn
          (setq point-b (match-beginning 0)))
      ;; (message "No matching start tag found for <%s>" tag-name)
      (return-from select-range-for-closing-tag))

    ;; 3. 地点Aと地点Bの間の開始タグと終了タグの数を数える
    (setq open-tag-count 0 close-tag-count 0)
    (setq open-tag-count (count-occurrences-in-region point-b point-a (concat "<" tag-name)))
    (setq close-tag-count (count-occurrences-in-region point-b point-a (concat "</" tag-name)))

    ;; 開始タグの数が終了タグより少ない場合は終了
    (when (< open-tag-count close-tag-count)
      (message "Mismatched tags: start tags < end tags for <%s>" tag-name)
      (return-from select-range-for-closing-tag))

    ;; 開始タグと終了タグの数が同数になるまで地点Bを更新
    (while (> open-tag-count close-tag-count)
      (goto-char point-b)
      (search-forward ">" nil t)
      (if (search-forward (concat "<" tag-name) point-a t)
          (setq point-b (match-beginning 0))
        (message "No matching start tag found to balance for <%s>" tag-name)
        (return-from select-range-for-closing-tag))
      ;; 再カウント
      (setq open-tag-count (count-occurrences-in-region point-b point-a (concat "<" tag-name))))

    ;; 4. 地点Aに移動し、地点C（地点Aの直近の開始タグ）を決定
    (goto-char point-a)
    (if (search-backward (concat "<" tag-name) point-b t)
        (setq point-c (match-beginning 0))
      (message "No matching start tag found before point A for <%s>" tag-name)
      (return-from select-range-for-closing-tag))

    ;; 地点Cと地点Aの間に終了タグがなければ地点Cと地点Aを選択
    (setq close-tag-count (count-occurrences-in-region point-c point-a (concat "</" tag-name)))
    (if (= close-tag-count 1)
        (progn
          (goto-char point-c)
          (set-mark point-a)
          (message "Selected range from C to A for tag: <%s>" tag-name))
      ;; 終了タグがある場合地点Aと地点Bを選択
      (progn
        (goto-char point-b)
        (set-mark point-a)
        (message "Selected range from B to A for tag: <%s>" tag-name)))))





(defun select-range-for-opening-tag2 (tag-name)
  "キャレットが開始タグ内にある場合に、開始タグから閉じタグの範囲を選択します。
TAG-NAMEはタグの名前です。"
  (interactive)
  (let (point-a point-b point-c open-tag-count close-tag-count)
    ;; 1. 直近の`<`に移動し、その位置を変数に格納する（地点A）
    (search-backward "<" nil t)
    (setq point-a (point))
    ;; (message "point-a %s" point-a)

    ;; 2. 文書末尾から終了タグを探し、その先頭の位置を地点Bに格納
    (goto-char (point-max))
    (if (search-backward (concat "</" tag-name) nil t)
        (progn
          (search-forward ">" nil t)
          (setq point-b (point)))
      (message "No matching start tag found for <%s>" tag-name)
      (return-from select-range-for-closing-tag))

    ;; 3. 地点Aと地点Bの間の開始タグと終了タグの数を数える
    (setq open-tag-count 0 close-tag-count 0)
    (setq open-tag-count (count-occurrences-in-region point-a point-b (concat "<" tag-name)))
    (setq close-tag-count (count-occurrences-in-region point-a point-b (concat "</" tag-name)))

    ;; (message "open-tag-count: %s" open-tag-count)
    (message "default close-tag-count: %s" close-tag-count)

    ;; 閉じタグの数が開始タグより少ない場合は終了
    (when (< close-tag-count open-tag-count)
      (message "Mismatched tags: close tags < end tags for <%s>" tag-name)
      (return-from select-range-for-closing-tag))

    (message "default point-b: %s" point-b)

    ;; 開始タグと終了タグの数が同数になるまで地点Bを更新
    (while (> close-tag-count open-tag-count)
      (goto-char point-b)
      (message "point-b: %s" point-b)
      (if (search-backward (concat "</" tag-name) point-a t)
          (progn
            (setq point-b (- (point) 1))
            (message "next point-b: %s" point-b)
            )
        (message "No matching start tag found to balance for <%s>" tag-name)
        (return-from select-range-for-closing-tag))
      ;; 再カウント
      (setq close-tag-count (count-occurrences-in-region point-a point-b (concat "</" tag-name)))
      (message "close-tag-count: %s" close-tag-count))
    ;; (message "point-b: %s" point-b)
    ;; (message "open-tag-count: %s" open-tag-count)

    (message "final close-tag-count: %s" close-tag-count)
    (message "final point-b: %s" point-b)

    ;; 4. 地点Aに移動し、地点C（地点Aの直近の終了タグ）を決定
    (goto-char point-a)
    (if (search-forward (concat "</" tag-name) point-b t)
        (progn
          (search-forward ">" nil t)
          (setq point-c (point)))
      (message "No matching start tag found before point A for <%s>" tag-name)
      (return-from select-range-for-closing-tag))

    ;; 地点Cと地点Aの間に開始タグがなければ地点Cと地点Aを選択
    (setq close-tag-count (count-occurrences-in-region point-a point-c (concat "<" tag-name)))
    (if (= close-tag-count 1)
        (progn
          (goto-char point-a)
          (set-mark point-c)
          (message "Selected range from C to A for tag: <%s>" tag-name))
      ;; 終了タグがある場合地点Aと地点Bを選択
      (progn
        (goto-char point-a)
        (set-mark point-b)
        (message "Selected range from B to A for tag: <%s>" tag-name)))))




(defun select-range-for-opening-tag (tag-name)
  "キャレットが開始タグ内にある場合に、開始タグから閉じタグの範囲を選択します。
TAG-NAMEはタグの名前です。"
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
      (message "point: %s" (point))
      (let ((is-closing-tag (string-prefix-p "</" (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))
        (if is-closing-tag
            (progn
              (pop stack)
              (message "pop - Current stack size: %d" (length stack)))
          (push tag-name stack)
          (message "push - Current stack size: %d" (length stack)))))

    ;; スタックが空になったとき終了位置を記録
    (search-forward ">" nil t)
      (when (not stack)
        (setq end-pos (point)))

    (if (and start-pos end-pos)
        (progn
          (goto-char start-pos)
          (set-mark end-pos)
          (message "Selected range for <%s>" tag-name))
      (message "No valid range found for <%s>" tag-name))))

(global-set-key (kbd "s-A") 'select-html-element-at-caret)

;; (setq debug-on-error t)

(defun select-html-element-with-stack (start-tag)
  "Select the range for a given START-TAG, ensuring nested tags are handled correctly."
  (interactive "sEnter start tag (e.g., div): ")
  (let ((stack '()) ; スタックを初期化
        (start-pos nil)
        (end-pos nil))
    (save-excursion
      ;; 現在のカーソル位置から開始タグを検索
      (when (search-forward (concat "<" start-tag) nil t)
        (setq start-pos (match-beginning 0)) ; 開始位置を記録
        (push start-tag stack)) ; 開始タグをスタックにプッシュ

      ;; 開始タグが見つかった場合、閉じタグを追跡
      (while (and stack (re-search-forward (concat "</?" start-tag) nil t))
        (if (looking-at (concat "</" start-tag)) ; 閉じタグの場合
            (pop stack) ; スタックからポップ
          (push start-tag stack))) ; 開始タグの場合スタックにプッシュ

      ;; スタックが空になったとき終了位置を記録
      (when (not stack)
        (setq end-pos (point))))

    ;; 範囲を選択
    (if (and start-pos end-pos)
        (progn
          (goto-char start-pos)
          (set-mark end-pos)
          (message "Selected range for <%s>" start-tag))
      (message "No valid range found for <%s>" start-tag))))
