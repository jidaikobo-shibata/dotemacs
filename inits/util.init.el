;;; util.init.el --- init for util
;;; Commentary:
;; provide util.init.
;;; Code:

;;; ------------------------------------------------------------
;; 新規バッファを開く
;; thx open-junk-file by rubikitch
(defun my-find-file-other-window (&optional frame)
  "Find file other window.  FRAME is optional."
  (interactive)
  (select-frame (if frame frame (selected-frame)))
  (find-file
   (format-time-string "~/.emacs.d/.tmp/%Y%m%d-%H%M%S.txt" (current-time))))
(global-set-key (kbd "s-n") 'my-find-file-other-window)

;;; ------------------------------------------------------------
;;; 選択範囲を計算してバッファに出力
;; gist-description: Emacs(Elisp): calculate region and insert. 選択範囲の数式を計算して、次の行にinsertします。数字が羅列されている場合は、加算します。数字や式と自然な文章が混在している場合は、数式のみを計算します。
;; gist-id: b967d6a7441f85aa541d
;; gist-name: calculate-region-and-insert.el
;; gist-private: nil

(defun add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.
It defaults to a comma."
  (let ((num (number-to-string number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat
                 (match-string 1 num) op
                 (match-string 2 num))))
    num))

(defun calculate-region-and-insert (beg end)
  "Calculate natural text of region and insert to current buffer.  BEG, END."
  (interactive "r")
  (let* ((strings (if mark-active
                      (buffer-substring-no-properties beg end)
                    (read-string " Expression: " "")))
         (is_num_format (string-match "," (buffer-substring-no-properties beg end)))
         result)
    ;; 余計なものを取り払って計算の準備
    (when mark-active
      (with-temp-buffer
        (insert strings)
        (perform-replace "[\t,　 ]+" "" nil t nil nil nil (point-min) (point-max))
        (perform-replace "\n" "+" nil t nil nil nil (point-min) (point-max))
        (perform-replace "[^0-9\\+\\*/\\(\\)^\\.-]" "+" nil t nil nil nil (point-min) (point-max))
        (perform-replace "\\++" "+" nil t nil nil nil (point-min) (point-max))
        (perform-replace "\\+$" "" nil t nil nil nil (point-min) (point-max))
        (perform-replace "^\\++" "" nil t nil nil nil (point-min) (point-max))
        (setq strings (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char end)
      (end-of-line)
      (newline))
    (setq result (calc-eval strings))
    ;; カンマ整形されている計算式だったらカンマ区切りで返す
    (when is_num_format (setq result (add-number-grouping (string-to-number result) ",")))
    ;; (calc-eval)は、小数点を含んだ式の場合、整数でも末尾にピリオドをつけるので抑止
    (when (string-match "\\.$" result)
      (setq result (substring result 0 (match-beginning 0))))
    (insert result)))
(global-set-key (kbd "M-c") 'calculate-region-and-insert)

;;; ------------------------------------------------------------
;;; 選択範囲の言語を確認して翻訳 (C-c t)

;; google-translate
;; http://rubikitch.com/2014/12/07/google-translate/
(require 'google-translate)
;; (setq google-translate--tkk-url "http://translate.google.cn/")
(defvar google-translate-english-chars "[:ascii:]"
  "Ascii means English.")
(defun google-translate-enja-or-jaen (&optional string)
  "Google translate enja or jaen.  STRING in region."
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)

;;; ------------------------------------------------------------
;;; 起動時には最後に作業していたファイルを開く
;; gist-description: Emacs(Elisp): Preserve last buffers and its each point to reopen. 終了時のバッファとポイントを記憶して、起動時に同じ状態で開くelispです。
;; gist-id: 35b4d739a149f70e86298f71e5b1f9e7
;; gist-name: preserve-last-buffers-and-point.el
;; gist-private: nil

(defvar my-hist-dir (expand-file-name "~/.emacs.d/histories/"))
(defvar my-hist-last-files (concat my-hist-dir "last-files"))

(add-hook 'kill-emacs-hook
          (lambda ()
            (let ((strings "")
                  buf-path)
              (with-temp-buffer
                (dolist (buf (buffer-list))
                  (save-current-buffer
                    (setq buf-path (buffer-file-name buf))
                    (when (and buf-path (file-exists-p buf-path))
                      (set-buffer buf)
                      (setq strings
                            (concat strings "\n" buf-path ":" (number-to-string (point)))))))
                (insert (string-trim strings))
                (write-file my-hist-last-files)))))

(when (file-exists-p my-hist-last-files)
  (let (tmp
        path
        pt
        (files (split-string
                (with-temp-buffer
                  (insert-file-contents my-hist-last-files)
                  (buffer-string))
                "\n")))
    (when files
      (dolist (file files)
        (setq tmp (split-string file ":"))
        (setq path (car tmp))
        (setq pt (string-to-number(car (reverse tmp))))
        (when (file-exists-p path)
          (find-file path)
          (goto-char  pt))))))

;;; ------------------------------------------------------------
;;; provides

(provide 'util.init)

;;; util.init.el ends here
