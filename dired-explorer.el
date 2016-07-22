;;; dired-explorer.init.el --- minor-mode provides Explorer like select file at dired.
;; Original: http://www.bookshelf.jp/soft/meadow_25.html#SEC286
;; Introduce: rubikitch
;; Maintainer: jidaikobo-shibata
;; for Emacs 24.5.1

;;; Commentary:
;; [en]
;; This mode provides "Windows / Macintosh (Mac OS X) like file selection" for dired's buffer.
;; Move cursor by just pressing alphabet or number key.
;; And also it prohibits dired from opening many buffers.
;; of course, at this mode, cannot use dired's default keybind like "c".
;; You may use keybind that made of one alphabet, use with Meta (e.g. M-d).
;; toggle mode by ":".
;; rubikitch told me about this elisp's url at his school.
;; but I couldn't know who made this originally.
;;
;; [ja]
;; WindowsやMac OS Xのデフォルトのファイラのようなファイル選択をdiredで行います。
;; 英数字のキーを打鍵するだけで、diredでファイル／ディレクトリを選択します。
;; また、diredeがたくさんのバッファを開きすぎることを抑止しています。
;; 当然ながら、このモードを有効にするとデフォルトのdiredのキーバインドが使えません。
;; diredのアルファベット一文字のキーバインドは基本的に"M-"にあて直しています。
;; モードの切り替えは":"で行ってください。
;; このelispは、るびきちさんが彼のEmacs塾で、僕にURLを教えてくれましたが、
;; 僕にはオリジナルの作者が誰かわからなかったので、URLだけ明示しています。

;;; Usage:
;; just write below in your .init.
;; (require 'dired-explorer)
;; below are also useful.
;; (define-key dired-mode-map (kbd "RET") 'dired-explorer-dired-open)
;; (define-key dired-mode-map (kbd "<s-return>") 'dired-explorer-dired-open)
;; toggle mode by ":".

;;; Code:

(require 'dired)

(defvar dired-explorer-mode-map          nil)
(defvar dired-explorer-mode              nil)
(defvar dired-explorer-isearch-next      "\C-r")
(defvar dired-explorer-isearch-prev      "\C-e")
(defvar dired-explorer-isearch-backspace "\C-h")
(defvar dired-explorer-isearch-return    "\C-g")
(defvar dired-explorer-isearch-returnkey "\C-m")
(defvar dired-explorer-isearch-word      "")
(defvar dired-explorer-mode-hook         nil)
(defvar dired-mode-old-local-map)

(if dired-explorer-mode-map ()
  (setq dired-explorer-mode-map (make-sparse-keymap))
  (set-keymap-parent dired-explorer-mode-map dired-mode-map)

  (define-key dired-explorer-mode-map "\M-a" 'dired-find-alternate-file)
  (define-key dired-explorer-mode-map "\M-d" 'dired-flag-file-deletion)
  (define-key dired-explorer-mode-map "\M-e" 'dired-find-file)
  (define-key dired-explorer-mode-map "\M-f" 'dired-find-file)
  (define-key dired-explorer-mode-map "\M-\C-m" 'dired-find-file)
  (define-key dired-explorer-mode-map "\M-g" 'revert-buffer)
  (define-key dired-explorer-mode-map "\M-i" 'dired-maybe-insert-subdir)
  (define-key dired-explorer-mode-map "\M-j" 'dired-goto-file)
  (define-key dired-explorer-mode-map "\M-k" 'dired-do-kill-lines)
  (define-key dired-explorer-mode-map "\M-l" 'dired-do-redisplay)
  (define-key dired-explorer-mode-map "\M-m" 'dired-mark)
  (define-key dired-explorer-mode-map "\M-n" 'dired-next-line)
  (define-key dired-explorer-mode-map "\M-o" 'dired-find-file-other-window)
  (define-key dired-explorer-mode-map "\M-p" 'dired-previous-line)
  (define-key dired-explorer-mode-map "\M-s" 'dired-sort-toggle-or-edit)
  (define-key dired-explorer-mode-map "\M-t" 'dired-toggle-marks)
  (define-key dired-explorer-mode-map "\M-u" 'dired-unmark)
  (define-key dired-explorer-mode-map "\M-v" 'dired-view-file)
  (define-key dired-explorer-mode-map "\M-w" 'dired-copy-filename-as-kill)
  (define-key dired-explorer-mode-map "\M-x" 'dired-do-flagged-delete)
  (define-key dired-explorer-mode-map "\M-y" 'dired-show-file-type)

  (define-key dired-explorer-mode-map ":" 'dired-explorer-mode)
  (define-key dired-explorer-mode-map "\C-m" 'dired-explorer-dired-open)
  (define-key dired-explorer-mode-map (kbd "<return>") 'dired-explorer-dired-open)
  (define-key dired-explorer-mode-map "^" 'dired-explorer-dired-open)
  (define-key dired-explorer-mode-map "I" 'dired-kill-subdir))

(defvar dired-explorer-mode nil)

(defun dired-explorer-mode (&optional arg)
  "Minor-mode dired-explorer-mode.  ARG."
  (interactive)
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq dired-explorer-mode nil))
   ((and arg (eq major-mode 'dired-mode))
    (setq dired-explorer-mode t))
   ((eq major-mode 'dired-mode)
    (setq dired-explorer-mode (not dired-explorer-mode))))
  (if dired-explorer-mode
      (progn
        (run-hooks 'dired-explorer-mode-hook)
        (setq dired-mode-old-local-map (current-local-map))
        (use-local-map dired-explorer-mode-map)
        (when (not (assq 'dired-explorer-mode minor-mode-alist))
          (setq minor-mode-alist
                (cons '(dired-explorer-mode " Expr")
                      minor-mode-alist))))
    (use-local-map dired-mode-old-local-map)))

  (defun dired-explorer-do-isearch (REGEX1 REGEX2 FUNC1 FUNC2 RPT)
    "Dired explorer isearch.  REGEX1 REGEX2 FUNC1 FUNC2 RPT."
  (interactive)
  (let ((input last-command-event)
        (inhibit-quit t)
        (oldpoint (point))
        regx
        str
        (n 1))
    (save-match-data
      (catch 'END
        (while t
          (funcall FUNC1)
          (cond
           ;;end
           ((and (integerp input) (= input ?:))
            (setq unread-command-events (append (list input) unread-command-events))
            (throw 'END nil))

           ;; character
           ;;_.-+~#
           ((and (integerp input)
                 (or (and (>= input ?a) (<= input ?z))
                     (and (>= input ?A) (<= input ?Z))
                     (and (>= input ?0) (<= input ?9))))
            (setq str (char-to-string input))
            (if (string= dired-explorer-isearch-word str)
                (setq n 2)
              (setq n 1))
            ;; .meadow.el meadow.el は同一視
            (setq regx (concat REGEX1 "[\.~#+_]*" str REGEX2))
            (if (not (re-search-forward regx nil t n))
                (progn
                  (goto-char (point-min))
                  (re-search-forward regx nil t nil)))
            (setq dired-explorer-isearch-word str))

           ;; backspace
           ((and (integerp input)
                 (or (eq 'backspace input)
                     (= input (string-to-char dired-explorer-isearch-backspace))))
            (setq str (if (eq 0 (length str)) str (substring str 0 -1)))
            (setq regx (concat REGEX1 str REGEX2))
            (goto-char oldpoint)
            (re-search-forward regx nil t nil))

           ;; next
           ((and (integerp input) (= input (string-to-char dired-explorer-isearch-next)))
            (re-search-forward regx nil t RPT))

           ;; previous
           ((and (integerp input) (= input (string-to-char dired-explorer-isearch-prev)))
            (re-search-backward regx nil t nil))

           ;; return
           ((and (integerp input) (= input (string-to-char dired-explorer-isearch-return)))
            (goto-char oldpoint)
            (message "return")
            (throw 'END nil))

           ;; other command
           (t
            (setq unread-command-events (append (list input) unread-command-events))
            (throw 'END nil)))

          (funcall FUNC2)
          ;; (highline-highlight-current-line)
          ;; (message str)
          (setq input (read-event)))))))

(defun dired-explorer-isearch()
  (interactive)
  (dired-explorer-do-isearch
   "[0-9] "                                                        ; REGEX1
   "[^ \n]+$"                                                      ; REGEX2
   (lambda() (if (not (= (point-min) (point))) (backward-char 3))) ; FUNC1
   'dired-move-to-filename                                         ; FUNC2
   2                                                               ; RPT
   ))

(defun dired-explorer-isearch-define-key (str)
  "Dired explorer isearch define key.  STR."
  (let ((i 0))
    (while (< i (length str))
      (define-key dired-explorer-mode-map (substring str i (1+ i)) 'dired-explorer-isearch)
      (setq i (1+ i)))))

(add-hook 'dired-explorer-mode-hook
          '(lambda ()
;;             (dired-explorer-isearch-define-key "abcdefghijklmnopqrstuvwxyz0123456789_.-+~#")
             (dired-explorer-isearch-define-key "abcdefghijklmnopqrstuvwxyz0123456789")))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map ":" (lambda () (interactive) (dired-explorer-mode t)))
            (dired-explorer-mode t)))

(put 'dired-find-alternate-file 'disabled nil)
(defun dired-explorer-dired-open ()
  "Dired open in accordance with situation."
  (interactive)
  (let (p1
        p2
        (file "")
        (path (dired-file-name-at-point))
        (is-explorer (eq major-mode 'dired-explorer-mode)))
    (save-excursion
      (setq p1 (dired-move-to-filename))
      (setq p2 (dired-move-to-end-of-filename)))
    (when (and p1 p2) (setq file (buffer-substring p1 p2)))
    ;; (message "this-event: %s this-command: %s" last-input-event this-command)
    (cond ((string= file ".")
           (message "current directory."))
          ;; up directory at same buffer
          ((and
            (one-window-p)
            (or
             (memq last-input-event '(94)) ; means "^"
             (and (string= file "..") (not (memq last-input-event '(s-return S-return))))))
           ;; (message "up")
           (find-alternate-file
            (file-name-directory (directory-file-name (dired-current-directory)))))
          ;; find file/directory at same buffer
          ((and
            (one-window-p)
            (or
             (and (file-directory-p path) (not (memq last-input-event '(s-return S-return))))))
           ;; (message "move")
           (dired-find-alternate-file))
          ;; find file/directory at new buffer when S-RET / s-RET
          (t
           ;; (message "etc")
           (dired-find-file)))
    ;; keep explorer-mode
    (when (or (and (file-directory-p path) is-explorer)
              (and (string= file "..") is-explorer))
      (unless dired-explorer-mode (dired-explorer-mode t)))))

;;; ------------------------------------------------------------
;;; Provide

(provide 'dired-explorer)

;;; dired-explorer.el ends here
