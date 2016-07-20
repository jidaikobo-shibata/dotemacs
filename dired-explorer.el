;;; dired-explorer.init.el --- Explorer like select file at dired.
;; Original: http://www.bookshelf.jp/soft/meadow_25.html#SEC286
;; Introduce: rubikitch
;; Maintainer: jidaikobo-shibata
;; for Emacs 24.5.1

;;; Commentary:
;; [en]
;; This mode provides "Windows / Macintosh (Mac OS X) like file selection" for dired's buffer.
;; Move cursor by just pressing alphabet or number key.
;; And also it prohibits dired from opening many buffers.
;; of course, at this mode, cannot use dired's default keybind like "C".
;; toggle mode by ":".
;; rubikitch told me about this elisp's url at his school.
;; but I couldn't know who made this originally.
;;
;; [ja]
;; WindowsやMac OS Xのデフォルトのファイラのようなファイル選択をdiredで行います。
;; 英数字のキーを打鍵するだけで、diredでファイル／ディレクトリを選択します。
;; また、diredeがたくさんのバッファを開きすぎることを抑止しています。
;; 当然ながら、このモードを有効にするとデフォルトのdiredのキーバインドが使えません。
;; モードの切り替えは:で行ってください。
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

(if dired-explorer-mode-map ()
  (setq dired-explorer-mode-map (make-sparse-keymap))
  (define-key dired-explorer-mode-map ":" 'dired-explorer-mode)
  (define-key dired-explorer-mode-map "\C-m" 'dired-explorer-dired-open)
  (define-key dired-explorer-mode-map (kbd "<return>") 'dired-explorer-dired-open)
  (define-key dired-explorer-mode-map "^" 'dired-explorer-dired-open)
  (define-key dired-explorer-mode-map "\M-A" 'dired-do-search)
  (define-key dired-explorer-mode-map "\M-C" 'dired-do-copy)
  (define-key dired-explorer-mode-map "\M-B" 'dired-do-byte-compile)
  (define-key dired-explorer-mode-map "\M-D" 'dired-do-delete)
  (define-key dired-explorer-mode-map "\M-G" 'dired-do-chgrp)
  (define-key dired-explorer-mode-map "\M-H" 'dired-do-hardlink)
  (define-key dired-explorer-mode-map "\M-I" 'dired-kill-subdir) ;; added
  (define-key dired-explorer-mode-map "\M-L" 'dired-do-load)
  (define-key dired-explorer-mode-map "\M-M" 'dired-do-chmod)
  (define-key dired-explorer-mode-map "\M-O" 'dired-do-chown)
  (define-key dired-explorer-mode-map "\M-P" 'dired-do-print)
  (define-key dired-explorer-mode-map "\M-Q" 'dired-do-query-replace-regexp)
  (define-key dired-explorer-mode-map "\M-R" 'dired-do-rename)
  (define-key dired-explorer-mode-map "\M-S" 'dired-do-symlink)
  (define-key dired-explorer-mode-map "\M-T" 'dired-do-touch)
  (define-key dired-explorer-mode-map "\M-X" 'dired-do-shell-command)
  (define-key dired-explorer-mode-map "\M-Z" 'dired-do-compress)
  (define-key dired-explorer-mode-map "\M-!" 'dired-do-shell-command)
  (define-key dired-explorer-mode-map "\M-&" 'dired-do-async-shell-command)
  (define-key dired-explorer-mode-map "\M-a" 'dired-explorer-dired-open)
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
  (define-key dired-explorer-mode-map "\M-\C-o" 'dired-display-file)
  (define-key dired-explorer-mode-map "\M-p" 'dired-previous-line)
  (define-key dired-explorer-mode-map "\M-s" 'dired-sort-toggle-or-edit)
  (define-key dired-explorer-mode-map "\M-t" 'dired-toggle-marks)
  (define-key dired-explorer-mode-map "\M-u" 'dired-unmark)
  (define-key dired-explorer-mode-map "\M-v" 'dired-view-file)
  (define-key dired-explorer-mode-map "\M-w" 'dired-copy-filename-as-kill)
  (define-key dired-explorer-mode-map "\M-x" 'dired-do-flagged-delete)
  (define-key dired-explorer-mode-map "\M-y" 'dired-show-file-type)
  (define-key dired-explorer-mode-map "\M-+" 'dired-create-directory)
  (define-key dired-explorer-mode-map "$" 'dired-hide-subdir)
  (define-key dired-explorer-mode-map "\M-$" 'dired-hide-all)
  (define-key dired-explorer-mode-map [down] 'dired-next-line)
  (define-key dired-explorer-mode-map [up] 'dired-previous-line)
  ;; Tree Dired commands
  (define-key dired-explorer-mode-map "\M-\C-?" 'dired-unmark-all-files)
  (define-key dired-explorer-mode-map "\M-\C-d" 'dired-tree-down)
  (define-key dired-explorer-mode-map "\M-\C-u" 'dired-tree-up)
  (define-key dired-explorer-mode-map "\M-\C-n" 'dired-next-subdir)
  (define-key dired-explorer-mode-map "\M-\C-p" 'dired-prev-subdir)
  (define-key dired-explorer-mode-map "*" 'dired-explorer-mark)
  (define-key dired-explorer-mode-map "\C-_" 'dired-undo)
  (define-key dired-explorer-mode-map "\C-xu" 'dired-undo))

(defvar dired-mode-old-major-mode)
(defvar dired-mode-old-mode-name)
(defvar dired-mode-old-local-map)
(defvar dired-explorer-mode-hook nil)

(defun dired-explorer-mode ()
  "Dired explorer mode."
  (interactive)
  (if (eq major-mode 'dired-explorer-mode)
      (progn
        (dired-move-to-filename)
        (use-local-map dired-mode-old-local-map)
        (setq mode-name dired-mode-old-mode-name)
        (setq major-mode dired-mode-old-major-mode)
        (force-mode-line-update))
    (setq dired-mode-old-local-map (current-local-map))
    (setq dired-mode-old-mode-name mode-name)
    (setq dired-mode-old-major-mode major-mode)
    (use-local-map dired-explorer-mode-map)
    (setq major-mode 'dired-explorer-mode)
    (setq mode-name "Explorer")
    (force-mode-line-update)
    (run-hooks 'dired-explorer-mode-hook)))

(defun dired-explorer-do-isearch (REGEX1 REGEX2 FUNC1 FUNC2 RPT)
  "Dired explorer isearch.  REGEX1 REGEX2 FUNC1 FUNC2 RPT."
  (interactive)
  (let ((input last-command-event)
        (inhibit-quit t)
        (oldpoint (point)) regx str (n 1))
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
          ;;(highline-highlight-current-line)
          (message str)
          (setq input (read-event)))))))

(defun dired-explorer-isearch()
  (interactive)
  (dired-explorer-do-isearch "[0-9] " "[^ \n]+$"
                 (lambda()
                   (if (not (= (point-min) (point)))
                       (backward-char 3)))
                 'dired-move-to-filename 2))

(defun dired-explorer-isearch-define-key (str)
  "Dired explorer isearch define key.  STR."
  (let ((i 0))
    (while (< i (length str))
      (define-key dired-explorer-mode-map (substring str i (1+ i)) 'dired-explorer-isearch)
      (setq i (1+ i)))))

(add-hook 'dired-explorer-mode-hook
          '(lambda ()
             (dired-explorer-isearch-define-key "abcdefghijklmnopqrstuvwxyz0123456789_.-+~#")))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map ":" 'dired-explorer-mode)
            (dired-explorer-mode)))

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
          ((or
            (memq last-input-event '(94)) ; means "^"
            (and (string= file "..") (not (memq last-input-event '(s-return S-return)))))
           (find-alternate-file
            (file-name-directory (directory-file-name (dired-current-directory)))))
          ;; find file/directory at same buffer
          ((and (file-directory-p path) (not (memq last-input-event '(s-return S-return))))
           (dired-find-alternate-file))
          ;; find file/directory at new buffer when S-RET / s-RET
          (t
           (dired-find-file)))
    ;; keep explorer-mode
    (when (or (and (file-directory-p path) is-explorer)
              (and (string= file "..") is-explorer))
      (unless (eq major-mode 'dired-explorer-mode) (dired-explorer-mode)))))

(defun dired-explorer-mark-toggle ()
  "Dired explorer mark toggle."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      (beginning-of-line)
      (if (not (eobp))
          (or (dired-between-files)
                (looking-at dired-re-dot)
              (apply 'subst-char-in-region
                     (point) (1+ (point))
                     (if (eq ?\040 (following-char)) ; SPC
                         (list ?\040 dired-marker-char)
                       (list dired-marker-char ?\040))))
        (forward-line 1)))))

(defun dired-explorer-mark ()
  "Dired explorer mark."
  (interactive)
  (dired-explorer-mark-toggle)
  (dired-next-line 1))

;;; ------------------------------------------------------------
;;; Provide

(provide 'dired-explorer)

;;; dired-explorer.el ends here
