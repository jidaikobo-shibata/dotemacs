;;; filer.init.el --- init for filer-mode
;;; Commentary:
;; provide filer.
;;; Code:

;;; ------------------------------------------------------------
;; find-fileをzshライクに
;; thx http://d.hatena.ne.jp/mooz/20101003/p1
(require 'zlc)
(zlc-mode 1)
(let ((map minibuffer-local-map))
  (define-key map (kbd "<down>") 'next-history-element)
  (define-key map (kbd "<up>")   'previous-history-element))

;;; ------------------------------------------------------------
;; root権限でファイルを開き直す
;; thx http://qiita.com/k_ui/items/d9e03ea9523036970519
(defun reopen-with-sudo ()
  "Reopen current buffer-file with sudo."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))

;;; ------------------------------------------------------------
;; 現在バッファのファイルのフルパスを取得
(defun get-current-path ()
  "Get current file path."
  (interactive)
  (let ((path (or (buffer-file-name) (expand-file-name default-directory))))
    (kill-new path)
    (message path)))
(global-set-key (kbd "M-s-k") 'get-current-path)

;;; ------------------------------------------------------------
;;; dired

(add-to-list 'load-path "~/.emacs.d/elisp/dired-explorer")

(require 'dired)
(require 'dired-aux)
(require 'dired-explorer)
(require 'wdired)

(setq ls-lisp-use-localized-time-format t)
(setq ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))

;; emacs-async
(eval-after-load "dired-aux" '(require 'dired-async))

;; omit .DS_Store
;; thx https://www.emacswiki.org/emacs/DiredOmitMode
(require 'dired-x)
(add-hook 'dired-load-hook #'(lambda () (require 'dired-x)))
(setq dired-omit-mode t)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.DS_Store")

;; dired-explorer
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map ":" (lambda () (interactive) (dired-explorer-mode t)))
            (dired-explorer-mode t)))

;; diredでファイル名編集（M-rで編集モード。:でdired-explorerを切って作業。C-c C-cで確定）
(define-key dired-mode-map "E" 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "<M-return>") 'dired-maybe-insert-subdir)
(define-key dired-explorer-mode-map "\M-r" 'wdired-change-to-wdired-mode)
(define-key wdired-mode-map (kbd "C-g") 'wdired-abort-changes)
(define-key wdired-mode-map [escape] 'wdired-abort-changes)

;; spaceでtoggle marks
(define-key dired-mode-map " " 'dired-toggle-mark)
(define-key dired-explorer-mode-map " " 'dired-toggle-mark)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) file."
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char " ")))
    (dired-mark arg)))

;; diredの前後の行移動をshift対応に
;; thx rubikitch
(defun dired-next-line--shift-select (&rest them)
  "Dired next line shift select.  THEM."
  (interactive "^p")
  (apply them))
(advice-add 'dired-next-line :around 'dired-next-line--shift-select)
(advice-add 'dired-previous-line :around 'dired-next-line--shift-select)

;; C-x C-f で現在位置を開く
(ffap-bindings)

;; ディレクトリ操作は再帰的に
(setq dired-recursive-copies 'always)

;; diredバッファでC-sした時にファイル名だけにマッチするように
(add-hook 'dired-mode-hook 'dired-isearch-filenames-mode)

;; ウィンドウ分割で左右に違うDiredを開いているときにRやCのデフォルト値がもう片方になる
(setq dired-dwim-target t)

;; key-binds
 (define-key dired-mode-map (kbd "M-o") 'other-window)
 (define-key dired-explorer-mode-map (kbd "M-o") 'other-window)
;; (define-key dired-mode-map (kbd "RET") 'dired-explorer-dired-open)
;; (define-key dired-mode-map (kbd "<s-return>") 'dired-explorer-dired-open)
(define-key dired-mode-map (kbd "a") 'dired-find-file)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
;; (define-key dired-mode-map (kbd "M-s") 'dired-isearch-filenames-regexp)
;; (define-key dired-mode-map (kbd "C-s") 'anything-occur)
(define-key dired-mode-map (kbd "s-d") (lambda () (interactive) (find-file "~/Desktop")))
(define-key dired-mode-map (kbd "s-u") (lambda () (interactive) (find-file "~/Desktop/uploads")))
(global-set-key (kbd "C-x C-d") (lambda () (interactive) (find-file default-directory)
                                  (delete-other-windows)))

;; dired-download-to-desktop
(defun dired-download-to-desktop ()
  "Download to desktop."
  (interactive)
  (dired-copy-file-recursive
   (dired-get-filename) "~/Desktop" t dired-copy-preserve-time t 'always)
  (message "Download to desktop."))
(define-key dired-mode-map (kbd "C-d") 'dired-download-to-desktop)
(define-key dired-explorer-mode-map (kbd "C-d") 'dired-download-to-desktop)

;;; ------------------------------------------------------------
;; .poファイルを保存したらmsgfmt -oする

(add-hook 'after-save-hook
          (lambda ()
            (when (string= (file-name-extension (buffer-file-name)) "po")
              (shell-command (concat
                              "msgfmt -o "
                              (substring (buffer-file-name) 0 -2) "mo "
                              (buffer-file-name))))))

;;; ------------------------------------------------------------
;;; provides

(provide 'filer.init)

;;; filer.init.el ends here
