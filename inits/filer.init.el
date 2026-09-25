;;; filer.init.el --- init for filer-mode
;;; Commentary:
;; provide filer.
;;; Code:

;;; ------------------------------------------------------------
;; find-fileをzshライクに
;; thx http://d.hatena.ne.jp/mooz/20101003/p1
(when (require 'zlc nil t)
  (zlc-mode 1)
  ;; File candidates can have `completion--unquoted' before `face'.
  ;; zlc 0.0.5 assumes `face' is the first property and inserts the
  ;; common prefix again (e.g. ~/.co + .codex/).
  (defun zlc-select-nth (n)
    "Select the Nth zlc candidate using its common-part face."
    (interactive)
    (setq zlc--index (zlc--normalize-index
                      n (length zlc--global-cache)))
    (delete-region zlc--field-begin (field-end))
    (if (>= zlc--index 0)
        (let* ((candidate (zlc--current-candidate))
               (string (if (consp candidate) (car candidate) candidate))
               (from (if (eq (get-text-property 0 'face string)
                             'completions-common-part)
                         (or (next-single-property-change 0 'face string)
                             (length string))
                       0)))
          (insert (substring string from))
          (zlc--highlight-nth-completion zlc--index))
      (zlc--clear-overlay))))
(let ((map minibuffer-local-map))
  (define-key map (kbd "<down>") 'next-history-element)
  (define-key map (kbd "<up>")   'previous-history-element))

;;; ------------------------------------------------------------
;; C-x C-f へ貼り付けたHTTP(S) URLをローカルファイルとして開く

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defvar my/find-file-url-roots nil
  "Session-local mappings from HTTP(S) URL prefixes to local directories.")

(defun my/find-file--http-url-p (value)
  "Return non-nil when VALUE is an HTTP(S) URL."
  (and (stringp value)
       (let ((case-fold-search t))
         (string-match-p "\\`https?://" value))))

(defun my/find-file--url-origin (url)
  "Return the scheme and authority portion of URL with a trailing slash."
  (let* ((parsed (url-generic-parse-url url))
         (scheme (downcase (or (url-type parsed) "")))
         (host (url-host parsed))
         (port (url-port parsed)))
    (unless (and (member scheme '("http" "https")) host)
      (user-error "Invalid HTTP(S) URL: %s" url))
    (format "%s://%s%s/"
            scheme
            (if (string-match-p ":" host)
                (format "[%s]" (downcase host))
              (downcase host))
            (if (or (and (string= scheme "http") (= port 80))
                    (and (string= scheme "https") (= port 443)))
                ""
              (format ":%s" port)))))

(defun my/find-file--canonical-url (url)
  "Return URL without its query or fragment and with a canonical origin."
  (let* ((parsed (url-generic-parse-url url))
         (origin (my/find-file--url-origin url))
         (path (car (split-string (or (url-filename parsed) "") "[?#]"))))
    (concat origin (replace-regexp-in-string "\\`/+" "" path))))

(defun my/find-file--suggest-url-root (url)
  "Return URL itself when directory-like, otherwise its parent directory."
  (let ((canonical-url (my/find-file--canonical-url url)))
    (if (string-suffix-p "/" canonical-url)
        canonical-url
      (file-name-directory canonical-url))))

(defun my/find-file--normalize-url-root (url-root url)
  "Normalize URL-ROOT and ensure that it is a directory prefix of URL."
  (let* ((root-origin (my/find-file--url-origin url-root))
         (url-origin (my/find-file--url-origin url))
         (canonical-root
          (file-name-as-directory (my/find-file--canonical-url url-root)))
         (canonical-url (my/find-file--canonical-url url)))
    (unless (string= root-origin url-origin)
      (user-error "URL root must use the same origin: %s" url-origin))
    (unless (string-prefix-p canonical-root canonical-url)
      (user-error "URL root is not a directory prefix of URL: %s"
                  canonical-root))
    canonical-root))

(defun my/find-file--matching-url-root (url)
  "Return the longest registered URL-prefix mapping matching URL."
  (let ((canonical-url (my/find-file--canonical-url url))
        best)
    (dolist (mapping my/find-file-url-roots best)
      (when (and (string-prefix-p (car mapping) canonical-url)
                 (or (null best)
                     (> (length (car mapping)) (length (car best)))))
        (setq best mapping)))))

(defun my/find-file--select-url-root (url)
  "Ask for and remember a URL prefix and its local directory for URL."
  (let* ((suggested-root (my/find-file--suggest-url-root url))
         (url-root
          (my/find-file--normalize-url-root
           (read-string "URL root: " suggested-root)
           url))
         (local-root
          (file-name-as-directory
           (expand-file-name
            (read-directory-name
             (format "Local root for %s: " url-root)
             default-directory nil t)))))
    (setq my/find-file-url-roots
          (cons (cons url-root local-root)
                (cl-remove url-root my/find-file-url-roots
                           :key #'car :test #'string=)))
    (cons url-root local-root)))

(defun my/find-file--url-root (url force-prompt)
  "Return the URL-prefix mapping for URL, prompting when needed.
When FORCE-PROMPT is non-nil, replace any mapping already remembered."
  (let ((known (my/find-file--matching-url-root url)))
    (if (and known (not force-prompt))
        known
      (my/find-file--select-url-root url))))

(defun my/find-file--url-relative-path (url url-root)
  "Return URL's safe, decoded path relative to URL-ROOT."
  (let* ((canonical-url (my/find-file--canonical-url url))
         (encoded (substring canonical-url (length url-root)))
         (relative (url-unhex-string encoded))
         (segments (split-string relative "/" t)))
    (when (or (string-match-p "\0" relative)
              (member ".." segments))
      (user-error "Unsafe path in URL: %s" url))
    relative))

(defun my/find-file--resolve-url (url force-root-prompt)
  "Resolve URL to a local path.
When FORCE-ROOT-PROMPT is non-nil, ask for the local root again."
  (let* ((mapping (my/find-file--url-root url force-root-prompt))
         (url-root (car mapping))
         (local-root (cdr mapping))
         (relative (my/find-file--url-relative-path url url-root))
         (path (expand-file-name relative local-root)))
    (unless (or (equal (directory-file-name path)
                       (directory-file-name local-root))
                (file-in-directory-p path local-root))
      (user-error "URL resolves outside the local root: %s" url))
    (if (or (string-empty-p relative)
            (string-suffix-p "/" relative))
        (cond
         ((file-exists-p (expand-file-name "index.html" path))
          (expand-file-name "index.html" path))
         ((file-exists-p (expand-file-name "index.htm" path))
          (expand-file-name "index.htm" path))
         (t path))
      path)))

(defun my/find-file-dwim (&optional force-root-prompt)
  "Open a regular file or map an HTTP(S) URL to a local file.
With FORCE-ROOT-PROMPT, select the URL prefix and local root again."
  (interactive "P")
  (let ((target (ffap-prompter)))
    (if (my/find-file--http-url-p target)
        (let ((local-path
               (my/find-file--resolve-url target force-root-prompt)))
          (when (or (file-exists-p local-path)
                    (y-or-n-p
                     (format "Local file does not exist; open anyway? %s "
                             local-path)))
            (find-file local-path)))
      (find-file-at-point target))))

;;; ------------------------------------------------------------
;; root権限でファイルを開き直す
(defun reopen-with-sudo ()
  "Open current file with sudo in a separate buffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name
      (error "Cannot get a file name"))
    (when (file-remote-p file-name)
      (error "Already a remote file"))
    (find-file (concat "/sudo::" file-name))))

;;; ------------------------------------------------------------
;; 現在バッファのファイルのフルパスを取得
(defun get-current-path ()
  "Get current file path."
  (interactive)
  (let* ((raw-path (or (buffer-file-name) (expand-file-name default-directory)))
         (path (if (file-remote-p raw-path)
                   (tramp-file-name-localname
                    (tramp-dissect-file-name raw-path))
                 raw-path)))
    (kill-new path)
    (message "%s" path)))
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
(eval-after-load "dired-aux" '(require 'dired-async nil t))

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
(global-set-key (kbd "C-x C-f") #'my/find-file-dwim)

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

(defun my/msgfmt-compile-po-file ()
  "Compile the current local .po file into a .mo file."
  (when (and buffer-file-name
             (string= (file-name-extension buffer-file-name) "po")
             (not (file-remote-p buffer-file-name))
             (executable-find "msgfmt"))
    (let ((po-file buffer-file-name)
          (mo-file (concat (file-name-sans-extension buffer-file-name) ".mo")))
      (unless (zerop (process-file "msgfmt" nil nil nil "-o" mo-file po-file))
        (message "msgfmt failed: %s" po-file)))))

(defun my/enable-msgfmt-after-save ()
  "Enable local .po compilation after save for the current buffer."
  (when (and buffer-file-name
             (string= (file-name-extension buffer-file-name) "po"))
    (add-hook 'after-save-hook #'my/msgfmt-compile-po-file nil t)))

(add-hook 'po-mode-hook #'my/enable-msgfmt-after-save)

;;; ------------------------------------------------------------
;;; provides

(provide 'filer.init)

;;; filer.init.el ends here
