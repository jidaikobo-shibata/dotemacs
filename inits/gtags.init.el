;;; gtags.init.el
;;; Commentary:
;; provide gtags.
;;; Code:

(require 'subr-x)
(require 'gtags)

(defvar my-gtags-allowed-dirs-file
  (expand-file-name "~/.emacs.d/gtags-allowed-dirs")
  "gtags-auto-update を許可するプロジェクトルート一覧のファイル。")

(defun my-gtags--read-allowed-dirs ()
  "許可済みのプロジェクトルート一覧を返す。"
  (when (file-readable-p my-gtags-allowed-dirs-file)
    (with-temp-buffer
      (insert-file-contents my-gtags-allowed-dirs-file)
      (let (dirs)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let ((trimmed (string-trim line)))
            (unless (or (string-empty-p trimmed)
                        (string-prefix-p "#" trimmed))
              (push (directory-file-name (expand-file-name trimmed)) dirs))))
        dirs))))

(defun my-gtags--project-root-with-gtags (&optional dir)
  "DIR から上にたどって GTAGS のあるルートを返す。"
  (let ((root (locate-dominating-file (or dir default-directory) "GTAGS")))
    (when root
      (directory-file-name root))))

(defun my-gtags-auto-update-allowed-p ()
  "現在のバッファで gtags-auto-update を有効にしてよいか返す。"
  (let ((root (my-gtags--project-root-with-gtags)))
    (and root
         (not (file-remote-p default-directory))
         (member root (my-gtags--read-allowed-dirs))
         (file-writable-p (expand-file-name "GTAGS" root)))))

(defun my-gtags-setup-auto-update ()
  "現在のバッファ向けに gtags-auto-update を設定する。"
  (setq-local gtags-auto-update (my-gtags-auto-update-allowed-p)))

(with-eval-after-load 'gtags
  (setq gtags-global-command
        (or (executable-find "global")
            "/usr/bin/global"))
  (setq gtags-path-style 'absolute)
  (setq gtags-auto-update nil)
  (define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag)
  (define-key gtags-mode-map (kbd "M->") 'gtags-find-rtag) ; M-shift-.
  (define-key gtags-mode-map (kbd "M-s-.") 'gtags-parse-current-file) ; M-super-.
  (define-key gtags-mode-map (kbd "M-,") 'gtags-pop-stack)
  (add-hook 'gtags-mode-hook #'my-gtags-setup-auto-update))

;;; ------------------------------------------------------------
;;; gtags-parse-fileをカレントバッファに対して行う

(defun gtags-parse-current-file ()
  "Show the list of tags in the current buffer's file."
  (interactive)
  (if (buffer-file-name)
      (let ((current-file (buffer-file-name)))
        (if (file-regular-p current-file)
            (let ((tagname (expand-file-name current-file)))
              (gtags-push-context)
              (gtags-goto-tag tagname "f"))
          (message "Current file is not a regular file.")))
    (message "Current buffer is not visiting a file.")))

;;; ------------------------------------------------------------
;;; provides

(provide 'gtags.init)

;;; gtags.init.el ends here
