;;; editor-tabs.init.el --- Stable tabs for editable buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Share FOEB's selection policy, but keep an independent, stable tab order.
;; Toggle the experiment with M-x my/editor-tabs-mode.

;;; Code:

(require 'cl-lib)
(require 'tab-line)
(add-to-list 'load-path
             (expand-file-name "elisp/focus-on-editable-buffers"
                               user-emacs-directory))
(require 'focus-on-editable-buffers nil t)

(defvar my/editor-tabs--buffers nil "Buffers in tab order.")
(defvar my/editor-tabs--saved-settings nil "Settings before enabling tabs.")
(defvar my/editor-tabs--was-global nil "Previous global tab line state.")

(defun my/editor-tabs--targets ()
  "Return live FOEB targets, preserving order and appending new targets."
  (let ((targets (cl-remove-if-not #'buffer-live-p (foeb/target-buffers))))
    (setq my/editor-tabs--buffers
          (append (cl-remove-if-not (lambda (buffer) (memq buffer targets))
                                    my/editor-tabs--buffers)
                  (cl-remove-if (lambda (buffer)
                                  (memq buffer my/editor-tabs--buffers))
                                (reverse targets))))
    my/editor-tabs--buffers))

(defun my/editor-tabs--close (buffer)
  "Close BUFFER with the existing modified-buffer confirmation."
  (let* ((tabs (my/editor-tabs--targets))
         (position (cl-position buffer tabs))
         (replacement (or (nth (1+ (or position 0)) tabs)
                          (and position (> position 0)
                               (nth (1- position) tabs))))
         (displayed (eq buffer (window-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (custom-kill-buffer-ask)))
    (when (and displayed (not (buffer-live-p buffer))
               (buffer-live-p replacement))
      (switch-to-buffer replacement))
    (tab-line-force-update t)))

(defun my/editor-tabs--tabs ()
  "Return tabs shared by all windows, including windows showing Dired."
  (mapcar (lambda (buffer)
            `((name . ,(concat (buffer-name buffer)
                              (if (buffer-modified-p buffer) " *" "")))
              (buffer . ,buffer)
              (selected . ,(eq buffer (window-buffer)))
              (close . ,(lambda () (my/editor-tabs--close buffer)))))
          (my/editor-tabs--targets)))

(defun my/editor-tabs--move (step)
  "Select a tab STEP positions away, wrapping at the ends."
  (let* ((tabs (my/editor-tabs--targets))
         (position (cl-position (current-buffer) tabs)))
    (when tabs
      (switch-to-buffer
       (nth (if position
                (mod (+ position step) (length tabs))
              (if (> step 0) 0 (1- (length tabs))))
            tabs)))))

(defun my/editor-tabs-next ()
  "Select the next editable tab, or the first tab from Dired."
  (interactive)
  (my/editor-tabs--move 1))

(defun my/editor-tabs-previous ()
  "Select the previous editable tab, or the last tab from Dired."
  (interactive)
  (my/editor-tabs--move -1))

(defun my/editor-tabs-new ()
  "Open an empty text buffer as a new tab without creating a file."
  (interactive)
  (let ((directory default-directory)
        (buffer (generate-new-buffer "untitled")))
    (with-current-buffer buffer
      (setq default-directory directory)
      (text-mode))
    (switch-to-buffer buffer)))

(defun my/editor-tabs--neighbour (_original &optional next)
  "Make FOEB neighbour commands follow tab order; NEXT selects direction."
  (my/editor-tabs--move (if next 1 -1)))

(defvar my/editor-tabs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-<left>") #'my/editor-tabs-previous)
    (define-key map (kbd "C-M-<right>") #'my/editor-tabs-next)
    (define-key map (kbd "C-t") #'my/editor-tabs-new)
    ;; Also accept the Super events observed with the desktop modifier mapping.
    (define-key map (kbd "M-s-<left>") #'my/editor-tabs-previous)
    (define-key map (kbd "M-s-<right>") #'my/editor-tabs-next)
    (define-key map (kbd "s-t") #'my/editor-tabs-new)
    (define-key map (kbd "C-x <left>") #'my/editor-tabs-previous)
    (define-key map (kbd "C-x C-<left>") #'my/editor-tabs-previous)
    (define-key map (kbd "C-x <right>") #'my/editor-tabs-next)
    (define-key map (kbd "C-x C-<right>") #'my/editor-tabs-next)
    map)
  "Keys for the editable buffer tab experiment.")

(define-minor-mode my/editor-tabs-mode
  "Toggle stable, shared tabs for FOEB's editable buffers."
  :global t
  :group 'tab-line
  (if my/editor-tabs-mode
      (if (not (featurep 'focus-on-editable-buffers))
          (progn
            (setq my/editor-tabs-mode nil)
            (user-error "focus-on-editable-buffers を利用できません"))
        (unless my/editor-tabs--saved-settings
          (setq my/editor-tabs--was-global global-tab-line-mode
                my/editor-tabs--saved-settings
                (mapcar (lambda (symbol) (cons symbol (default-value symbol)))
                        '(tab-line-tabs-function tab-line-close-button-show
                          tab-line-new-button-show)))
          (setq-default tab-line-tabs-function #'my/editor-tabs--tabs
                        tab-line-close-button-show t
                        tab-line-new-button-show nil)
          (setq my/editor-tabs--buffers nil)
          (my/editor-tabs--targets)
          (add-hook 'buffer-list-update-hook #'my/editor-tabs--targets)
          (advice-add 'foeb/switch-to-neighbour-buffer :around
                      #'my/editor-tabs--neighbour)
          (global-tab-line-mode 1)))
    (when my/editor-tabs--saved-settings
      (unless my/editor-tabs--was-global (global-tab-line-mode -1))
      (remove-hook 'buffer-list-update-hook #'my/editor-tabs--targets)
      (advice-remove 'foeb/switch-to-neighbour-buffer
                     #'my/editor-tabs--neighbour)
      (dolist (setting my/editor-tabs--saved-settings)
        (set-default (car setting) (cdr setting)))
      (setq my/editor-tabs--saved-settings nil
            my/editor-tabs--buffers nil)))
  (tab-line-force-update t))

(provide 'editor-tabs.init)
;;; editor-tabs.init.el ends here
