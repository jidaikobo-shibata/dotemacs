;;; mozc.init.el --- init for mozc-mode
;;; Commentary:
;; provide mozc.init.
;;; Code:

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;;; ------------------------------------------------------------
;; muhenkanでMozcを抜ける
(defun my-deactivate-input-method ()
  "Deactivate the input method."
  (interactive)
  (deactivate-input-method))
(with-eval-after-load 'mozc
  (define-key mozc-mode-map (kbd "<muhenkan>") 'my-deactivate-input-method))
(global-set-key (kbd "<muhenkan>") 'my-deactivate-input-method)

;;; ------------------------------------------------------------
;; henkanでMozcを起こす
(global-set-key (kbd "<henkan>")
		(lambda () (interactive)
		  (activate-input-method default-input-method)))

;;; ------------------------------------------------------------
;; mozc-modeでもdelete-selection-modeを機能させる
(defun my-mozc-handle-event (orig-fun &rest args)
  "Make `delete-selection-mode` work in mozc-mode.  ORIG-FUN, ARGS."
  (let* ((event (nth 0 args))
         (event-type (and event (event-basic-type event))))
    (if (and (use-region-p)
             event
             (not (or (member event-type '(left right up down henkan muhenkan wheel-up wheel-down escape))
                      (member 'shift (event-modifiers event))
                      (member 'super (event-modifiers event))
                      (member 'meta (event-modifiers event))
                      (member 'control (event-modifiers event))
                      (mouse-event-p event))))
        (delete-region (region-beginning) (region-end)))
    (apply orig-fun args)))

(advice-add 'mozc-handle-event :around #'my-mozc-handle-event)

(defun my-delete-selection-before-yank (&rest _args)
  "Deactivate the selection before yank."
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))))

(advice-add 'yank :before #'my-delete-selection-before-yank)

;;; ------------------------------------------------------------
;;; provides

(provide 'mozc.init)

;;; mozc.init.el ends here
