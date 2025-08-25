;;; jidaikobo-dark-theme.el --- theme
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: nicr-takahata
;; URL: https://github.com/nicr-takahata/dotemacs
;; Fork: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; 黒っぽい背景のテーマ
;; M-x list-faces-displayでface一覧表表示
;; 変更したい場所を見つけたらカーソルを持って行ってdescribe-face

;;; Code:

(deftheme jidaikobo-dark "Dark theme for jidaikobo.")

;;; ------------------------------------------------------------
;;; color palette
(let ((class '((class color) (min-colors 89) (background dark)))
      (jidaikobo-fg            "#ffffff")
      (jidaikobo-bg            "#201f1f")
      (jidaikobo-gray          "#404040")
      (jidaikobo-cloudyred     "#f87373")
      (jidaikobo-red           "#ff0000")
      (jidaikobo-pink          "#f92672")
      (jidaikobo-orange        "#fd971f")
      (jidaikobo-deeporange    "#e96529")
      (jidaikobo-darkgoldenrod "#e6db74")
      (jidaikobo-wheat         "#c4be89")
      (jidaikobo-olive         "#808000")
      (jidaikobo-chartreuse    "#a6e22e")
      (jidaikobo-darkwine      "#1e0010")
      (jidaikobo-wine          "#960050")
      (jidaikobo-aqua          "#00ffff")
      (jidaikobo-lightblue     "#66d9ef")
      (jidaikobo-blue          "#0064ff")
      (jidaikobo-palevioletred "#d33682"))

;;; ------------------------------------------------------------
;;; custom-theme-set-faces
  (custom-theme-set-faces
   'jidaikobo-dark

;;; ------------------------------------------------------------
;;; base
   `(default   ((t (:foreground ,jidaikobo-fg :background ,jidaikobo-bg))))
   `(fringe    ((t (:foreground ,jidaikobo-fg :background ,jidaikobo-bg)))) ; フレームの余白
   `(highlight ((t (:background ,jidaikobo-olive))))
   `(region    ((t (:background ,jidaikobo-blue :extend t))))
   `(warning   ((t (:foreground ,jidaikobo-palevioletred :weight bold))))
   `(error     ((t (:foreground ,jidaikobo-pink :weight bold))))
   `(success   ((t (:foreground ,jidaikobo-chartreuse :weight bold))))
   `(shadow    ((t (:foreground ,jidaikobo-gray))))
   `(link      ((t (:foreground ,jidaikobo-lightblue :underline t))))
   `(minibuffer-prompt ((t (:foreground ,jidaikobo-lightblue :weight bold))))
   `(secondary-selection ((t (:background ,jidaikobo-darkwine))))

;;; ------------------------------------------------------------
;;; cursor
   `(cursor ((t (:foreground ,jidaikobo-bg :background ,jidaikobo-fg))))

   ;; multiple-cursors
   `(mc/cursor-bar-face
     ((t (:foreground ,jidaikobo-fg
          :background ,jidaikobo-bg))))

;;; ------------------------------------------------------------
;;; font lock

   ;; コメント
   `(font-lock-comment-face ((t (:foreground ,jidaikobo-orange :background ,jidaikobo-bg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,jidaikobo-orange :background ,jidaikobo-bg))))

   ;; ビルトイン関数
   `(font-lock-builtin-face ((t (:foreground ,jidaikobo-aqua))))

   ;; 関数名
   `(font-lock-function-name-face ((t (:foreground ,jidaikobo-chartreuse))))

   ;; 変数名
   `(font-lock-variable-name-face ((t (:foreground ,jidaikobo-darkgoldenrod))))
   `(font-lock-warning-face ((t (:foreground ,jidaikobo-palevioletred :weight bold))))

   ;; 定数
   `(font-lock-constant-face ((t (:foreground ,jidaikobo-wheat))))
   `(font-lock-doc-face ((t (:foreground ,jidaikobo-darkgoldenrod))))

   ;; キーワード
   `(font-lock-keyword-face ((t (:foreground ,jidaikobo-lightblue))))

   ;; for easily-overlooked negation characters.
   `(font-lock-negation-char-face ((t (:foreground ,jidaikobo-deeporange :weight bold))))

   ;; 数値
   `(font-lock-number-face ((t (:foreground ,jidaikobo-pink))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))

   ;; 文字列
   `(font-lock-string-face ((t (:foreground ,jidaikobo-cloudyred))))

   ;; データ型名
   `(font-lock-type-face ((t (:foreground ,jidaikobo-pink))))

;;; ------------------------------------------------------------
;;; mode line
   `(mode-line ((t (:foreground "black" :background ,jidaikobo-wheat :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground "grey30" :background ,jidaikobo-bg :box nil))))

   ;; 標準の補助フェイス
   `(mode-line-emphasis  ((t (:weight bold))))
   `(mode-line-highlight ((t (:inherit highlight))))

;;; ------------------------------------------------------------
;;; header-line
   `(header-line ((t (:foreground ,jidaikobo-bg :background ,jidaikobo-wheat :weight bold))))

;;; ------------------------------------------------------------
;;; isearch
   `(isearch ((t (:foreground "black" :background ,jidaikobo-wheat :weight bold))))
   `(isearch-fail ((t (:foreground ,jidaikobo-wine :background ,jidaikobo-darkwine))))

   ;; 2番目以降の候補（インクリメンタル検索の“薄い強調”）
   `(lazy-highlight ((t (:background ,jidaikobo-gray :foreground ,jidaikobo-fg))))

;;; ------------------------------------------------------------
;;; flycheck
   `(flycheck-error   ((t (:underline (:style wave :color ,jidaikobo-pink)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,jidaikobo-palevioletred)))))
   `(flycheck-info    ((t (:underline (:style wave :color ,jidaikobo-lightblue)))))

;;; ------------------------------------------------------------
;;; anything
   `(anything-selection-face ((t (:foreground "black" :background ,jidaikobo-wheat :weight bold))))

;;; ------------------------------------------------------------
;;; linum
   `(linum ((t (:foreground "grey30" :background ,jidaikobo-bg))))

;;; ------------------------------------------------------------
;;; line numbers (display-line-numbers-mode)
   `(line-number ((t (:foreground "grey30" :background ,jidaikobo-bg))))
   `(line-number-current-line ((t (:foreground ,jidaikobo-wheat :background ,jidaikobo-bg :weight bold))))

;;; ------------------------------------------------------------
;;; hl-line-mode
   `(hl-line ((t (:foreground unspecified :background "grey30" :extend t))))

;;; ------------------------------------------------------------
;;; whitespace
   `(whitespace-trailing ((t (:background ,jidaikobo-bg :underline t))))
   `(whitespace-space ((t (:background ,jidaikobo-bg :underline nil))))
   `(whitespace-empty ((t (:background ,jidaikobo-bg :underline nil))))
   `(whitespace-tab ((t (:foreground "grey20" :background ,jidaikobo-bg :underline nil))))

;;; ------------------------------------------------------------
;;; auto-complete and popup
   `(ac-completion-face ((t (:foreground "purple" :background "grey10"))))
   `(ac-candidate-face ((t (:foreground "grey50"))))
   `(ac-selection-face ((t (:background "grey40"))))
   `(popup-face ((t (:foreground "grey40" :background "grey20"))))
   ))

;;; ------------------------------------------------------------
;;; Provide

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jidaikobo-dark)

;;; jidaikobo-dark-theme.el ends here
