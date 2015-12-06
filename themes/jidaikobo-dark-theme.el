;;; jidaikobo-dark.el --- theme
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: nicr-takahata
;; URL: https://github.com/nicr-takahata/dotemacs
;; Fork: jidaikobo-shibata
;; URL: https://github.com/jidaikobo-shibata/dotemacs

;;; Commentary:
;; 黒っぽい背景のテーマ
;; 変更したい場所を見つけたらカーソルを持って行ってdescribe-face

;;; Code:

(deftheme jidaikobo-dark "jidaikobo-dark color theme")

;; jidaikobo color palette
(let ((class '((class color) (min-colors 89) (background dark)))
			(jidaikobo-white             "#ffffff")
			(jidaikobo-fg                "#ffffff")
			(jidaikobo-bg                "#201f1f")
			(jidaikobo-base01            "#465457")
			(jidaikobo-base02            "#455354")
			(jidaikobo-base03            "#293739")
			(jidaikobo-red-1             "#f87373")
			(jidaikobo-red               "#ff0000")
			(jidaikobo-pink              "#f92672")
			(jidaikobo-orange-1          "#fd971f")
			(jidaikobo-orange            "#e96529")
			(jidaikobo-yellow            "#ffff00")
			(jidaikobo-darkgoldenrod     "#e6db74")
			(jidaikobo-wheat             "#c4be89")
			(jidaikobo-olive             "#808000")
			(jidaikobo-chartreuse        "#a6e22e")
			(jidaikobo-lime              "#00ff00")
			(jidaikobo-yellowgreen       "#83a300")
			(jidaikobo-green             "#008000")
			(jidaikobo-darkwine          "#1e0010")
			(jidaikobo-maroon            "#800000")
			(jidaikobo-wine              "#960050")
			(jidaikobo-teal              "#008080")
			(jidaikobo-aqua              "#00ffff")
			(jidaikobo-blue-3            "#88ddff")
			(jidaikobo-blue-2            "#77e9ef")
			(jidaikobo-blue-1            "#66d9ef")
			(jidaikobo-blue              "#0064ff")
			(jidaikobo-dodgerblue        "#13354a")
			(jidaikobo-slateblue         "#7070f0")
			(jidaikobo-purple-1          "#7f58c6")
			(jidaikobo-purple            "#7b4eff")
			(jidaikobo-palevioletred     "#d33682")
			(jidaikobo-grey-7            "#f0f0f0")
			(jidaikobo-grey-6            "#e0e0e0")
			(jidaikobo-grey-5            "#d0d0d0")
			(jidaikobo-grey-4            "#c0c0c0")
			(jidaikobo-grey-3            "#b0b0b0")
			(jidaikobo-grey-2            "#a0a0a0")
			(jidaikobo-grey-1            "#909090")
			(jidaikobo-grey              "#808080")
			(jidaikobo-grey+1            "#707070")
			(jidaikobo-grey+2            "#606060")
			(jidaikobo-grey+3            "#505050")
			(jidaikobo-grey+4            "#404040")
			(jidaikobo-grey+5            "#303030")
			(jidaikobo-grey+6            "#202020")
			(jidaikobo-grey+7            "#101010")
			(jidaikobo-dark              "#000000")
			(jidaikobo-black             "#000000"))

	;; custom-theme-set-faces
	(custom-theme-set-faces
	 'jidaikobo-dark
	 ;; M-x list-color-displayでカラーコード一覧表表示

	 ;; base
	 `(default   ((t (:background ,jidaikobo-bg :foreground ,jidaikobo-fg))))
	 `(cursor    ((t (:background ,jidaikobo-fg :foreground ,jidaikobo-bg))))
	 `(fringe    ((t (:foreground ,jidaikobo-bg :background ,jidaikobo-bg)))) ;; フレームの余白
	 `(highlight ((t (:background ,jidaikobo-grey))))
	 `(region    ((t (:background  ,jidaikobo-grey+1))
								(t :inverse-video t)))
	 `(warning   ((t (:foreground ,jidaikobo-palevioletred :weight bold))))

	 ;; font lock

	 ;; コメント
	 `(font-lock-comment-face ((t (:background ,jidaikobo-bg :foreground ,jidaikobo-orange-1))))
	 `(font-lock-comment-delimiter-face ((t (:background ,jidaikobo-bg :foreground ,jidaikobo-orange-1))))

	 ;; ビルトイン関数
	 `(font-lock-builtin-face ((t (:foreground ,jidaikobo-aqua))))

	 ;; 関数名
	 `(font-lock-function-name-face ((t (:foreground ,jidaikobo-chartreuse))))

	 ;; 変数名
	 `(font-lock-variable-name-face ((t (:foreground ,jidaikobo-yellowgreen))))
	 `(font-lock-warning-face ((t (:foreground ,jidaikobo-palevioletred :weight bold))))

	 ;; 定数
	 `(font-lock-constant-face ((t (:foreground ,jidaikobo-wheat))))
	 `(font-lock-doc-string-face ((t (:foreground ,jidaikobo-darkgoldenrod))))

	 ;; キーワード
	 `(font-lock-keyword-face ((t (:foreground ,jidaikobo-blue-1))))

	 ;; for easily-overlooked negation characters.
	 `(font-lock-negation-char-face ((t (:foreground ,jidaikobo-wine))))

	 ;; 数値
	 `(font-lock-number-face ((t (:foreground,jidaikobo-blue))))
	 `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
	 `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
	 `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))

	 ;; 文字列
	 `(font-lock-string-face ((t (:foreground ,jidaikobo-red-1))))

	 ;; データ型名
	 `(font-lock-type-face ((t (:foreground ,jidaikobo-blue))))

	 ;; mode line
	 `(mode-line ((t (:foreground ,jidaikobo-dark
																:background ,jidaikobo-wheat
																:box nil))))
	 `(mode-line-buffer-id ((t (:weight bold))))
	 `(mode-line-inactive ((t (:foreground ,jidaikobo-grey+3
																				 :background ,jidaikobo-bg
																				 :box nil))))
	 `(mode-line-read-only-face ((t (:foreground ,jidaikobo-dark
																							 :background ,jidaikobo-red
																							 :box nil))))

	 `(header-line ((t (:foreground ,jidaikobo-bg
																	:background ,jidaikobo-wheat
																	:weight bold))))

	 ;; search
	 `(isearch ((t (:foreground ,jidaikobo-dark :background ,jidaikobo-wheat :weight bold))))
	 `(isearch-fail ((t (:foreground ,jidaikobo-wine :background ,jidaikobo-darkwine))))

	 ;; linum-mode
	 `(linum ((t (:foreground ,jidaikobo-grey+3 :background ,jidaikobo-bg))))

	 ;; hl-line-mode
	 `(hl-line-face ((,class (:background ,jidaikobo-grey+3)) (t :weight bold)))
	 `(hl-line ((,class (:background ,jidaikobo-grey+3)) (t :weight bold)))

	 ;; TODO
	 ;; ido-mode
	 ;; flycheck
	 ;; show-paren
	 ;; rainbow-delimiters
	 ;; highlight-symbols
	 ))

;;; ------------------------------------------------------------
;;; Provide

;;;###autoload
(when load-file-name
	(add-to-list 'custom-theme-load-path
							 (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jidaikobo-dark)


;;; jidaikobo-dark.el ends here
