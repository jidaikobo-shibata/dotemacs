;;; web-select-html-ele.el --- Select an HTML element or its content -*- lexical-binding: t; -*-

;;; Commentary:
;; Select the innermost HTML element at point with `s-A'.  Repeating the
;; command toggles between the whole element and its content.  Tag matching
;; is independent of the current major mode.

;;; Code:

(defconst web-select-html-ele--void-elements
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link"
    "meta" "param" "source" "track" "wbr")
  "HTML elements that do not have an end tag.")

(defconst web-select-html-ele--raw-text-elements
  '("script" "style" "textarea" "title")
  "Elements whose content must not be interpreted as HTML tags.")

(defconst web-select-html-ele--tag-start-regexp
  "<!--\\|<\\(/?\\)\\([[:alpha:]][[:alnum:]_.:-]*\\)\\b"
  "Regexp matching the start of an HTML comment or tag.")

(defvar-local web-select-html-ele--last-element nil
  "Element bounds used by the most recent selection command.")

(defvar-local web-select-html-ele--last-kind nil
  "Selection kind produced most recently: `whole' or `content'.")

(defvar-local web-select-html-ele--last-region nil
  "Region bounds produced by the most recent selection command.")

(defun web-select-html-ele--tag-end ()
  "Move past the current tag's closing `>' and return point.
Point must initially be after the tag name.  Quoted attribute values are
skipped, so a `>' within an attribute does not end the tag."
  (catch 'end
    (while (re-search-forward "\"[^\"]*\"\\|'[^']*'\\|>" nil t)
      (when (eq (char-before) ?>)
        (throw 'end (point))))
    nil))

(defun web-select-html-ele--self-closing-p (name tag-beg tag-end)
  "Return non-nil when NAME from TAG-BEG to TAG-END is an empty element."
  (or (member name web-select-html-ele--void-elements)
      (string-match-p "/[[:space:]]*>\\'"
                      (buffer-substring-no-properties tag-beg tag-end))))

(defun web-select-html-ele--raw-element (name opening-beg opening-end)
  "Find raw-text NAME after OPENING-END and return its element bounds."
  (goto-char opening-end)
  (let ((case-fold-search t)
        (closing-regexp
         (concat "</[[:space:]]*" (regexp-quote name)
                 "[[:space:]]*>")))
    (when (re-search-forward closing-regexp nil t)
      (list :whole (cons opening-beg (match-end 0))
            :content (cons opening-end (match-beginning 0))))))

(defun web-select-html-ele--elements ()
  "Return all complete HTML element boundaries in the current buffer.
Each result is a plist containing `:whole' and, where applicable,
`:content' bounds.  Parsing uses regular expressions and does not depend
on the current major mode."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          stack
          elements)
      (while (re-search-forward web-select-html-ele--tag-start-regexp nil t)
        (let ((tag-beg (match-beginning 0)))
          (if (equal (match-string-no-properties 0) "<!--")
              (unless (search-forward "-->" nil t)
                (goto-char (point-max)))
            (let* ((closing-p (equal (match-string-no-properties 1) "/"))
                   (name (downcase (match-string-no-properties 2)))
                   (tag-end (web-select-html-ele--tag-end)))
              (if (null tag-end)
                  (goto-char (point-max))
                (cond
                 (closing-p
                  (let ((matching
                         (catch 'match
                           (dolist (opening stack)
                             (when (equal name (plist-get opening :name))
                               (throw 'match opening))))))
                    (when matching
                      (push (list :whole
                                  (cons (plist-get matching :beg) tag-end)
                                  :content
                                  (cons (plist-get matching :end) tag-beg))
                            elements)
                      (setq stack (cdr (memq matching stack))))))
                 ((web-select-html-ele--self-closing-p
                   name tag-beg tag-end)
                  (push (list :whole (cons tag-beg tag-end)) elements))
                 ((member name web-select-html-ele--raw-text-elements)
                  (let ((element
                         (web-select-html-ele--raw-element
                          name tag-beg tag-end)))
                    (when element
                      (push element elements))))
                 (t
                  (push (list :name name :beg tag-beg :end tag-end)
                        stack))))))))
      elements)))

(defun web-select-html-ele--region-equal-p (bounds)
  "Return non-nil when the active region is exactly BOUNDS."
  (and bounds
       (use-region-p)
       (= (region-beginning) (car bounds))
       (= (region-end) (cdr bounds))))

(defun web-select-html-ele--selected-state (elements)
  "Return the element and kind matching the active region.
An exact whole-element match takes precedence over a content match."
  (when (use-region-p)
    (or (catch 'selected
          (dolist (element elements)
            (when (web-select-html-ele--region-equal-p
                   (plist-get element :whole))
              (throw 'selected (cons element 'whole)))))
        (catch 'selected
          (dolist (element elements)
            (when (web-select-html-ele--region-equal-p
                   (plist-get element :content))
              (throw 'selected (cons element 'content))))))))

(defun web-select-html-ele--continued-state (elements)
  "Return the saved element and kind when this is a valid continuation.
ELEMENTS contains the freshly parsed element boundaries for the buffer."
  (when (and (eq last-command 'select-html-element-at-caret)
             (use-region-p)
             web-select-html-ele--last-element
             web-select-html-ele--last-kind
             (equal web-select-html-ele--last-region
                    (cons (region-beginning) (region-end))))
    (let ((whole (plist-get web-select-html-ele--last-element :whole)))
      (catch 'found
        (dolist (element elements)
          (when (equal (plist-get element :whole) whole)
            (throw 'found
                   (cons element web-select-html-ele--last-kind))))))))

(defun web-select-html-ele--element-at (position elements)
  "Return the innermost member of ELEMENTS containing POSITION."
  (let (found)
    (dolist (element elements)
      (let ((whole (plist-get element :whole)))
        (when (and (<= (car whole) position)
                   (< position (cdr whole))
                   (or (null found)
                       (> (car whole)
                          (car (plist-get found :whole)))))
          (setq found element))))
    found))

(defun web-select-html-ele--select (bounds)
  "Select BOUNDS, a cons cell of beginning and end positions."
  (goto-char (car bounds))
  (set-mark (cdr bounds))
  (activate-mark))

(defun web-select-html-ele--remember (element kind bounds)
  "Remember that ELEMENT was selected as KIND using BOUNDS."
  (setq web-select-html-ele--last-element element
        web-select-html-ele--last-kind kind
        web-select-html-ele--last-region (cons (car bounds) (cdr bounds))))

(defun select-html-element-at-caret ()
  "Select the innermost HTML element at point or toggle its content.

When the whole element is selected, select only its content.  When only
the content is selected, select the whole element.  Otherwise, select the
innermost element containing point."
  (interactive)
  (let* ((elements (web-select-html-ele--elements))
         (state (or (web-select-html-ele--continued-state elements)
                    (web-select-html-ele--selected-state elements)))
         (element (or (car-safe state)
                      (web-select-html-ele--element-at (point) elements)))
         (kind (cdr-safe state))
         (whole (plist-get element :whole))
         (content (plist-get element :content))
         target
         target-kind)
    (cond
     ((null element)
      (user-error "No enclosing HTML element found"))
     ((eq kind 'whole)
      (setq target (or content whole)
            target-kind (if content 'content 'whole)))
     ((eq kind 'content)
      (setq target whole
            target-kind 'whole))
     (t
      (setq target whole
            target-kind 'whole)))
    (web-select-html-ele--select target)
    (web-select-html-ele--remember element target-kind target)))

(global-set-key (kbd "s-A") #'select-html-element-at-caret)

;;; ------------------------------------------------------------
;;; Provide

(provide 'web-select-html-ele)

;;; web-select-html-ele.el ends here
