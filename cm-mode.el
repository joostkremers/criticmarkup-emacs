;;; cm-mode.el --- Minor mode for CriticMarkup

;; Copyright (c) 2013-2024 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 14 Feb 2013
;; Version: 1.8
;; Keywords: text, markdown
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; CriticMarkup for Emacs
;; ======================
;;
;; `cm-mode' is a minor mode that provides support for CriticMarkup in Emacs.
;;
;; CriticMarkup is a way for authors and editors to track changes to
;; documents in plain text.  It defines the following patterns for marking
;; changes:
;;
;; - Addition {++ ++}
;; - Deletion {-- --}
;; - Substitution {~~ ~> ~~}
;; - Comment {>> <<}
;; - Highlight {== ==}{>> <<}
;;
;; `cm-mode' provides the following functionality:
;;
;; - font lock support
;; - key bindings to insert CriticMarkup.
;; - 'follow changes' mode: automatically record changes to the buffer.
;; - accept/reject changes interactively.
;; - automatically add author tag.
;; - navigation to move between changes.
;;
;;
;; Key bindings
;; ------------
;;
;; `cm-mode' provides the following key bindings:
;;
;; `C-c * a' : add text
;; `C-c * d' : delete text
;; `C-c * s' : make a substitution
;; `C-c * c' : add a comment
;; `C-c * i' : accept/reject change at point
;; `C-c * I' : accept/reject all changes interactively
;; `C-c * *' : move forward out of a change
;; `C-c * f' : move forward to the next change
;; `C-c * b' : move backward to the previous change
;; `C-c * t' : set author
;; `C-c * F' : activate follow changes mode
;;
;; The `C-c *' prefix can easily be changed, if so desired:
;;
;; (define-key cm-mode-map (kbd "C-c *") nil)
;; (define-key cm-mode-map (kbd "C-c c") 'cm-prefix-map)
;;
;; This unbinds `C-c *' and sets up `C-c c' as the prefix for all cm-mode
;; commands.
;;
;; Usage
;; -----
;;
;; See README.md for details.

;;; Code:

(require 'thingatpt)
(require 'cl-lib)

(defvar cm-follow-changes nil
  "Flag indicating whether follow changes mode is active.")
(make-variable-buffer-local 'cm-follow-changes)

(defvar cm-current-deletion nil
  "The deleted text in follow changes mode.
The value is actually a list consisting of the text and a flag
indicating whether the deletion was done with the backspace
key.")

(defvar cm-addition-regexp "\\(?:{\\+\\+\\(\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\)\\+\\+}\\)"
  "CriticMarkup addition regexp.")

(defvar cm-deletion-regexp "\\(?:{--\\(\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\)--}\\)"
  "CriticMarkup deletion regexp.")

(defvar cm-substitution-regexp "\\(?:{~~\\(\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\)~>\\(\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\)~~}\\)"
  "CriticMarkup substitution regexp.")

(defvar cm-comment-regexp "\\(?:{>>\\(\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\)<<}\\)"
  "CriticMarkup comment regexp.")

(defvar cm-highlight-regexp "\\(?:{==\\(\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\)==}\\)"
  "CriticMarkup highlight regexp.")

(defvar cm-current-markup-overlay nil
  "Overlay marking the current highlight.")
(make-variable-buffer-local 'cm-current-markup-overlay)

(defgroup criticmarkup nil "Minor mode for CriticMarkup."
  :prefix "cm-"
  :group 'wp
  :group 'markdown)

(defgroup criticmarkup-faces nil "Faces for CriticMarkup."
  :prefix "cm-"
  :group 'criticmarkup)

(defcustom cm-author nil
  "Author tag.
If set, each change is automatically marked with a comment
containing this tag.

The tag should not contain spaces.  Do not include the `@' sign,
it is added automatically."
  :group 'criticmarkup
  :safe 'stringp
  :type '(choice (const :tag "None" nil)
                 (string :tag "Author")))
(make-variable-buffer-local 'cm-author)

(defcustom cm-read-only-annotations t
  "Make annotations read-only.
By default, annotation markers are read-only, so they cannot be
overwritten.  This interferes with reformatting, however, so you
can disable this behaviour.  Note that if you change the value of
this variable for a particular buffer, you may need to deactivate
and reactivate `cm-mode'."
  :group 'criticmarkup
  :safe 'booleanp
  :type 'boolean)
(make-variable-buffer-local 'cm-read-only-annotations)

(defface cm-addition-face '((t (:inherit success)))
  "Face for CriticMarkup additions."
  :group 'criticmarkup-faces)

(defface cm-deletion-face '((t (:inherit error)))
  "Face for CriticMarkup deletions."
  :group 'criticmarkup-faces)

(defface cm-substitution-face '((t (:inherit font-lock-warning-face)))
  "Face for CriticMarkup substitutions."
  :group 'criticmarkup-faces)

(defface cm-comment-face '((t (:inherit font-lock-comment-face)))
  "Face for CriticMarkup comments."
  :group 'criticmarkup-faces)

(defface cm-highlight-face '((t (:inherit highlight)))
  "Face for CriticMarkup highlights."
  :group 'criticmarkup-faces)

(defvar cm-addition-face 'cm-addition-face
  "CriticMarkup addition face.")

(defvar cm-deletion-face 'cm-deletion-face
  "CriticMarkup deletion face.")

(defvar cm-substitution-face 'cm-substitution-face
  "CriticMarkup substitution face.")

(defvar cm-comment-face 'cm-comment-face
  "CriticMarkup comment face.")

(defvar cm-highlight-face 'cm-highlight-face
  "CriticMarkup highlight face.")

;;; Create markup predicates.

(eval-and-compile
  (defvar cm-delimiters '((cm-addition "{++" "++}")
                          (cm-deletion "{--" "--}")
                          (cm-substitution "{~~" "~>" "~~}")
                          (cm-comment "{>>" "<<}")
                          (cm-highlight "{==" "==}"))
    "CriticMarkup delimiters."))

(eval-and-compile
  (mapc (lambda (markup)
          (fset (intern (concat (symbol-name markup) "-p"))
                `(lambda (change)
                   (eq (car change) (quote ,markup)))))
        (mapcar #'car cm-delimiters)))

(defvar cm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c *") 'cm-prefix-map)
    map)
  "Keymap for `cm-mode'.
This keymap contains only one binding: `C-c *', which is bound to
`cm-prefix-map', the keymap that holds the actual key bindings.")

(defvar cm-prefix-map)  ; Mainly to silence the byte compiler.
(define-prefix-command 'cm-prefix-map)
(define-key cm-prefix-map "a" #'cm-addition)
(define-key cm-prefix-map "d" #'cm-deletion)
(define-key cm-prefix-map "s" #'cm-substitution)
(define-key cm-prefix-map "c" #'cm-comment)
(define-key cm-prefix-map "i" #'cm-accept/reject-change-at-point)
(define-key cm-prefix-map "I" #'cm-accept/reject-all-changes)
(define-key cm-prefix-map "*" #'cm-forward-out-of-change)
(define-key cm-prefix-map "f" #'cm-forward-change)
(define-key cm-prefix-map "b" #'cm-backward-change)
(define-key cm-prefix-map "t" #'cm-set-author)
(define-key cm-prefix-map "F" #'cm-follow-changes)

(defvar cm-mode-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'cm-forward-change)
    (define-key map (kbd "b") #'cm-backward-change)
    map)
  "Repeat keymap for `cm-mode'.")
(put 'cm-forward-change 'repeat-map 'cm-mode-repeat-map)
(put 'cm-backward-change 'repeat-map 'cm-mode-repeat-map)

(easy-menu-define cm-mode-menu cm-mode-map "CriticMarkup Menu."
  '("CriticMarkup"
    ["Addition" cm-addition t]
    ["Deletion" cm-deletion t]
    ["Substitution" cm-substitution t]
    ["Comment" cm-comment t]
    "--"
    ["Accept/Reject Change" cm-accept/reject-change-at-point t]
    ["Accept/Reject All Changes" cm-accept/reject-all-changes t]
    "--"
    ["Move To Next Change" cm-forward-change t]
    ["Move To Previous Change" cm-backward-change t]
    "--"
    ["Set Author" cm-set-author t]))

;;;###autoload
(define-minor-mode cm-mode
  "Minor mode for CriticMarkup."
  :init-value nil :lighter (:eval (concat " CM" (if cm-author (concat "@" cm-author)) (if cm-follow-changes "*"))) :global nil
  (cond
   (cm-mode                             ; `cm-mode' is turned on.
    (setq font-lock-multiline t)
    (font-lock-add-keywords nil (cm-font-lock-keywords) t)
    (when cm-read-only-annotations
      (add-to-list 'font-lock-extra-managed-props 'read-only))
    (add-to-list 'font-lock-extra-managed-props 'rear-nonsticky)
    (font-lock-ensure)
    (setq cm-current-markup-overlay (make-overlay 1 1))
    (overlay-put cm-current-markup-overlay 'face 'highlight))
   ((not cm-mode)                       ; `cm-mode' is turned off.
    (font-lock-remove-keywords nil (cm-font-lock-keywords))
    (setq font-lock-extra-managed-props (delq 'read-only (delq 'rear-nonsticky font-lock-extra-managed-props)))
    (let ((modified (buffer-modified-p)))
      (cm-make-markups-writable)  ; We need to remove the read-only property by hand; it's cumbersome to do it with font-lock.
      (unless modified
        (set-buffer-modified-p nil)))  ; Removing text properties marks the buffer as modified, so we may need to adjust.
    (font-lock-ensure)
    (remove-overlays))))

;;; Font lock

(defun cm-font-lock-for-markup (type)
  "Create a font lock entry for markup TYPE."
  (let ((markup (cdr type))
        (face (intern (concat (symbol-name (car type)) "-face")))
        font-lock)
    (add-to-list 'font-lock (mapconcat #'(lambda (elt)  ; First we create the regexp to match.
                                           (regexp-opt (list elt) t))
                                       markup
                                       "\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?"))
    (add-to-list 'font-lock `(0 ,face prepend) t)  ; The highlighter for the entire change.
    (dotimes (n (length markup))
      (when cm-read-only-annotations
	(add-to-list 'font-lock `(,(1+ n) '(face ,face read-only t)) t)  ; Make the tags read-only.
	(add-to-list 'font-lock `("." (progn  ; And make the read-only property of the final character rear-nonsticky
					(goto-char (1- (match-end ,(1+ n))))
					(1+ (point)))
				  nil
				  (0 '(face ,face rear-nonsticky (read-only))))
                     t)))
    font-lock))

;; `cm-font-lock-for-markup' produces a font-lock entry that can be given
;; to `font-lock-add-keywords'. To illustrate, the entry it produces for
;; additions is the following:
;; ("\\({\\+\\+\\)\\(?:[[:ascii:]]\\|[[:nonascii:]]\\)*?\\(\\+\\+}\\)"
;;  (0 cm-addition-face prepend)
;;  (1 '(face cm-addition-face read-only t))
;;  ("." (progn (goto-char (1- (match-end 1)))
;;              (1+ (point)))
;;   nil
;;   (0 '(face cm-addition-face rear-nonsticky (read-only))))
;;  (2 '(face cm-addition-face read-only t))
;;  ("." (progn (goto-char (1- (match-end 2)))
;;              (1+ (point)))
;;   nil
;;   (0 '(face cm-addition-face rear-nonsticky (read-only)))))
;;
;; This does some nice magic: it highlights addition markups with
;; cm-addition-face, it makes the tags themselves, `{++' and `++}', read-only, and
;; it gives the last character of the tags the text property (rear-nonsticky
;; (read-only)), so that it's possible to add characters after the tag.

(defun cm-font-lock-keywords ()
  "Return a list of font lock keywords."
  (mapcar #'cm-font-lock-for-markup cm-delimiters))

;;; Follow Changes

(defun cm-follow-changes (&optional arg)
  "Activate follow changes mode.
If ARG is positive, activate follow changes mode, if ARG is 0 or
negative, deactivate it.  If ARG is `toggle', toggle follow
changes mode."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable (if (eq arg 'toggle)
                    (not cm-follow-changes)
                  (> (prefix-numeric-value arg) 0))))
    (if enable
        (progn
          (add-to-list 'before-change-functions 'cm-before-change t)
          (add-to-list 'after-change-functions 'cm-after-change)
          (setq cm-follow-changes t)
          (message "Follow changes mode activated."))
      (setq before-change-functions (delq 'cm-before-change before-change-functions))
      (setq after-change-functions (delq 'cm-after-change after-change-functions))
      (setq cm-follow-changes nil)
      (message "Follow changes mode deactivated."))))

(defun cm-before-change (beg end)
  "Function to execute before a buffer change.
BEG and END are the beginning and the end of the region to be
changed."
  (unless (or undo-in-progress
              (and (= beg (point-min)) (= end (point-max))))  ; This happens on buffer switches.
    (if (= beg end)  ; Addition.
        (cm-make-addition (cm-markup-at-point))
      ;; When the deletion was done with backspace, point is at end.
      (setq cm-current-deletion (list (buffer-substring beg end) (= (point) end))))))

(defun cm-after-change (beg end length)
  "Function to execute after a buffer change.
This function marks deletions.  See cm-before-change for details.
BEG and END mark the region to be changed, LENGTH is the length
of the affected text."
  (unless (or undo-in-progress
              (not cm-current-deletion))
    (apply #'cm-make-deletion cm-current-deletion)
    (setq cm-current-deletion nil)))

(defmacro cm-without-following-changes (&rest body)
  "Execute BODY without following changes."
  (declare (indent defun))
  `(let ((inhibit-modification-hooks t))
     ,@body))

(defun cm-make-markups-writable ()
  "Make all CM markup delimiters in the current buffer writable."
  (save-excursion
    (goto-char (point-min))
    (let ((delims-regexp (concat (regexp-opt (mapcar #'cl-second cm-delimiters) t)
                                 "\\([[:ascii:]]\\|[[:nonascii:]]\\)*?"
                                 "\\(?:\\(~>\\)\\([[:ascii:]]\\|[[:nonascii:]]\\)*?\\)?"
                                 (regexp-opt (mapcar (lambda (e) (car (last e))) cm-delimiters) t)))
          (inhibit-read-only t))
      (while (re-search-forward delims-regexp nil t)
        (dolist (n '(1 2 3))
          (when (match-string n)
            (remove-text-properties (match-beginning n) (match-end n) '(read-only nil rear-nonsticky nil))))))))

(defun cm-insert-markup (type &optional text)
  "Insert CriticMarkup of TYPE.
Also insert TEXT if non-nil.  For deletions, TEXT is the deleted
text; for substitutions, the text to be substituted; for
comments, the text to be highlighted.

If `cm-author' is set, a comment is added with its value,
preceded with `@'.

If TYPE is `cm-highlight', a comment is added, which optionally
starts with `cm-author'."
  (let* ((delims (cdr (assq type cm-delimiters)))
         (bdelim (cl-first delims))
         (middle (if (cl-third delims) (cl-second delims))) ; "~>" for cm-substitution, otherwise nil
         (edelim (car (last delims))))
    (insert (or bdelim "")
            (or text (if (and (eq type 'cm-comment)
                              cm-author)
                         (concat "@" cm-author " ")
                       ""))
            (or middle "")
            (or edelim "")))
  (if (and (not (eq type 'cm-comment))
           (or cm-author (eq type 'cm-highlight)))
      (insert "{>>"
              (if cm-author (concat "@" cm-author))
              (if (and (eq type 'cm-highlight)
                       cm-author)
                  " "
                "")
              "<<}")))

;; Making an addition is fairly simple: we just need to add markup if point
;; isn't already at an addition markup, and then position point
;; appropriately. The user can then type new text. A deletion is more
;; difficult, because it also needs to (re)insert the deleted text and do
;; something sensible with point. This is especially difficult in follow
;; changes mode, because the deletion may be made with DEL or BACKSPACE.

(defun cm-addition ()
  "Make an addition at point.
If point is at an addition markup already, the new addition is
combined with it.  If point is inside any other markup, no
addition can be made."
  (interactive)
  (let ((change (cm-markup-at-point)))
    (if (or (not (cm-point-inside-change-p change))
            (cm-addition-p change))
        (cm-without-following-changes
          (cm-make-addition change))
      (error "Cannot make an addition here"))))

(defun cm-deletion (beg end)
  "Mark text for deletion.
BEG and END delimit the region to be deleted."
  (interactive "r")
  (let ((change (cm-markup-at-point)))
    (when (cm-point-inside-change-p change)
      (error "Cannot make a deletion here"))  ; TODO We should check whether the region contains markup.
    (when (use-region-p)
      (cm-without-following-changes
        (cm-make-deletion (delete-and-extract-region beg end))))))

(defun cm-make-addition (change)
  "Position point for an addition and insert addition markup if necessary.
CHANGE is the change markup at point, if any, as returned by
cm-markup-at-point.  If this is an addition, the new addition is
combined with it, even if point is right outside it.  This avoids
having two additions adjacent to each other.  If it is another
kind of markup, and point is inside the curly braces, we make
sure point is not in the delimiter before adding text."
  (setq change (cm-merge-comment change))
  (if (or (cm-point-inside-change-p change)
          (and (cm-addition-p change)
               (cm-has-current-author-p change)))
      (cm-move-into-markup 'cm-addition)
    (cm-insert-markup 'cm-addition)
    (cm-move-into-markup 'cm-addition t)))

(defun cm-make-deletion (text &optional backspace)
  "Reinsert TEXT into the buffer and add deletion markup if necessary.
TEXT is the text that's being deleted.

If BACKSPACE is t, the deletion was done with the backspace key;
point will then be left before the deletion markup."
  ;; TODO: We should check whether the text to be deleted contains part of
  ;; a change.
  (let ((change (cm-merge-comment (cm-markup-at-point))))
    (unless (cm-point-inside-change-p change)
      (save-excursion
        (if (not (and (cm-deletion-p change)
                      (cm-has-current-author-p change)))
            (cm-insert-markup 'cm-deletion text)
          (cm-move-into-markup 'cm-deletion)
          (insert text)))
      ;; `save-excursion' leaves point at the start of the deletion markup.
      (unless backspace
        (cm-forward-out-of-change)))))

(defun cm-substitution (beg end)
  "Mark a substitution.
BEG and END delimit the text to be substituted."
  (interactive "r")
  (when (cm-point-inside-change-p (cm-markup-at-point))
    (error "Cannot make a substitution here"))  ; TODO We should check whether the region contains markup.
  (cm-without-following-changes
    (let ((text (delete-and-extract-region beg end)))
      (cm-insert-markup 'cm-substitution text)
      (cm-move-into-markup 'cm-substitution))))

(defun cm-comment (&optional beg end)
  "Add a comment.
If the region is active, the text in the region as delimited by
BEG and END, is highlighted.  If point is in an existing change,
the comment is added after it."
  (interactive "r")
  (cm-without-following-changes
    (let ((change (cm-markup-at-point))
          text)
      (if (or (cm-comment-p change)
              (cm-highlight-p change))
          (error "Cannot make a comment here")
        (cond
         (change
          (cm-end-of-markup (car change)))
         ;; Note: we do not account for the possibility that the region
         ;; contains a change but point is outside of it...
         ((use-region-p)
          (setq text (delete-and-extract-region beg end))))
        (if text
            (cm-insert-markup 'cm-highlight text)
          (cm-insert-markup 'cm-comment))
        (cm-move-into-markup 'cm-comment)))))

(defun cm-point-at-delim (delim &optional end strict)
  "Return non-nil if point is at delimiter DELIM.
DELIM should be one of the strings in `cm-delimiters'.  If DELIM
is an end delimiter, optional argument END must be t.

Point counts as being at DELIM if it is in a delimiter or
directly outside, but not when it is directly inside.  So `|{++',
`{|++', `{+|+', return 0, 1, and 2 respectively, while `{++|'
returns nil.  Similarly, `++}|', `++|}', `+|+}' return 0, 1, and
2, while `|++}' returns nil.

If STRICT is non-nil, point must be inside the delimiter.  That
is, instead of 0, the return value will be nil."
  (save-excursion
    (if end
        (let ((distance (skip-chars-forward (substring delim 1) (+ (point) 2))))
          (if (looking-back (regexp-quote delim) (- (point) 3))
              (if (> distance 0)
                  distance
                (and (not strict) 0))))
      (let ((distance (skip-chars-backward (substring delim 0 -1) (- (point) 2))))
        (if (looking-at (regexp-quote delim))
            (if (< distance 0)
                (abs distance)
              (and (not strict) 0)))))))

(defun cm-forward-markup (type &optional n)
  "Move forward to the next markup of TYPE.
Optional argument N indicates how many markups to move.  If N is
negative, move backward.  If point is inside a delimiter, this
function moves point to the previous/next markup.  If point is
inside a markup, it moves to the edge.  If point is at the edge
of a markup, it moves to the end of the next markup of the same
type."
  (or n (setq n 1))
  (cond
   ((> n 0)  ; Moving forward.
    (let ((delim (car (last (assq type cm-delimiters)))))
      (backward-char (- (length delim) (or (cm-point-at-delim delim t t)
                                           (length delim))))  ; Adjust point if it's inside a delimiter.
      (re-search-forward (regexp-quote delim) nil t n)))
   (t  ; Moving backward.
    (let ((delim (cl-second (assq type cm-delimiters))))
      (forward-char (- (length delim) (or (cm-point-at-delim delim nil t)
                                          (length delim))))  ; Adjust point if it's inside a delimiter.
      (re-search-backward (regexp-quote delim) nil t (abs n))))))

(defun cm-beginning-of-markup (type)
  "Move to the beginning of a markup of TYPE."
  ;; First move out of the delimiter, if we're in one.
  (cm-move-past-delim (cl-second (assq type cm-delimiters)))
  (cm-forward-markup type -1))

(defun cm-end-of-markup (type)
  "Move to the end of a markup of TYPE."
  ;; First move out of the delimiter, if we're in one.
  (cm-move-past-delim (car (last (assq type cm-delimiters))) t)
  (cm-forward-markup type))

(defun cm-move-past-delim (delim &optional end)
  "Move point past DELIM into the markup.
DELIM must be one of the strings in `cm-delimiters'.  If DELIM is
an end delimiter, END must be t. If point is not at a delimiter,
do not move.  Return t if point has moved."
  (let ((len (length delim))
        (pos (point)))
    (if end
        (backward-char (- len (or (cm-point-at-delim delim end)
                                  len)))
      (forward-char (- len (or (cm-point-at-delim delim)
                               len))))
    (/= pos (point))))

(defun cm-move-into-markup (type &optional backwards)
  "Make sure point is inside the delimiters of TYPE.
Point is either moved forward if at an opening delimiter or
backward if at a closing delimiter.  When moving backward, point
is moved past a comment if the change before the comment is of
TYPE.

If BACKWARDS is t, only try moving backwards."
  (unless (and (not backwards)
               (cm-move-past-delim (cl-second (assq type cm-delimiters))))
    (if (and (not (eq type 'cm-comment))
             (cm-comment-p (cm-markup-at-point t)))
        (cm-forward-markup 'cm-comment -1))
    (cm-move-past-delim (car (last (assq type cm-delimiters))) t)))

(defun cm-forward-addition (&optional n)
  "Move forward N addition markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-addition n))

(defun cm-beginning-of-addition ()
  "Move to the beginning of an addition."
  (cm-beginning-of-markup 'cm-addition))

(defun cm-end-of-addition ()
  "Move to the end of an addition."
  (cm-end-of-markup 'cm-addition))

(put 'cm-addition 'forward-op 'cm-forward-addition)
(put 'cm-addition 'beginning-op 'cm-beginning-of-addition)
(put 'cm-addition 'end-op 'cm-end-of-addition)

(defun cm-forward-deletion (&optional n)
  "Move forward N deletion markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-deletion n))

(defun cm-beginning-of-deletion ()
  "Move to the beginning of a deletion."
  (cm-beginning-of-markup 'cm-deletion))

(defun cm-end-of-deletion ()
  "Move to the end of a deletion."
  (cm-end-of-markup 'cm-deletion))

(put 'cm-deletion 'forward-op 'cm-forward-deletion)
(put 'cm-deletion 'beginning-op 'cm-beginning-of-deletion)
(put 'cm-deletion 'end-op 'cm-end-of-deletion)

(defun cm-forward-substitution (&optional n)
  "Move forward N substitution markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-substitution n))

(defun cm-beginning-of-substitution ()
  "Move to the beginning of a substitution."
  (cm-beginning-of-markup 'cm-substitution))

(defun cm-end-of-substitution ()
  "Move to the end of a substitution."
  (cm-end-of-markup 'cm-substitution))

(put 'cm-substitution 'forward-op 'cm-forward-substitution)
(put 'cm-substitution 'beginning-op 'cm-beginning-of-substitution)
(put 'cm-substitution 'end-op 'cm-end-of-substitution)

(defun cm-forward-comment (&optional n)
  "Move forward N comment markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-comment n))

(defun cm-beginning-of-comment ()
  "Move to the beginning of a comment."
  (cm-beginning-of-markup 'cm-comment))

(defun cm-end-of-comment ()
  "Move to the end of a comment."
  (cm-end-of-markup 'cm-comment))

(put 'cm-comment 'forward-op 'cm-forward-comment)
(put 'cm-comment 'beginning-op 'cm-beginning-of-comment)
(put 'cm-comment 'end-op 'cm-end-of-comment)

(defun cm-forward-highlight (&optional n)
  "Move forward N highlight markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-highlight n))

(defun cm-beginning-of-highlight ()
  "Move to the beginning of a highlight."
  (cm-beginning-of-markup 'cm-highlight))

(defun cm-end-of-highlight ()
  "Move to the end of a highlight."
  (cm-end-of-markup 'cm-highlight))

(put 'cm-highlight 'forward-op 'cm-forward-highlight)
(put 'cm-highlight 'beginning-op 'cm-beginning-of-highlight)
(put 'cm-highlight 'end-op 'cm-end-of-highlight)

(defun cm-bounds-of-markup-at-point (type)
  "Return the bounds of markup TYPE at point.
The return value is a list of the form (START-POS END-POS).  If
point is not within a markup of TYPE, return nil.

TYPE is one of `cm-addition', `cm-deletion', `cm-substitution',
`cm-comment', or `cm-highlight'.  Note that in the case of
comments, only the comment is returned, any preceding highlight
is ignored.  The same holds for highlights: the following comment
is not included."
  (if (thing-at-point type)
      (let ((beg (save-excursion
                   (cm-beginning-of-markup type)
                   (point)))
            (end (save-excursion
                   (cm-end-of-markup type)
                   (point))))
        (list beg end))))

(defun cm-markup-at-point (&optional backward)
  "Find the markup at point.
Return a list of the form (TYPE TEXT START-POS END-POS), or nil
if point is not at a markup.

Note that if point is in between two markups, this function
returns the one that follows point, unless BACKWARD is non-nil."
  (let* ((types (delq nil (mapcar #'(lambda (tp)
                                      (if (thing-at-point tp)
                                          tp))
                                  (mapcar #'car cm-delimiters))))
         (type (if (= (length types) 1)
                   (car types)
                 (save-excursion
                   (forward-char (if backward -1 1))
                   (if (thing-at-point (car types))
                       (car types)
                     (cadr types))))))
    (when type
      (append (list type) (list (thing-at-point type)) (cm-bounds-of-markup-at-point type)))))

(defun cm-point-inside-change-p (change)
  "Return t if point is inside CHANGE.
CHANGE is a change as returned by `cm-markup-at-point'.  Point is
within a change if it's inside the curly braces, not directly
outside of them.  The latter counts as being AT a change."
  (and change ; if there *is* no change, we're not inside one...
       (> (point) (cl-third change))
       (< (point) (cl-fourth change))))

(defun cm-extract-comment (change)
  "Extract the comment from CHANGE."
  (let ((bdelim (regexp-quote (cl-second (assq 'cm-comment cm-delimiters))))
        (edelim (regexp-quote (car (last (assq 'cm-comment cm-delimiters)))))
        (text (cl-second change)))
    (if (string-match (concat bdelim "\\(\\([[:ascii:]]\\|[[:nonascii:]]\\)*?\\)" edelim) text)
        (match-string 1 text))))

(defun cm-extract-author (change)
  "Extract the author tag of CHANGE.
The author tag should start with an `@' sign, should not contain
any spaces and should be at the start of the comment part of
CHANGE.  The return value is the author tag without `@', or nil if
CHANGE has no comment part or a comment without an author."
  (let ((comment (cm-extract-comment change)))
    (if (and comment
             (string-match "^@\\([^[:space:]]*\\)\\([[:ascii:]]\\|[[:nonascii:]]\\)*?$" comment))
        (match-string 1 comment))))

(defun cm-has-current-author-p (change)
  "Return t if the user is the author of CHANGE.
The user is considered the author of CHANGE if the author tag of
CHANGE matches `cm-author'; if CHANGE has no author; or if
`cm-author' is nil."
  (let ((author (cm-extract-author change)))
    (or (not cm-author)
        (not author)
        (string= author cm-author))))

(defun cm-merge-comment (change)
  "Merge CHANGE and an adjacent comment.
CHANGE is a list as returned by `cm-markup-at-point'.  Check if
there is a comment following CHANGE, or, if CHANGE is a comment
itself, check if there is a change preceding CHANGE.  If there
is, return an updated list that contains both.  If CHANGE is nil,
return nil."
  (unless (not change)
    (cond
     ((cm-comment-p change)
      (save-excursion
        (cm-beginning-of-comment)
        (backward-char 3)               ; hard-coded adjustment of point
        (let ((preceding (cm-markup-at-point)))
          (if preceding
              (list (car preceding) (concat (cl-second preceding) (cl-second change)) (cl-third preceding) (cl-fourth change))
            change))))
     (t (save-excursion
          (cm-end-of-markup (car change))
          (forward-char 3)              ; hard-coded adjustment of point
          (let ((comment (cm-markup-at-point)))
            (if (cm-comment-p comment)
                (list (car change) (concat (cl-second change) (cl-second comment)) (cl-third change) (cl-fourth comment))
              change)))))))

(defun cm-accept/reject-change-at-point (&optional interactive)
  "Accept or reject change at point interactively.
If the change is accepted or rejected, return point.  If the
change it is skipped, return or the position after the
change (point is not altered in that case).  If no change is
found at point, the return value is nil.

INTERACTIVE is used to determine whether the function was called
interactively or not."
  (interactive "p") ; we use "p" to signal that the function was called interactively
  (let ((change (cm-markup-at-point)))
    (when change
      (setq change (cm-merge-comment change)) ; include highlight & comment into one change
      (move-overlay cm-current-markup-overlay (cl-third change) (cl-fourth change))
      (let ((action (cond
                     ((memq (car change) '(cm-addition cm-deletion cm-substitution))
                      (read-char-choice (format "%s: (a)ccept/(r)eject/(s)kip%s? "
                                                (capitalize (substring (symbol-name (car change)) 3))
                                                (if interactive "" "/(q)uit"))
                                        '(?a ?r ?s ?q) t))
                     ((memq (car change) '(cm-comment cm-highlight))
                      (read-char-choice (format "%s: (d)elete/(s)kip%s? "
                                                (capitalize (substring (symbol-name (car change)) 3))
                                                (if interactive "" "/(q)uit"))
                                        '(?d ?s ?q) t)))))
        (delete-overlay cm-current-markup-overlay)
        (when (and (not interactive) (eq action ?q))  ; If the user aborted,
          (throw 'quit nil))                          ; get out.
        (cond
         ((memq action '(?a ?r ?d))
          (let ((inhibit-read-only t))
            (cm-without-following-changes
              (delete-region (cl-third change) (cl-fourth change))
              (insert (cm-substitution-string change action))))
          (point))
         ((eq action ?s)
          (cl-fourth change)))))))

(defun cm-substitution-string (change action)
  "Create the string to substitute CHANGE.
ACTION is a character, either `a' (accept), `r' (reject), or
`d' (delete).  `a' and `r' are valid for additions, deletions and
substitutions, `d' for comments and highlights."
  (when (eq action ?r)
    (setq action nil))  ; When rejecting a change, we don't need to do anything.
  (let ((type (cl-first change))
        (text (delete ?\n (cl-second change))))  ; Delete newlines because they mess up string-match below.
    (cond
     ((eq type 'cm-addition)
      (if (not action)
          ""
        (string-match cm-addition-regexp text)
        (match-string 1 text)))
     ((eq type 'cm-deletion)
      (if action
          ""
        (string-match cm-deletion-regexp text)
        (match-string 1 text)))
     ((eq type 'cm-substitution)
      (string-match cm-substitution-regexp text)
      (match-string (if action 2 1) text))
     ((and (eq type 'cm-comment)
           (eq action ?d))
      "")
     ((and (eq type 'cm-highlight)
           (eq action ?d))
      (string-match cm-highlight-regexp text)
      (match-string 1 text)))))

(defun cm-accept/reject-all-changes ()
  "Accept/reject all changes interactively."
  (interactive)
  (catch 'quit
    (goto-char (point-min))
    (while (cm-forward-change)
      (let ((pos (cm-accept/reject-change-at-point)))
        (when pos (goto-char pos)))))) ; move to the end of current change

;;; Navigation

(defun cm-forward-out-of-change ()
  "Move forward out of the change at point."
  (interactive)
  (let ((change (cm-merge-comment (cm-markup-at-point))))
    (if change
        (goto-char (cl-fourth change)))))

(defun cm-forward-change (&optional n)
  "Move forward to the N'th next change."
  (interactive "p")
  (or n (setq n 1))
  (funcall (if (> n 0)
               #'re-search-forward
             #'re-search-backward)
           (regexp-opt (mapcar #'cl-second cm-delimiters))
           nil t (abs n)))

(defun cm-backward-change (&optional n)
  "Move backward to the N'th preceding change."
  (interactive "p")
  (cm-forward-change (- n)))

(defun cm-set-author (str)
  "Set the author string to STR."
  (interactive "sSet author to: ")
  (setq cm-author (if (string= str "") nil str)))

(provide 'cm-mode)

;;; cm-mode.el ends here
