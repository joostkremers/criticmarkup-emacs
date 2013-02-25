;;; cm-mode.el --- Minor mode for CriticMarkup

;; Copyright (c) 2013 Joost Kremers

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 14 Feb 2013
;; Version: 0.1
;; Keywords: text

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
;; cm-mode is a minor mode that provides support for CriticMarkup in Emacs.
;; 
;; CriticMarkup defines the following patterns for marking changes to a
;; text:
;; 
;; -   Addition {++ ++}
;; -   Deletion {-- --}
;; -   Substitution {~~ ~> ~~}
;; -   Comment {>> <<}
;; -   Highlight {{ }}{>> <<}
;; 
;; Activating cm-mode provides key bindings to insert the markup above and
;; thus mark one's changes to the text. The provided key bindings are:
;; 
;; -   C-c * a: add text
;; -   C-c * d: delete text
;; -   C-c * s: substitute text
;; -   C-c * c: insert a comment (possibly with highlight)
;; 
;; The commands to delete or substitute text operate on the region. The
;; command to insert a comment can be used with an active region, in which
;; case the text in the region will be highlighted. It can also be used
;; inside an existing markup to add a comment to it. If it is used anywhere
;; else, it just adds a lone comment. The commands for inserting and
;; substituting text and for inserting a comment all put the cursor at the
;; correct position, so you can start typing right away.
;; 
;; Follow changes mode
;; -------------------
;; 
;; cm-mode also provides a (rudimentary) 'follow changes' mode. When
;; activated, changes you make to the buffer are automatically marked as
;; insertions or deletions. Substitutions cannot be made automatically
;; (that is, if you mark a word, delete it and then type a replacement, it
;; will still be marked as sequence of deletion+insertion, not as a
;; substitution), but they can still be made manually with C-c * s. You can
;; activate and deactivate follow changes mode with C-c * F. When it's
;; active, the modeline indicator for cm-mode changes from cm to cm*.
;; 
;; Note that this functionality is in development and not very polished
;; yet. Multiple deletions in sequence, for example, are not combined, so
;; that deleting a word with <backspace> leaves a string of deletion
;; markups. Deleting a character with <del> also leaves the cursor in the
;; wrong position. Follow changes mode should also be considered
;; alpha-grade, i.e., it works to the extent that it works. (If you
;; experience problems with it, please open up an issue on Github or send
;; me an email.)
;; 
;; Accepting or rejecting changes
;; ------------------------------
;; 
;; One can interactively accept or reject a change by putting the cursor
;; inside it and hitting C-c * i. For additions, deletions and
;; substitutions, you get a choice between a to accept the change or r to
;; reject it. There are two other choices, s to skip this change or q to
;; quit. Both leave the change untouched and if you're just dealing with
;; the change at point, they are essentially identical.
;; 
;; For comments and highlights, the choices are different: d to delete the
;; comment or highlight (whereby the latter of course retains the
;; highlighted text, but the comment and the markup are removed), or k to
;; keep the comment or highlight. Again q quits and is essentially
;; identical to k. (Note that you can also use s instead of k, in case you
;; get used to skipping changes that way.)
;; 
;; You can interactively accept or reject all changes with C-c * I (that is
;; a capital i). This will go through each change asking you whether you
;; want to accept, reject or skip it, or delete or keep it. Typing q quits
;; the accept/reject session.
;; 
;; Font lock
;; ---------
;; 
;; cm-mode also adds the markup patterns defined by CriticMarkup to
;; font-lock-keywords and provides customisable faces to highlight them.
;; The customisation group is called criticmarkup.
;; 
;; You may notice that changes that span multiple lines are not
;; highlighted. The reason for this is that multiline font lock in Emacs is
;; not straightforward. There are ways to deal with this, but since cm-mode
;; is a minor mode, it could interfere with the major mode's font locking
;; mechanism if it did that.
;; 
;; To mitigate this problem, you can use soft wrap (with visual-line-mode).
;; Since each paragraph is then essentially a single line, font lock works
;; even across multiple (visual) lines.
;; 
;; TODO
;; ----
;; 
;; -   Commands to accept or reject all changes in one go.
;; -   Mouse support?

;;; Code:

(require 'thingatpt)

(defvar cm-delimiters '((cm-addition "{++" "++}")
                        (cm-deletion "{--" "--}")
                        (cm-substitution "{~~" "~~}")
                        (cm-comment "{>>" "<<}")
                        (cm-highlight "{{" "}}"))
  "CriticMarkup Delimiters.")

(defvar cm-follow-changes nil
  "Flag indicating whether follow changes mode is active.")

(defvar cm-current-deletion nil
  "The deleted text in follow changes mode.
The value is actually a list consisting of the text and a flag
indicating whether the deletion was done with the backspace
key.")

(defvar cm-change-no-record nil
  "Flag indicating whether to actually record a change.
In follow changes mode, some operations that change the buffer
must not be recorded with markup. Such functions can set this
flag to indicate this. (Though they should actually use the macro
`cm-without-following-changes'.)")

(defvar cm-addition-regexp "\\(?:{\\+\\+.*?\\+\\+}\\)"
  "CriticMarkup addition regexp.")

(defvar cm-deletion-regexp "\\(?:{--.*?--}\\)"
  "CriticMarkup deletion regexp.")

(defvar cm-substitution-regexp "\\(?:{~~.*?~>.*?~~}\\)"
  "CriticMarkup substitution regexp.")

(defvar cm-comment-regexp "\\(?:{>>.*?<<}\\)"
  "CriticMarkup comment regexp.")

(defvar cm-highlight-regexp "\\(?:{{.*?}}\\)"
  "CriticMarkup highlight regexp.")

(defvar cm-current-markup-overlay nil
  "Overlay marking the current highlight.")
(make-variable-buffer-local 'cm-current-markup-overlay)

(defgroup criticmarkup nil "Minor mode for CriticMarkup." :group 'wp)

(defface cm-addition-face '((t (:foreground "green")))
  "*Face for CriticMarkup additions."
  :group 'criticmarkup)

(defface cm-deletion-face '((t (:foreground "red")))
  "*Face for CriticMarkup deletions."
  :group 'criticmarkup)

(defface cm-substitution-face '((t (:foreground "orange")))
  "*Face for CriticMarkup substitutions."
  :group 'criticmarkup)

(defface cm-comment-face '((t (:foreground "blue")))
  "*Face for CriticMarkup comments."
  :group 'criticmarkup)

(defface cm-highlight-face '((t (:foreground "magenta")))
  "*Face for CriticMarkup highlights."
  :group 'criticmarkup)

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

(defvar cm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c*a" 'cm-addition)
    (define-key map "\C-c*d" 'cm-deletion)
    (define-key map "\C-c*s" 'cm-substitution)
    (define-key map "\C-c*c" 'cm-comment)
    (define-key map "\C-c*i" 'cm-accept/reject-change-at-point)
    (define-key map "\C-c*I" 'cm-accept/reject-all-changes)
    (define-key map "\C-c*F" 'cm-follow-changes)
    map)
  "Keymap for cm-mode.")

;;;###autoload
(define-minor-mode cm-mode
  "Minor mode for CriticMarkup."
  :init-value nil :lighter (:eval (concat " cm" cm-follow-changes)) :global nil
  (cond
   (cm-mode                             ; cm-mode is turned on
    (font-lock-add-keywords nil `((,cm-addition-regexp . cm-addition-face)
                                  (,cm-deletion-regexp . cm-deletion-face)
                                  (,cm-substitution-regexp . cm-substitution-face)
                                  (,cm-comment-regexp . cm-comment-face)
                                  (,cm-highlight-regexp . cm-highlight-face)) t)
    (setq cm-current-markup-overlay (make-overlay 1 1))
    (overlay-put cm-current-markup-overlay 'face 'highlight))
   ((not cm-mode)                       ; cm-mode is turned off
    (font-lock-remove-keywords nil `((,cm-addition-regexp . cm-addition-face)
                                     (,cm-deletion-regexp . cm-deletion-face)
                                     (,cm-substitution-regexp . cm-substitution-face)
                                     (,cm-comment-regexp . cm-comment-face)
                                     (,cm-highlight-regexp . cm-highlight-face)))
    (remove-overlays))))

(defun cm-follow-changes ()
  "Record changes."
  (interactive)
  (if cm-follow-changes
      (progn
        (setq before-change-functions (delq 'cm-before-change before-change-functions))
        (setq after-change-functions (delq 'cm-after-change after-change-functions))
        (ad-deactivate 'undo)
        (setq cm-follow-changes nil)
        (message "Follow changes mode deactivated."))
    (add-to-list 'before-change-functions 'cm-before-change t)
    (add-to-list 'after-change-functions 'cm-after-change)
    (ad-activate 'undo t)
    (setq cm-follow-changes "*")
    (message "Follow changes mode activated.")))

(defun cm-before-change (beg end)
  "Function to execute before a buffer change."
  (unless (or cm-change-no-record       ; do not record this change
              (and (= beg (point-min)) (= end (point-max)))) ; this happens on buffer switches
    ;; (message "Point: %s; beg: %s; end: %s" (point) beg end)
    (if (= beg end)                   ; addition
        (cm-make-addition (cm-markup-at-point))
      ;; when the deletion was done with backspace, point is at end.
      (setq cm-current-deletion (list (buffer-substring beg end) (= (point) end))))))

(defun cm-after-change (beg end length)
  "Function to execute after a buffer change.
This function marks deletions. See cm-before-change for
details."
  (unless (or cm-change-no-record
              (not cm-current-deletion))
    (apply 'cm-make-deletion cm-current-deletion)
    (setq cm-current-deletion nil)))

(defmacro cm-without-following-changes (&rest body)
  "Execute BODY without following changes."
  (declare (indent defun))
  `(let ((cm-change-no-record t))
     ,@body))

(defadvice undo (around cm-no-follow (&optional arg))
  "Temporarily remove cm-record-change from before-change-functions."
  (cm-without-following-changes
    ad-do-it))

;;;###autoload
(defun turn-on-cm ()
  "Unconditionally turn on cm-mode."
  (interactive)
  (cm-mode 1))

(defun turn-off-cm ()
  "Unconditionally turn off cm-mode"
  (interactive)
  (cm-mode -1))

;; Making an addition is fairly simple: we just need to add markup if point
;; isn't already at an addition markup, and then position point
;; appropriately. The user can then type new text. A deletion is more
;; difficult, because it also needs to (re)insert the deleted text and do
;; something sensible with point. This is especially difficult in follow
;; changes mode, because the deletion may be made with DEL or BACKSPACE.

(defun cm-addition ()
  "Make an addition at point.
If point is at an addition markup already, the new addition is
combined with it. If point is inside any other markup, no
addition can be made."
  (interactive)
  (let ((change (cm-markup-at-point)))
    (if (or (not (cm-point-inside-change-p change))
            (eq (car change) 'cm-addition))
        (cm-without-following-changes
          (cm-make-addition change))
      (error "Cannot make an addition here"))))

(defun cm-deletion (beg end)
  "Mark text for deletion."
  (interactive "r")
  (let ((change (cm-markup-at-point)))
    (when (cm-point-inside-change-p change)
      (error "Cannot make a deletion here")) ; TODO we should check whether the region contains markup.
    (when (use-region-p)
      (cm-without-following-changes
        (cm-make-deletion (delete-and-extract-region beg end))))))

(defun cm-make-addition (change)
  "Position point for an addition and insert addition markup if necessary.
CHANGE is the change markup at point, if any, as returned by
cm-markup-at-point. If this is an addition, the new addition is
combined with it, even if point is right outside it. (That avoids
having two additions adjacent to each other.) If it is another
kind of markup, and point is inside the curly braces, we make
sure point is not in the delimiter before adding text."
  (if (or (eq (car change) 'cm-addition)
          (cm-point-inside-change-p change))
      (cm-move-into-markup (car change))
    (insert "{++++}")
    (backward-char 3)))

(defun cm-make-deletion (text &optional backspace)
  "Reinsert TEXT into the buffer and add deletion markup if necessary.
TEXT is the text that's being deleted, CHANGE the change at
point, if any.

If BACKSPACE is T, the deletion was done with the backspace key;
point will then be left before the deletion markup."
  ;; TODO: we should check whether the text to be deleted contains part of
  ;; a change.
  (let ((change (cm-markup-at-point)))
    (unless (cm-point-inside-change-p change)
      (save-excursion
        (if (not (or change
                     (eq (car change) 'cm-deletion)))
            (insert (concat "{--" text "--}"))
          (cm-move-into-markup 'cm-deletion)
          (insert text)))
      ;; the save-excursion leaves point at the start of the deletion markup
      (unless backspace
        (cm-end-of-markup 'cm-deletion)))))

(defun cm-substitution (beg end)
  "Mark a substitution."
  (interactive "r")
  (when (cm-point-inside-change-p (cm-markup-at-point))
    (error "Cannot make a substitution here")) ; TODO we should check whether the region contains markup.
  (cm-without-following-changes
    (let ((text (delete-and-extract-region beg end)))
      (insert (concat "{~~"  text "~>~~}"))
      (backward-char 3))))

(defun cm-comment (beg end)
  "Add a comment.
If the region is active, the text in the region is highlighted.
If point is in an existing change, the comment is added after it."
  (interactive "r")
  (cm-without-following-changes
    (let ((change (cm-markup-at-point))
          text)
      (cond
       (change
        (deactivate-mark)               ; we don't want the region active
        (cm-end-of-markup (car change)))
       ;; note: we do not account for the possibility that the region
       ;; contains a change but point is outside of it...
       ((use-region-p)
        (setq text (delete-and-extract-region beg end))))
      (insert (if text (concat "{{" text "}}") "") "{>><<}")
      (backward-char 3))))

(defun cm-point-at-delim (delim &optional end strict)
  "Return non-NIL if point is at a delimiter.
If DELIM is an end delimiter, optional argument END must be T. 

Point counts as being at delim if it is in a delimiter or
directly outside, but not when it is directly inside. So `|{++',
`{|++', `{+|+', return 0, 1, and 2 respectively, while `{++|'
returns NIL. Similarly, `++}|', `++|}', `+|+}' return 0, 1, and
2, while `|++}' returns NIL.

If STRICT is non-NIL, point must be inside the delimiter. That
is, instead of 0, the return value will be NIL."
  (save-excursion
    (if end
        (let ((distance (skip-chars-forward (substring delim 1) (+ (point) 2))))
          (if (looking-back (regexp-quote delim))
              (if (> distance 0)
                  distance
                (and (not strict) 0))))
      (let ((distance (skip-chars-backward (substring delim 0 -1) (- (point) 2))))
        (if (looking-at (regexp-quote delim))
            (if (< distance 0)
                (abs distance)
              (and (not strict) 0)))))))

(defun cm-forward-markup (type &optional n)
  "Move forward N markups of TYPE.
If N is negative, move backward. If point is inside a delimiter,
this function moves point to the previous/next markup. If it's
inside a markup, it moves it to the edge. If point is at the edge
of a markup, it moves to the end of the next markup of the same
type."
  (or n (setq n 1))
  (cond
   ((> n 0)                             ; moving forward
    (let ((delim (third (assq type cm-delimiters))))
      (backward-char (- 3 (or (cm-point-at-delim delim t t) 3))) ; adjust point if it's inside a delim
      (re-search-forward (regexp-quote delim) nil t n)))
   (t                                   ; moving backward
    (let ((delim (second (assq type cm-delimiters))))
      (forward-char (- 3 (or (cm-point-at-delim delim nil t) 3))) ; adjust point if it's inside a delim
      (re-search-backward (regexp-quote delim) nil t (abs n))))))

(defun cm-beginning-of-markup (type)
  "Move to the beginning of a markup of TYPE."
  ;; first move out of the delimiter, if we're in one.
  (cm-move-past-delim (second (assq type cm-delimiters)))
  (cm-forward-markup type -1))

(defun cm-end-of-markup (type)
  "Move to the end of a markup of TYPE."
  ;; first move out of the delimiter, if we're in one.
  (cm-move-past-delim (third (assq type cm-delimiters)) t)
  (cm-forward-markup type))

(defun cm-move-past-delim (delim &optional end)
  "Move point past DELIM into the markup.
If DELIM is an end delimiter, END must be T. If point is not at a
delimiter, do not move."
  (if end
      (backward-char (- 3 (or (cm-point-at-delim delim end)
                              3)))
    (forward-char (- 3 (or (cm-point-at-delim delim)
                           3)))))

(defun cm-move-into-markup (type)
  "Make sure point is inside the delimiters of TYPE."
  ;; we simply call cm-move-past-delim twice, since it's harmless if we're
  ;; not on the right delimiter.
  (cm-move-past-delim (second (assq type cm-delimiters)))
  (cm-move-past-delim (third (assq type cm-delimiters)) t))

(defun cm-forward-addition (&optional n)
  "Move forward N addition markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-addition n))

(defun cm-beginning-of-addition ()
  "Move to the beginning of an addition."
  (cm-forward-markup 'cm-addition -1))

(defun cm-end-of-addition ()
  "Move to the end of an addition."
  (cm-forward-markup 'cm-addition 1))

(put 'cm-addition 'forward-op 'cm-forward-addition)
(put 'cm-addition 'beginning-op 'cm-beginning-of-addition)
(put 'cm-addition 'end-op 'cm-end-of-addition)

(defun cm-forward-deletion (&optional n)
  "Move forward N deletion markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-deletion n))

(defun cm-beginning-of-deletion ()
  "Move to the beginning of an deletion."
  (cm-forward-markup 'cm-deletion -1))

(defun cm-end-of-deletion ()
  "Move to the end of an deletion."
  (cm-forward-markup 'cm-deletion 1))

(put 'cm-deletion 'forward-op 'cm-forward-deletion)
(put 'cm-deletion 'beginning-op 'cm-beginning-of-deletion)
(put 'cm-deletion 'end-op 'cm-end-of-deletion)

(defun cm-forward-substitution (&optional n)
  "Move forward N substitution markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-substitution n))

(defun cm-beginning-of-substitution ()
  "Move to the beginning of an substitution."
  (cm-forward-markup 'cm-substitution -1))

(defun cm-end-of-substitution ()
  "Move to the end of an substitution."
  (cm-forward-markup 'cm-substitution 1))

(put 'cm-substitution 'forward-op 'cm-forward-substitution)
(put 'cm-substitution 'beginning-op 'cm-beginning-of-substitution)
(put 'cm-substitution 'end-op 'cm-end-of-substitution)

(defun cm-forward-comment (&optional n)
  "Move forward N comment markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-comment n))

(defun cm-beginning-of-comment ()
  "Move to the beginning of an comment."
  (cm-forward-markup 'cm-comment -1))

(defun cm-end-of-comment ()
  "Move to the end of an comment."
  (cm-forward-markup 'cm-comment 1))

(put 'cm-comment 'forward-op 'cm-forward-comment)
(put 'cm-comment 'beginning-op 'cm-beginning-of-comment)
(put 'cm-comment 'end-op 'cm-end-of-comment)

(defun cm-forward-highlight (&optional n)
  "Move forward N highlight markups.
If N is negative, move backward."
  (cm-forward-markup 'cm-highlight n))

  ;; (or n (setq n 1))
  ;; (cond
  ;;  ((> n 0)
  ;;   (re-search-forward "}}" nil t n))
  ;;  (t
  ;;   (when (and (looking-back "{" (1- (point)))
  ;;              (looking-at "{"))
  ;;     (forward-char))
  ;;   (re-search-backward "{{" nil t (abs n)))))

(defun cm-beginning-of-highlight ()
  "Move to the beginning of an highlight."
  (cm-forward-markup 'cm-highlight -1))

(defun cm-end-of-highlight ()
  "Move to the end of an highlight."
  (cm-forward-markup 'cm-highlight 1))

(put 'cm-highlight 'forward-op 'cm-forward-highlight)
(put 'cm-highlight 'beginning-op 'cm-beginning-of-highlight)
(put 'cm-highlight 'end-op 'cm-end-of-highlight)

(defun cm-bounds-of-markup-at-point (type)
  "Return the bounds of markup TYPE at point.
The return value is a list of the form (START-POS END-POS). If
point is not within a markup of TYPE, return NIL.

TYPE is one of `cm-addition', `cm-deletion', `cm-substitution',
`cm-comment', or `cm-highlight'. Note that in the case of
comments, only the comment is returned, any preceding highlight
is ignored. The same holds for highlights: the following comment
is not included."
  (if (thing-at-point type)
      (let ((beg (save-excursion
                   (cm-beginning-of-markup type)
                   (point)))
            (end (save-excursion
                   (cm-end-of-markup type)
                   (point))))
        (list beg end))))

(defun cm-markup-at-point ()
  "Find the markup at point.
Return a list of the form (TYPE TEXT START-POS END-POS), or NIL
if point is not at a markup."
  ;; if point is in between two markups, the one that is first in
  ;; cm-delimiters will be returned, regardless whether it's before or
  ;; after point. this is not very pretty, but it does no harm, so no need
  ;; to change it.
  (let ((type (catch 'found
                (dolist (type (mapcar #'car cm-delimiters))
                  (when (thing-at-point type)
                    (throw 'found type))))))
    (when type
      (append (list type) (list (thing-at-point type)) (cm-bounds-of-markup-at-point type)))))

(defun cm-point-inside-change-p (change &optional correction)
  "Return T if point is inside CHANGE.
CHANGE is a change as returned by `cm-markup-at-point'. Point is
within a change if it's inside the curly braces, not directly
outside of them. The latter counts as being AT a change.

If non-NIL, CORRECTION is added to the value of point; this is
useful if `cm-point-inside-change-p' is used after a deletion but
with a change that follows that deletion but was extracted before
it."
  (and change ; if there *is* no change, we're not inside one...
       (not (or (= (+ (or correction 0) (point)) (third change))
                (= (+ (or correction 0) (point)) (fourth change))))))

(defun cm-expand-change (change)
  "Expand CHANGE with a following comment or, if a comment, with a preceding change.
If CHANGE is a comment, check if there's another change preceding
it; if so, include it and change the type accordingly. If CHANGE
is of any other type, check if there's a commend and include it."
  (cond
   ((eq (car change) 'cm-comment)
    (save-excursion
      (cm-beginning-of-comment)
      (skip-chars-backward "[:space:]") ; allow for any whitespace between change and comment
      (backward-char 3)                 ; adjust point
      (let ((preceding (cm-markup-at-point)))
        (if preceding
            (list (car preceding) (concat (second preceding) (second change)) (third preceding) (fourth change))
          change))))
   (t (save-excursion
        (cm-end-of-markup (car change))
        (skip-chars-forward "[:space:]") ; allow for any whitespace between change and comment
        (forward-char 3)                 ; adjust point
        (let ((comment (cm-markup-at-point)))
          (if (eq (car comment) 'cm-comment)
              (list (car change) (concat (second change) (second comment)) (third change) (fourth comment))
            change))))))

(defun cm-accept/reject-change-at-point (&optional interactive)
  "Accept or reject change at point interactively.

Return point if the change is accepted or rejected or the
position after the change if it is skipped (point is not changed
in that case). If no change is found at point, the return value
is NIL."
  (interactive "p") ; we use "p" to signal that the function was called interactively
  (let ((change (cm-markup-at-point)))
    (when change
      (setq change (cm-expand-change change)) ; include highlight & comment into one change
      (move-overlay cm-current-markup-overlay (third change) (fourth change))
      (let ((action (cond
                     ((memq (car change) '(cm-addition cm-deletion cm-substitution))
                      (read-char-choice (format "%s: (a)ccept/(r)eject/(s)kip/(q)uit? "
                                                (capitalize (substring (symbol-name (car change)) 3)))
                                        '(?a ?r ?s ?q) t))
                     ((memq (car change) '(cm-comment cm-highlight))
                      (read-char-choice (format "%s: (d)elete/(k)eep/(q)uit? "
                                                (capitalize (substring (symbol-name (car change)) 3)))
                                        '(?d ?k ?s ?q) t)))))
        (delete-overlay cm-current-markup-overlay)
        (when (and (not interactive) (eq action ?q)) ; if the user aborted 
          (throw 'quit nil))                         ; get out
        (cond
         ((memq action '(?a ?r ?d))
          (delete-region (third change) (fourth change))
          (insert (cm-substitution-string change action))
          (point))
         ((memq action '(?s ?k))
          (fourth change)))))))

(defun cm-substitution-string (change action)
  "Create the string to substitute CHANGE.
ACTION is a character, either `a' (accept), `r' (reject), or
`d' (delete). `a' and `r' are valid for additions, deletions and
substitutions, `d' for comments and highlights."
  (when (eq action ?r)
    (setq action nil)) ; so we can use a simple `if' rather than a `cond'
  (let ((type (first change))
        (text (delete ?\n (second change)))) ; delete newlines because they mess up string-match below.
    (cond
     ((eq type 'cm-addition)
      (if (not action)
          ""
        (string-match "{\\+\\+\\(.*?\\)\\+\\+}" text)
        (match-string 1 text)))
     ((eq type 'cm-deletion)
      (if action
          ""
        (string-match "{--\\(.*?\\)--}" text)
        (match-string 1 text)))
     ((eq type 'cm-substitution)
      (string-match "{~~\\(.*?\\)~>\\(.*?\\)~~}" text)
      (match-string (if action 2 1) text))
     ((and (eq type 'cm-comment)
           (eq action ?d))
      "")
     ((and (eq type 'cm-highlight)
           (eq action ?d))
      (string-match "{{\\(.*?\\)}}" text)
      (match-string 1 text)))))

(defun cm-accept/reject-all-changes ()
  "Accept/reject all changes interactively."
  (interactive)
  (catch 'quit
    (let ((delims-regexp (regexp-opt (mapcar #'second cm-delimiters))))
      (goto-char (point-min))
      (while (re-search-forward delims-regexp nil t)
        (let ((pos (cm-accept/reject-change-at-point)))
          (when pos (goto-char pos))))))) ; move to the end of current change

(provide 'cm-mode)

;;; cm-mode ends here
