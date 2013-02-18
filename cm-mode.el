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
;; cm-mode is a minor mode that provides (rudimentary) support for
;; CriticMarkup in Emacs.
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
;; -   C-c * c: insert a comment
;; -   C-c * h: highlight text and insert a comment
;;
;; The commands to delete, substitute and highlight text all operate on the
;; region. The commands for inserting and substituting text and for
;; inserting a comment (which includes the command to highlight text) all
;; put the cursor at the correct position, so you can start typing right
;; away.
;;
;; Accepting or rejecting changes
;; ------------------------------
;;
;; You can interactively accept or reject a change by putting the cursor
;; inside it and hitting C-c * i. For additions, deletions and
;; substitutions, you get a choice between a to accept the change or r to
;; reject it. There are two other choices, s to skip this change or q to
;; quit. Both leave the change untouched and if you're just dealing with
;; the change at point, they are essentially identical. (They have
;; different functions when accepting or rejecting all changes
;; interactively, though.)
;;
;; For comments and highlights, the choices are different: d to delete the
;; comment or highlight (whereby the latter of course retains the
;; highlighted text, but the comment and the markup are removed), or k to
;; keep the comment or highlight. Again q quits and is essentially
;; identical to k. (Note that you can also use s instead of k, in case you
;; get used to skipping changes that way.)
;;
;; Not implemented yet is the ability to go through all changes in a buffer
;; and accept or reject them one by one, nor the ability to accept or
;; reject all changes all at once.
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
;; -   Command to accept or reject all changes interactively (C-c * I)
;; -   Commands to accept or reject all changes in one go
;; -   Follow changes mode: automatically insert CriticMarkup when changes
;; -   are made to the buffer.
;; -   Mouse support?

;;; Code:

(require 'thingatpt)

(defvar cm-delimiters '((cm-addition "{++" "++}")
                        (cm-deletion "{--" "--}")
                        (cm-substitution "{~~" "~~}")
                        (cm-comment "{>>" "<<}")
                        (cm-highlight "{{" "}}"))
  "CriticMarkup Delimiters.")

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

(defface ebib-crossref-face '((t (:foreground "red")))
  "*Face used to indicate values inherited from crossreferenced entries."
  :group 'ebib-faces)

(defvar cm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c*a" 'cm-addition)
    (define-key map "\C-c*d" 'cm-deletion)
    (define-key map "\C-c*s" 'cm-substitution)
    (define-key map "\C-c*c" 'cm-comment)
    (define-key map "\C-c*h" 'cm-highlight)
    (define-key map "\C-c*i" 'cm-accept/reject-change-at-point)
    (define-key map "\C-c*I" 'cm-accept/reject-all)
    map)
  "Keymap for cm-mode.")

;;;###autoload
(define-minor-mode cm-mode
  "Minor mode for CriticMarkup."
  :init-value nil :lighter " cm" :global nil
  (cond
   (cm-mode                             ; cm-mode is turned on
    (font-lock-add-keywords nil `((,cm-addition-regexp . 'cm-addition-face)
                                  (,cm-deletion-regexp . 'cm-deletion-face)
                                  (,cm-substitution-regexp . 'cm-substitution-face)
                                  (,cm-comment-regexp . 'cm-comment-face)
                                  (,cm-highlight-regexp . 'cm-highlight-face)) t)
    (setq cm-current-markup-overlay (make-overlay 1 1))
    (overlay-put cm-current-markup-overlay 'face 'highlight))
   ((not cm-mode)                       ; cm-mode is turned off
    (font-lock-remove-keywords nil `((,cm-addition-regexp . 'cm-addition-face)
                                     (,cm-deletion-regexp . 'cm-deletion-face)
                                     (,cm-substitution-regexp . 'cm-substitution-face)
                                     (,cm-comment-regexp . 'cm-comment-face)
                                     (,cm-highlight-regexp . 'cm-highlight-face)))
    (remove-overlays))))

;;;###autoload
(defun turn-on-cm ()
  "Unconditionally turn on cm-mode."
  (interactive)
  (cm-mode 1))

(defun turn-off-cm ()
  "Unconditionally turn off cm-mode"
  (interactive)
  (cm-mode -1))

(defun cm-addition ()
  "Make an addition."
  (interactive)
  (when (cm-markup-at-point)
    (error "Already inside a change"))
  (insert "{++++}")
  (backward-char 3))

(defun cm-deletion (beg end)
  "Mark text for deletion."
  (interactive "r")
  (when (cm-markup-at-point)
    (error "Already inside a change"))
  (let ((text (delete-and-extract-region beg end)))
    (insert (concat "{--" text "--}"))))

(defun cm-substitution (beg end)
  "Mark a substitution."
  (interactive "r")
  (when (cm-markup-at-point)
    (error "Already inside a change"))
  (let ((text (delete-and-extract-region beg end)))
    (insert (concat "{~~" text "~>~~}"))
    (backward-char 3)))

(defun cm-comment ()
  "Add a comment."
  (interactive)
  (when (cm-markup-at-point)
    (error "Already inside a change"))
  (insert "{>><<}")
  (backward-char 3))

(defun cm-highlight (beg end)
  "Highlight a stretch of text and add a comment."
  (interactive "r")
  (when (cm-markup-at-point)
    (error "Already inside a change"))
  (let ((text (delete-and-extract-region beg end)))
    (insert (concat "{{" text "}}{>><<}"))
    (backward-char 3)))

(defun cm-forward-markup (type &optional n)
  "Move forward N markups of TYPE.
If N is negative, move backward."
  (if (eq type 'cm-highlight) ; highlights have a delimiter of two characters
      (cm-forward-highlight n) ; therefore they have their own forward function
    (or n (setq n 1))
    ;; note that the delimiters are all three characters long. we must
    ;; therefore allow for the possibility that point is *within* a
    ;; delimiter. in the exx below, point is indicated with `|'.
    (cond
     ((> n 0) ; moving forward
      (let ((delim (third (assq type cm-delimiters))))
        ;; if point is inside the delimiter `+|+}':
        (when (looking-at (regexp-quote (substring delim -2)))
          (backward-char))
        (re-search-forward (regexp-quote delim) nil t n)))
     (t ; moving backward
      (let ((delim (second (assq type cm-delimiters))))
        ;; if point is inside the delimiter `{|++':
        (when (and (looking-back (regexp-quote (substring delim 0 1)) (1- (point)))
                   (looking-at (regexp-quote (substring delim 1))))
          (forward-char 2))
        ;; if point is inside the delimiter `{+|+':
        (when (looking-back (regexp-quote (substring delim 0 2)) (- (point) 2)) 
          (forward-char))
        (re-search-backward (regexp-quote delim) nil t (abs n)))))))

(defun cm-beginning-of-markup (type)
  "Move to the beginning of a markup of TYPE."
  (cm-forward-markup type -1))

(defun cm-end-of-markup (type)
  "Move to the end of a markup of TYPE."
  (cm-forward-markup type 1))

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
  (or n (setq n 1))
  (cond
   ((> n 0)
    (re-search-forward "}}" nil t n))
   (t
    (when (and (looking-back "{" (1- (point)))
               (looking-at "{"))
      (forward-char))
    (re-search-backward "{{" nil t (abs n)))))

(defun cm-beginning-of-highlight ()
  "Move to the beginning of an highlight."
  (cm-forward-highlight -1))

(defun cm-end-of-highlight ()
  "Move to the end of an highlight."
  (cm-forward-highlight 1))

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
if point is not inside a markup."
  (let ((type (catch 'found
                (dolist (type (mapcar #'car cm-delimiters))
                  (when (thing-at-point type)
                    (throw 'found type))))))
    (when type
      (append (list type) (list (thing-at-point type)) (cm-bounds-of-markup-at-point type)))))

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
      (backward-char 3) ; adjust point
      (let ((preceding (cm-markup-at-point)))
        (if preceding
            (list (car preceding) (concat (second preceding) (second change)) (third preceding) (fourth change))
          change))))
   (t
    (save-excursion
      (cm-end-of-markup (car change))
      (skip-chars-forward "[:space:]") ; allow for any whitespace between change and comment
      (forward-char 3) ; adjust point
      (let ((comment (cm-markup-at-point)))
        (if comment
            (list 'cm-highlight (concat (second change) (second comment)) (third change) (fourth comment))
          change))))))

(defun cm-accept/reject-change-at-point ()
  "Accept or reject change at point interactively."
  (interactive)
  (let ((change (cm-markup-at-point)))
    (when change
      (setq change (cm-expand-change change)) ; include highlight & comment into one change
      (move-overlay cm-current-markup-overlay (third change) (fourth change))
      (let ((action (cond
                     ((memq (car change) '(cm-addition cm-deletion cm-substitution))
                      (read-char-choice "(a)ccept/(r)eject/(s)kip/(q)uit? " '(?a ?r ?s ?q) t))
                     ((memq (car change) '(cm-comment cm-highlight))
                      (read-char-choice "(d)elete/(k)eep/(q)uit? " '(?d ?k ?s ?q) t)))))
        (delete-overlay cm-current-markup-overlay)
        (when (memq action '(?a ?r ?d))
          (delete-region (third change) (fourth change))
          (insert (cm-substitution-string change action)))))))

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
      (if action (substring text 3 -3)
        ""))
     ((eq type 'cm-deletion)
      (if action "" (substring text 3 -3)))
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

(provide 'cm-mode)

;;; cm-mode ends here
