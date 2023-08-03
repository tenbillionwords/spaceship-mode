;; elastindent-mode  --- fix indentation with variable-pitch fonts. -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Scott Messick (tenbillionwords)
;; Copyright (C) 2023 Jean-Philippe Bernardy

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; General terminological note: elastindent-mode is concerned with
;;; adjusting the width of only spaces and tabs which occur before a
;;; printing character (not space or tab) on a line.  We use the word
;;; “indentation” to refer to these tabs or spaces.  It is ambiguous
;;; whether Unicode space characters other than space and (horizontal)
;;; tab should be considered part of the leading space or not, but in
;;; the code we assume it is only spaces and tabs.  Thus
;;; elastindent-mode treats other space characters as printing
;;; characters.
;;; The support for tabs is currently limited.  Tabs can only be first
;;; in the indentation (they cannot follows spaces).  Editting code
;;; with tabs isn't fully supported either.

;;; Code:

(require 'cl-lib)
(require 'dash)

(defun elastindent-mode-maybe ()
  "Function to put in hooks, for example `prog-mode-hook'."
  ;; See org-src-font-lock-fontify-block for buffer name.  Elastindent
  ;; isn't needed in fontification buffers. Fontification is called on
  ;; every keystroke (‽). Calling elastindent-do-buffer on each
  ;; keystroke on the whole block is very slow.
  (unless (string-prefix-p " *org-src-fontification:" (buffer-name))
    (elastindent-mode)))

(defface elastindent-lvl-0 '((t (:inherit region))) "Face for indentation level column 0.")
(defface elastindent-lvl-1 '((t (:inherit region))) "Face for indentation level column 1.")
(defface elastindent-lvl-2 '((t (:inherit region))) "Face for indentation level column 2.")
(defface elastindent-lvl-3 '((t (:inherit region))) "Face for indentation level column 3.")
(defface elastindent-lvl-4 '((t (:inherit region))) "Face for indentation level column 4.")
(defface elastindent-lvl-5 '((t (:inherit region))) "Face for indentation level column 5.")
(defface elastindent-lvl-6 '((t (:inherit region))) "Face for indentation level column 6.")
(defface elastindent-lvl-7 '((t (:inherit region))) "Face for indentation level column 7.")
(defface elastindent-lvl-8 '((t (:inherit region))) "Face for indentation level column 8.")
(defface elastindent-lvl-9 '((t (:inherit region))) "Face for indentation level column 9.")

(define-minor-mode elastindent-mode
  "Improves indentation with in variable-pitch face.
Adjust the width of indentation characters to align the indented
code to the correct position.  The correct position is defined as
the same relative position to the previous line as it were if a
fixed-pitch face was used.

More precisely, any space character on a line with no printing
characters before it will be matched up with a corresponding
character on the previous line, if there is one.  That character
may itself be a width-adjusted space, meaning the width
ultimately comes from some other kind of character higher up.

Due to technical limitations, this mode does not try to detect
situations where the font has changed but the text hasn't, which
will mess up the alignment.  You can run
‘elastindent-do-buffer-if-enabled’ to fix it."
  :init-value nil :lighter nil :global nil
  (if elastindent-mode
      (progn
        ;; (message "activating elastindent in %s" (buffer-name))
        (elastindent-do-buffer)
        ;; add change function to beginning of list, to ensure it comes before that of
        ;; tabble
        (add-hook 'before-change-functions 'elastindent-before-change-function nil t)
        (add-hook 'after-change-functions 'elastindent-after-change-function nil t)
        (add-hook 'post-command-hook 'elastindent-handle-queue) ;; FIXME: tabble
        (add-hook 'text-scale-mode-hook 'elastindent-do-buffer nil t))
    (progn
      (remove-hook 'before-change-functions 'elastindent-before-change-function t)
      (remove-hook 'after-change-functions 'elastindent-after-change-function t)
      (remove-hook 'text-scale-mode-hook 'elastindent-do-buffer t)
      (remove-hook 'post-command-hook 'elastindent-handle-queue) ;; FIXME: tabble
      (elastindent-clear-buffer))))

(defcustom elastindent-reference-col-width 10
  "Default width of a column (space character) in pixels.
When setting the width of a space character after the end of the
previous line, there is no column to align to.  In this case, the
width of the column will be set to.
`elastindent-reference-col-width'."
  :type 'int :group 'elastindent)

(defun elastindent-on-col-2 (pos)
  "Is POS on 2nd column?"
  (save-excursion
    (goto-char pos)
    (and (not (bolp))
         (progn (forward-char -1)
                (bolp)))))

(defun elastindent-char-pixel-width (pos)
  "Return the pixel width of char at POS."
  (if-let (p (get-text-property pos 'elastindent-width))
      p
    ;; Emacs bug: sometimes the returned window-text-pixel-size is
    ;; wrong. In this case computing it indirectly like below
    ;; seems to fix the issue.
    (let ((c (car (window-text-pixel-size nil pos (1+ pos)))))
      (if (or (<= c 1) ; suspicious
              (elastindent-on-col-2 pos)) ; emacs is often wrong on that column, for some reason.
          (- (car (window-text-pixel-size nil (1- pos) (1+ pos)))
             (car (window-text-pixel-size nil (1- pos) pos)))
        c))))

(defun elastindent-char-lvl (pos l-pos)
  "Return the indentation level at POS.
An indentation level is not the colum, but rather defined as the
number of times an indentation occured.  Level is negative for the
spaces which aren't in the first column of any given level.  If
this cannot be determined locally by what happens at POS, then
look at L-POS, which is a position just to the left of the
position for which we want the level."
  (or (get-text-property pos 'elastindent-lvl)
      (if (or (eq pos (point-min)) (eq (char-after (1- pos)) ?\n))
          1 ;; first char in the line. creates an indentation level by default.
        (let ((lvl-before (get-text-property (1- pos) 'elastindent-lvl)))
          (if (and lvl-before (<= lvl-before 0))
              ;; it's a space before this position. Thus this character creates a new indentation level.
              (1+ (abs lvl-before))
            (- (abs (get-text-property l-pos 'elastindent-lvl))))))))

(defun elastindent-show-char-pixel-width (pos)
  "Display pixel width of region char at POS .
This is a debug utility for `elastindent-mode'"
  (interactive "d")
  (message "pixel width of region: %s, raw=%s, disp=%s, prop=%s"
           (elastindent-char-pixel-width pos)
           (car (window-text-pixel-size nil pos (1+ pos)))
           (get-char-property pos 'elastindent-width)
           (get-char-property pos 'display)))

(defmacro elastindent-with-suitable-window (&rest body)
  "Execute BODY in a context where current buffer as a window."
  (declare (indent 0))
  (let ((temp-frame-symb (make-symbol "temp-frame"))
        (window-symb (make-symbol "window")))
    `(let* ((,window-symb (get-buffer-window nil t))
            (,temp-frame-symb
             (if ,window-symb
                 nil
               ;; the new frame will display the current buffer by default
               (make-frame '((visibility . nil))))))
       (unwind-protect
           ;; using ‘with-selected-window’ is probably inefficient for
           ;; our purposes, could rewrite to say explicitly exactly
           ;; what really needs restoration
           (with-selected-window
               (or ,window-symb
                   (frame-selected-window ,temp-frame-symb))
             (progn ,@body))
         (when ,temp-frame-symb
           (delete-frame ,temp-frame-symb))))))

(defun elastindent-clear-region-properties (start end cue-prop props-to-remove)
  "Clear PROPS-TO-REMOVE text properties in given region.
The region is the part between START and END which also has the
text property CUE-PROP be t."
  (with-silent-modifications
    (cl-do ((pos1 start) pos2) (nil)
      (setq pos1 (text-property-any pos1 end cue-prop t))
      (unless pos1 (cl-return))
      (setq pos2 (or (next-single-property-change pos1 cue-prop nil end)
                     end))
      (remove-list-of-text-properties pos1 pos2 props-to-remove)
      (setq pos1 pos2))))

(defun elastindent-set-char-pixel-width (pos w)
  "Set the width of character at POS to be W.
This only works if the character in question is a space or a tab.
Also add text properties to remember that we did this change and
by what."
  (if w (add-text-properties pos (1+ pos)
                             (list 'display (list 'space :width (list w))
                                   'elastindent-adjusted t
                                   'elastindent-width w))
    (remove-text-properties pos (1+ pos) '(display elastindent-width elastindent-adjusted))))

(defun elastindent-avg-info (lst)
  "Return the combination of infos in LST."
  (cons (-sum (-map 'car lst)) (-some 'cdr lst)))

(defun elastindent-set-char-info (pos i)
  "Set width and face I for space at POS.
The car of I is the width, and the cdr of I is the level."
  (when i
    (elastindent-set-char-pixel-width pos (car i))
    (let* ((lvl (or (cdr i))))
      (put-text-property pos (1+ pos) 'elastindent-lvl lvl)
      (if (and lvl (> lvl 0))
          (put-text-property pos (1+ pos) 'font-lock-face
                             (intern (concat "elastindent-lvl-" (int-to-string (mod lvl 10)))))
        (remove-list-of-text-properties pos (1+ pos) '(font-lock-face))))))

(defun elastindent-column-leaves-indent (target)
  "Return t if by advancing TARGET columns one reaches the end of the indentation."
  (let ((col 0))
    (while (and (not (eobp)) (< col target))
      (pcase (char-after)
        (?\s (setq col (1+ col)))
        (?\t (setq col (+ 8 col)))
        (_ (setq target -1)))
      (when (<= 0 target) (forward-char)))
    (not (looking-at (rx (any "\s\t"))))))

(defun elastindent-in-indent () ;; TODO: also return current column and avoid call to current-column
  "Return t iff all characters to the left are indentation chars."
  (save-excursion
    (while (and (not (bolp))
                (pcase (char-before)
                  ((or ?\s ?\t) (or (backward-char 1) t)))))
    (bolp)))

(defun elastindent-do-1 (force-propagate start-col change-end)
  "Adjust width of indentations.
This is in response to a change starting at point and ending at
CHANGE-END.  START-COL is the minimum column where a change
occured.  Start at point and continue until line which cannot be
impacted by the change is found.  Such a line occurs if its
indentation level is less than START-COL and starts after
CHANGE-END.

Thus a large value of START-COL means that little work needs to
be done by this function.  This optimisation is important,
because otherwise one needs to find a line with zero indentation,
which can be much further down the buffer.

If a change spans several lines, then START-COL is ignored, and
changes are propagated until indentation level reaches 0.
FORCE-PROPAGATE forces even single-line changes to be treated
this way."
  (let (prev-widths ; the list of widths of each *column* of indentation of the previous line
        (reference-pos 0) ; the buffer position in the previous line of 1st printable char
        space-widths) ; accumulated widths of columns for current line
    ;; (message "elastindent-do: %s [%s, %s]" start-col (point) change-end)
    (cl-flet* ((get-next-column-width () ; find reference width in the previous line. (effectful)
                 (let* ((l-pos (1- (point)))
                        (w (if prev-widths (pop prev-widths) ; we have a cached width: use it.
                             (if (eql (char-after reference-pos) ?\n) ; we're at the end of the reference line.
                                 (cons nil (if (bolp)
                                               (- 1) ; we're at a space on the 1st column with an empty line above. No indentation here.
                                             (- (abs (elastindent-char-lvl l-pos nil))))) ; leave width as is. Level is the same as char on the left; but not 1st column.
                               (prog1
                                   (cons (elastindent-char-pixel-width reference-pos) (elastindent-char-lvl reference-pos l-pos))
                                 (setq reference-pos (1+ reference-pos)))))))
                   (push w space-widths) ; cache width for next line
                   ;; (message "char copy: %s->%s (w=%s) %s" (1- reference-pos) (point) w prev-widths)
                   w))
               (char-loop () ; take care of one line.
                 ;; copy width from prev-widths, then reference-pos to (point). Loop to end of indentation.
                 ;; Preconditions: cur col = start-col.  prev-widths=nil or contains cached widths
                 ;; (message "@%s %s=>%s %s" (line-number-at-pos) (- reference-pos (length prev-widths)) (point) prev-widths)
                 (while-let ((cur-line-not-ended-c (not (eolp)))
                             (char (char-after))
                             (char-is-indent-c (or (eql char ?\s) (eql char ?\t))))
                   (pcase char
                     (?\s (elastindent-set-char-info (point) (get-next-column-width)))
                     (?\t (elastindent-set-char-info (point)
                                                     (elastindent-avg-info
                                                      (--map (get-next-column-width) (-repeat tab-width ()))))))
                   (forward-char)))
               (next-line () ; advance to next line, maintaining state.
                 (setq prev-widths (nreverse space-widths))
                 (setq space-widths nil)
                 (setq reference-pos (point)) ; we go to next line exactly after we reached the last space
                 (forward-line)))
      (save-excursion
        (when (eq (forward-line -1) 0)
          (setq reference-pos (progn (move-to-column start-col) (point)))))
      ;; (message "%s: first line. update from startcol=%s curcol=%s" (line-number-at-pos) start-col (current-column))
      (when (elastindent-in-indent) ; if not characters are not to be changed.
        (char-loop))
      (next-line)
      (when (or force-propagate (< (point) change-end))
        ;; (message "%s: main phase; update lines from col 0" (line-number-at-pos))
        (when (> start-col 0)  ; reference is wrong now
          (setq start-col 0)
          (setq prev-widths nil)
          (setq reference-pos (save-excursion (forward-line -1) (point))))
        (while (< (point) change-end)
          (char-loop)
          (next-line)))
      ;; (message "%s: propagate changes and stop if indentation is too small" (line-number-at-pos))
      (while (not (elastindent-column-leaves-indent start-col))
        (char-loop)
        (next-line))
      ;; (message "%s: propagation complete" (line-number-at-pos))
      )))

(defun elastindent-change-extend (end)
  "Return the first position after END which does not contain a space."
  (max (point)
       (save-excursion
         (goto-char end)
         ;; if we are changing something then the spaces just after the changes are
         ;; invalid and must be updated.
         (search-forward-regexp "[^\t\s]" nil t)
         (1- (point)))))

(defun elastindent-do-region (force-propagate start end)
  "Adjust width of indentation characters in given region and propagate.
The region is between START and END in current
buffer.  Propagation means to also fix the indentation of the
lines which follow, if their indentation widths might be impacted
by changes in given region.  See `elastindent-do' for the
explanation of FORCE-PROPAGATE."
  (interactive "r")
  (elastindent-clear-region start end)
  (goto-char start)
  (elastindent-do-1 force-propagate (current-column) end))

(defmacro elastindent-with-context (&rest body)
  "Run BODY in a context which is suitable for applying our adjustments."
  (declare (indent 0))
  `(save-match-data ; just in case
     (elastindent-with-suitable-window ; we need a window to compute the character widths.
      (save-excursion
        (without-restriction ; because changes may propagate beyond the restriction.
          (with-silent-modifications
            ,@body))))))

(defun elastindent-do-buffer ()
  "Adjust width of all indentation spaces and tabs in current buffer."
  (interactive)
  (elastindent-with-context
    (elastindent-do-region nil (point-min) (point-max))))

(defun elastindent-do-buffer-if-enabled ()
  "Call `elastindent-do-buffer' if `elastindent-mode' is enabled."
  (interactive)
  (when 'elastindent-mode (elastindent-do-buffer)))

(defun elastindent-clear-region (start end)
  "Remove all `elastindent-mode' properties between START and END."
  (interactive "r")
  (elastindent-clear-region-properties
   start end 'elastindent-adjusted '(elastindent-adjusted
                                     elastindent-width
                                     elastindent-lvl
                                     display
                                     font-lock-face
                                     mouse-face)))

(defun elastindent-clear-buffer ()
  "Remove all `elastindent-mode' properties in buffer."
  (interactive)
  (elastindent-clear-region (point-min) (point-max)))

(defvar-local elastindent-deleted-newline nil
  "Did the last change delete a newline?")

(defun elastindent-before-change-function (start end)
  "Queue a call to `elastindent-do-region' for START and END."
  (setq elastindent-deleted-newline
        (save-excursion
          (goto-char start)
          (search-forward "\n" end t))))

(defvar-local elastindent-queue nil
  "Queue of changes to handle.
We need queueing because some commands (for instance
`fill-paragraph') will cause many changes, which may individually
propagate down the buffer.  Doing all this work many times can
cause visible slowdowns.")

(defun elastindent-after-change-function (start end _len)
  "Call `elastindent-do-region' for START and END."
  (push (list elastindent-deleted-newline (copy-marker start) (copy-marker end t))
        elastindent-queue))

(defun elastindent-handle-queue ()
  "Take care of intervals in queue.
If input comes before the work can be finished, then stop and
continue the work later, when idle."
  (setq elastindent-queue (-sort (-on #'< #'cadr) elastindent-queue))
  (elastindent-with-context
    (goto-char (point-min))
    (while-no-input ;  see post-command-hook documentation.
      (while elastindent-queue
        (pcase (car elastindent-queue)
          (`(,force-propagate ,start ,end)
           (when (> end (point)) ; otherwise the change has already been taken care of.
             (elastindent-do-region force-propagate
                                    (max (point) start) ; portion before point was already done.
                                    end))))
        ;; pop only when we're done so we don't forget something
        (pop elastindent-queue)))
    (when elastindent-queue
      ;; input came: we continue later.
      (run-with-idle-timer 0.2 nil #'elastindent-handle-queue))))

(provide 'elastindent)
;;; elastindent.el ends here
