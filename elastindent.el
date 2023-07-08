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
        (elastindent-do-buffer)
        ;; add change function to beginning of list, to ensure it comes before that of
        ;; tabble
        (add-hook 'after-change-functions 'elastindent-after-change-function nil t)
        (add-hook 'text-scale-mode-hook 'elastindent-do-buffer nil t))
    (progn
      (remove-hook 'after-change-functions 'elastindent-after-change-function t)
      (remove-hook 'text-scale-mode-hook 'elastindent-do-buffer t)
      (elastindent-clear-buffer))))

(defcustom elastindent-reference-col-width 10
  "Default width of a column (space character) in pixels.
When setting the width of a space character after the end of the
previous line, there is no column to align to.  In this case, the
width of the column will be set to.
`elastindent-reference-col-width'."
  :type 'int :group 'elastindent)

(defun elastindent-char-pixel-width (pos)
  "Return the pixel width of char at POS."
  (if-let (p (get-char-property pos 'elastindent-width))
      p
    (let ((c (car (window-text-pixel-size nil pos (1+ pos)))))
      (if (> c 2) c
        ;; Emacs bug: sometimes the returned window-text-pixel-size is
        ;; negative. In this case computing it indirectly like below
        ;; seems to fix the issue.
        (- (car (window-text-pixel-size nil (1- pos) (1+ pos)))
           (car (window-text-pixel-size nil (1- pos) pos)))))))

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
  (add-text-properties pos (1+ pos)
                       (list 'display (list 'space :width (list w))
                             'elastindent-adjusted t
                             'elastindent-width w)))

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

(defun elastindent-do (start-col change-end)
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
which can be much further down the file."
  (with-silent-modifications
    (let (prev-widths ; the list of widths of each *column* of indentation of the previous line
          (reference-pos 0) ; the buffer position in the previous line of 1st printable char
          space-widths) ; accumulated widths of columns for current line
      ;; (message "elastindent-do: %s [%s, %s]" start-col (point) change-end)
      (cl-flet* ((get-next-column-width () ; find reference width in the previous line. (effectful)
                   (let ((w (if prev-widths (pop prev-widths) ; we have a cached width: use it.
                              (if (eql (char-after reference-pos) ?\n) elastindent-reference-col-width
                                (prog1 (elastindent-char-pixel-width reference-pos)
                                  (setq reference-pos (1+ reference-pos)))))))
                     (push w space-widths) ; cache width for next line
                     ;; (message "char copy: %s->%s (w=%s) %s" (1- reference-pos) (point) w prev-widths)
                     w))
                 (char-loop ()
                   ;; copy width from prev-widths, then reference-pos to (point). Loop to end of indentation.
                   ;; Preconditions: cur col = start-col.  prev-widths=nil or contains cached widths
                   ;; (message "@%s %s=>%s %s" (line-number-at-pos) (- reference-pos (length prev-widths)) (point) prev-widths)
                   (while-let ((cur-line-not-ended-c (not (eolp)))
                               (char (char-after))
                               (char-is-indent-c (or (eql char ?\s) (eql char ?\t))))
                     (pcase char
                       (?\s (elastindent-set-char-pixel-width (point) (get-next-column-width)))
                       (?\t (elastindent-set-char-pixel-width
                             (point) (-sum (--map (get-next-column-width) (-repeat tab-width ()))))))
                     (forward-char)))
                 (next-line () ; advance to next line, maintaining state.
                   (setq prev-widths (reverse space-widths))
                   (setq space-widths nil)
                   (setq reference-pos (point)) ; we go to next line exactly after we reached the last space
                   (forward-line)))
        (save-excursion
          (when (eq (forward-line -1) 0)
            (setq reference-pos (save-excursion (move-to-column start-col) (point)))))
        ;; (message "%s: first line. update from col=%s" (line-number-at-pos) (current-column))
        (when (elastindent-in-indent)
          (char-loop)) ; if not characters are not to be changed.
        (next-line)
        (when (< (point) change-end)
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
          (next-line))))))

(defun elastindent-do-region (start end)
  "Adjust width of indentation characters in given region and propagate.
The region is between START and END in current buffer.
Propagation means to also fix the indentation of the lines which
follow, if their indentation widths might be impacted by changes
in given region."
  (interactive "r")
  (elastindent-with-suitable-window
    (save-excursion
      ;; for some reason clearing is necessary for fill-paragraph.
      (elastindent-clear-region start end)
      (goto-char start)
      (let ((col (if (> end (line-end-position)) 0
                   (current-column))))
        (elastindent-do col end)))))

(defun elastindent-do-buffer ()
  "Adjust width of all indentation spaces and tabs in current buffer."
  (interactive)
  (elastindent-do-region (point-min) (point-max)))

(defun elastindent-do-buffer-if-enabled ()
  "Call `elastindent-do-buffer' if `elastindent-mode' is enabled."
  (interactive)
  (when 'elastindent-mode (elastindent-do-buffer)))

(defun elastindent-clear-region (start end)
  "Remove all `elastindent-mode' properties between START and END."
  (interactive "r")
  (elastindent-clear-region-properties
   start end 'elastindent-adjusted '(elastindent-adjusted elastindent-width display)))

(defun elastindent-clear-buffer ()
  "Remove all `elastindent-mode' properties in buffer."
  (interactive)
  (elastindent-clear-region (point-min) (point-max)))

(defun elastindent-after-change-function (start end _len)
  "Call `elastindent-do-region' for START and END."
  (save-match-data (elastindent-do-region start end)))

(provide 'elastindent)
;;; elastindent.el ends here
