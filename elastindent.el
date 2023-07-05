;; -*- lexical-binding: t; -*-

;; elastindent-mode and tabble-mode
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

(require 'cl-lib)
(require 'dash)

;;; General terminological note: elastindent-mode is concerned with
;;; adjusting the width of only spaces and tabs which occur before a
;;; printing character (not space or tab) on a line.  We use the word
;;; “indentation” to refer to these tabs or spaces.  It is ambiguous
;;; whether Unicode space characters other than space and (horizontal)
;;; tab should be considered part of the leading space or not, but in
;;; the code we assume it is only spaces and tabs. Thus
;;; elastindent-mode treats other space characters as printing
;;; characters.

;;; Code:

(define-minor-mode elastindent-mode
  "Improves indentation with in variable-pitch face.
 Adjust the width of indentation characters to align the indented
code to the correct position.  The correct position is defined as
the same relative position to the previous line as it were if a
fixed-pitch face was used.

More precisely, any space character on a line with no printing characters before
it will be matched up with a corresponding character on the previous line, if
there is one.  That character may itself be a width-adjusted space, meaning the
width ultimately comes from some other kind of character higher up.

Due to technical limitations, this mode does not try to detect situations where
the font has changed but the text hasn't, which will mess up the alignment.  You
can run ‘elastindent-do-buffer’ to fix it."
  :init-value nil :lighter nil :global nil
  (if elastindent-mode
      (progn
        (elastindent-do-buffer)
        ;; add change function to beginning of list, to ensure it comes before that of
        ;; tabble
        ;; TODO: it should come after aggressive-indent. Not sure if tabble really needs to be after.
        (add-hook 'after-change-functions 'elastindent-after-change-function nil t)
        (add-hook 'text-scale-mode-hook 'elastindent-do-buffer nil t))
    (progn
      (remove-hook 'after-change-functions 'elastindent-after-change-function t)
      (remove-hook 'text-scale-mode-hook 'elastindent-do-buffer t)
      (elastindent-clear-buffer))))

(defun elastindent-char-pixel-width (pos)
  "Return the pixel width of char at POS."
  (if-let (p (get-char-property pos 'elastindent-width))
      p
    (let ((c (car (window-text-pixel-size nil pos (1+ pos)))))
      (if (> c 2) c ;; Emacs bug: sometimes the returned window-text-pixel-size is negative. In this case computing it indirectly like below seems to fix the issue.
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
      (setq pos2 (or (next-single-property-change
                      pos1 cue-prop nil end)
                     end))
      (remove-list-of-text-properties pos1 pos2 props-to-remove)
      (setq pos1 pos2))))

(defconst elastindent-safe-line-regexp
  (rx line-start (* "\t")
      (? (group (not (any "\s\t\n"))
                (* not-newline)))
      line-end)
  "Regexp to test if a line has no indentation spaces.
If so, then it is a safe starting point for the adjustments of
`elastindent-mode'.")

(defun elastindent-set-char-pixel-width (pos w)
  (add-text-properties pos (1+ pos)
                       (list 'display (list 'space :width (list w))
                             'elastindent-adjusted t
                             'elastindent-width w)))

(defun elastindent-do (start)
  "Adjust width of indentation in starting at START.
Continue until a safe line is encountered, and return its
position.  START is assumed to be a line start position with no
indentation."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (let (prev-widths ; the list of width of each *column* of indentation of the previous line, as a list.
            (reference-pos  ; the buffer position in the previous line of 1st printable char
             (line-beginning-position)) ; no indent on 1st line
            (prev-line-end (line-end-position)); end position of previous line
            space-widths) ; accumulated widths of columns for current line
        (cl-flet ((get-next-column-width ()
                    (let ((w (if prev-widths (pop prev-widths) ; we have a cached width: use it.
                               (if (>= reference-pos prev-line-end) 20 ; arbitrary: TODO: fix
                                 (prog1 (elastindent-char-pixel-width reference-pos)
                                   (setq reference-pos (1+ reference-pos)))))))
                      (push w space-widths)
                      w)))
          (while (and (not (eobp)); in case buffer ended without newline
                      (forward-line) ; jump to beginning of next line...
                      (not (looking-at elastindent-safe-line-regexp)))
            ;; (message "line loop: %s, %s" (point) prev-widths)
            ;; loop over chars
            (while-let ((cur-line-ended-c (not (eolp)))
                        (char (char-after))
                        (char-is-indent-c (or (eql char ?\s) (eql char ?\t))))
              (cond
               ((eql char ?\s)
                (elastindent-set-char-pixel-width (point) (get-next-column-width)))
               ((eql char ?\t)
                (elastindent-set-char-pixel-width
                 (point) (-sum (--map (get-next-column-width) (-repeat tab-width ()))))))
              ;; advance
              (forward-char))
            (setq prev-widths (reverse space-widths))
            (setq space-widths nil)
            (setq prev-line-end (line-end-position))
            (setq reference-pos (point)))))
      ;; return start of first line we did not process, which is guaranteed to
      ;; have no matching chars (so may be passed back to this function).  if
      ;; we processed all lines in buffer, this will be end of buffer (not
      ;; necessarily start of line)
      (point))))

;; TODO: instead of safe line: consider the lowest column at which a change was made.
;; return start of a safe line (or first line) before or including pos
(defun elastindent-find-safe-start (pos)
  "Start of a safe line (or first line) before or including POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (if (looking-at elastindent-safe-line-regexp)
        (point)
      (or (re-search-backward elastindent-safe-line-regexp nil t)
          (point-min)))))

;; return end of a safe line (or last line) after and *excluding* pos.  this one
;; is exclusive because the after-change-function needs to consider the
;; possibility that a line that looks safe now still needs to be re-processed
;; because it has old adjustments that were spoiled by changes in the previous
;; line
(defun elastindent-find-safe-end (pos)
  "Return end of a safe line (or last line) after and excluding POS."
  (save-excursion
    (goto-char pos)
    (forward-line)
    (or (re-search-forward elastindent-safe-line-regexp nil t)
        (point-max))))

(defun elastindent-do-region (start end)
  "Adjust width of all indentation spaces and tabs in given region.
The region is between START and END in current buffer"
  (interactive "r")
  (elastindent-with-suitable-window
    (let* ((start (elastindent-find-safe-start start))
           (end (elastindent-find-safe-end end))
           (pos start))
      (elastindent-clear-region start end)
      (while (and pos (< pos end))
        (setq pos (elastindent-do pos))))))

(defun elastindent-do-buffer ()
  "Adjust width of all indentation spaces and tabs in current buffer."
  (interactive)
  (elastindent-do-region (point-min) (point-max)))

(defun elastindent-clear-region (start end)
  "Remove all `elastindent-mode' properties between START and END."
  (interactive "r")
  (elastindent-clear-region-properties
   start end 'elastindent-adjusted '(elastindent-adjusted elastindent-width display)))

(defun elastindent-clear-buffer ()
  (interactive)
  "Remove all `elastindent-mode' properties in buffer."
  (elastindent-clear-region (point-min) (point-max)))

(defmacro elastindent-with-reported-errors (name &rest body)
  "Execute BODY, trapping and reporting any errors which occur.
This is a debugging utility for `elastindent-mode' and
`tabble-mode', which catches errors and shows a message prefixed
with NAME.  It's really annoying when even a single error in an
after-change or before-change function causes everything to
immediately stop functioning.  I have removed calls to this macro
from the public release because it circumvents a well-conceived
safety feature."
  (declare (indent 1))
  (let ((err (make-symbol "error")))
    `(condition-case ,err
         (progn ,@body)
       (error (message "%s: trapped error during execution:\n%s"
                       ,name
                       (error-message-string ,err))))))

(defun elastindent-after-change-function (start end _len)
  (save-match-data
    (elastindent-do-region start end)))

(provide 'elastindent-mode)
