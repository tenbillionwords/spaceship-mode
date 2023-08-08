;; elastic-table --- alignment using tabs with variable pitch fonts. -*- lexical-binding: t; -*-
;; Copyright (C) 2023 JP Bernardy
;; Copyright (C) 2021 Scott Messick (tenbillionwords)

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

;;; Terminology: a tab after a printing char on a line is a “elastic tab” and a
;;; line which has one is a “elastic table line”.  A sequence of one or more
;;; consecutive lines which have elastic tabs is a single “elastic table”.

;;; Code:
(require 'cl-lib)
(require 'elastic-tools)

(defcustom elastic-table-column-minimum-margin nil
  "Minimum size of the space that replaces a tab.  Expressed in pixels.
By default, `frame-char-width` will be used."
  :type 'int :group 'elastic-table)

(define-minor-mode elastic-table-mode
  "Mode for alignment of using tabs, with variable pitch fonts.
When `elastic-table-mode' is enabled, tabstops in consecutive
lines are the same.

This is implemented by automatically adjusting the width of tab
characters which occur after the first printing char on a
line (henceforth: “elastic tabs”) so to allow forming a kind of
table (“elastic tables”) is adjusted for alignment.  An elastic
table is formed by a sequence of consecutive lines which each
have elastic tabs and all have the same leading-space, and
the corresponding elastic tabs are adjusted so that the
following text has the same horizontal position on each line.

One consequence of these rules is that every elastic table cell
in the first column must have an entry, to avoid ending the
table.  Other columns can be empty (which happens when there are
consecutive elastic tabs)."
  :init-value nil :lighter nil :global nil
  (if elastic-table-mode
      (elastic-tools-add-handler 'elastic-table-do-region 90)
    (progn
      (elastic-tools-remove-handler 'elastic-table-do-region))
      (elastic-table-clear-buffer)))

(cl-defstruct elastic-table rows (num-cols 0) (max-widths []))
(cl-defstruct elastic-table-cell start end width)

;; WARNING: under certain circumstances, these rules imply that line-trailing
;; whitespace is significant.  To some extent, this is unavoidable, because you
;; want the elastic table to look right *as you're typing it*, including having the
;; cursor show up in the right place right after you enter a tab char.  But
;; another case is where an elastic table is held together by a line whose only elastic tab
;; is at the end of the line.  It's probably bad style to do that, but we don't
;; want to forbid it either, because it would require an ad hoc exception to the
;; above rules making this code harder to implement correctly and maintain.

;; The rows of a elastic table are its lines, and the cells of each row are the strings
;; separated by tabs, with enough implicit empty cells in each row to make the
;; number of columns consistent for the whole elastic table.

(defconst non-elastic-table-line-regexp
  (rx bol
      (* blank)
      (? (group (not (any blank "\n")) ; after first printing char...
                (* (not (any "\t\n"))))) ; any tab would be a elastic-table tab
      eol))

(defconst elastic-table-line-regexp
  (rx bol
      (* blank)
      (not (any blank "\n"))
      (* (not (any "\t\n")))
      "\t"))

(defconst elastic-table-leading-space-regexp
  ;; this always matches
  (rx (* "\t") (* (group (+ "\s") (+ "\t")))))

(defun elastic-table-leading-space-string (pos)
  (save-excursion
    (goto-char pos)
    (looking-at elastic-table-leading-space-regexp)
    (match-string-no-properties 0)))

(defun elastic-table-do (start)
  "Update alignment of the elastic table starting at START.
Do this by parsing and propertizing the elastic table.  We assume START
is correct."
  (save-excursion
    (goto-char start)
    (let* ((leading-space (elastic-table-leading-space-string (point)))
           (leading-space-len (length leading-space))
           (the-table (make-elastic-table)))
      (while (and (not (eobp))
                  (equal leading-space (elastic-table-leading-space-string (point)))
                  (looking-at elastic-table-line-regexp))
        (forward-char leading-space-len)
        (elastic-table-add-row the-table (point))
        (forward-line))
      ;; note that rows are in reverse order, currently this shouldn't matter
      (elastic-table-propertize the-table)
      (point))))

(defun elastic-table-add-row (the-table pos)
  "Scan a row and add it to THE-TABLE.
Assuming POS is at end of leading-space."
  (save-excursion
    (goto-char pos)
    (let ((line-end (line-end-position))
          (old-num-cols (elastic-table-num-cols the-table))
          cells len)
      (while (< (point) line-end)
        (looking-at "[^\t\n]*")
        (push (make-elastic-table-cell
               :start (point)
               :end (match-end 0)
               :width (elastic-tools-text-pixel-width (point) (match-end 0)))
              cells)
        (goto-char (match-end 0))
        (unless (eobp) (forward-char)))
      (setq len (length cells))
      (setq cells (nreverse cells))
      ;; add more columns to the elastic-table if needed
      (when (< old-num-cols len)
        (setf (elastic-table-max-widths the-table)
              (cl-concatenate 'vector
                              (elastic-table-max-widths the-table)
                              (make-vector (- len old-num-cols) 0)))
        (setf (elastic-table-num-cols the-table) len))
      ;; update the column widths
      (cl-loop for i below (elastic-table-num-cols the-table)
               for cell in cells
               when (< (aref (elastic-table-max-widths the-table) i)
                       (elastic-table-cell-width cell))
               do (setf (aref (elastic-table-max-widths the-table) i)
                        (elastic-table-cell-width cell)))
      ;; add the row
      (push cells (elastic-table-rows the-table)))))

(defface elastic-table-column-separator-face '((t)) "Face of column separators in a elastic-table.")

(defun elastic-table-cursor-sensor (_window _pos action)
  "Cursor sensor function for `elastic-table-mode'.
This defun is added to the cursor-sensor-functions properties of
elastic-table separators.  Depending on ACTION a elastic-table separator, show
or hide the separator boundaries by changing face attributes."
  (if (eq action 'entered)
      (face-spec-set 'elastic-table-column-separator-face '((t (:box (:line-width (-1 . 0))))))
      (face-spec-set 'elastic-table-column-separator-face '((t )))))

(defun elastic-table-propertize (the-table)
  (let ((min-col-sep (or elastic-table-column-minimum-margin
                         (frame-char-width))))
    (dolist (row (elastic-table-rows the-table))
      (cl-loop
       for cell in row
       for col from 0
       for pos = (elastic-table-cell-end cell)
       ;; avoid propertizing newline after last cell
       when (equal (char-after pos) ?\t)
       do (progn
            (add-text-properties
             pos (1+ pos)
             (list 'display
                   (list 'space :width
                         (list (- (+ (aref (elastic-table-max-widths the-table) col)
                                     min-col-sep)
                                  (elastic-table-cell-width cell))))
                   'font-lock-face 'elastic-table-column-separator-face
                   'cursor-sensor-functions (list 'elastic-table-cursor-sensor)
                   'elastic-table-adjusted t)))))))

;; return start of a non-elastic-table line entirely before pos, if possible, or
;; beginning of buffer otherwise.  we need to see a non-elastic-table line to be safe in
;; case of changes on a line that affect a elastic-table which began earlier and used to
;; include this line but now doesn't.
(defun elastic-table-find-safe-start (pos)
  "Return start of a non-elastic-table line entirely before POS.
If such a like does not exist, return the beginning of the
buffer."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (or (re-search-backward non-elastic-table-line-regexp nil t)
        (point-min))))

(defun elastic-table-find-safe-end (pos)
  "Return end of a non-elastic-table line entirely after POS, or end of buffer."
  (save-excursion
    (goto-char pos)
    (forward-line)
    (or (re-search-forward non-elastic-table-line-regexp nil t)
        (point-max))))

(defun elastic-table-do-region (_ start end)
  "Update alignment of all elastic-tables intersecting in the given region.
The region is between START and END in current buffer."
    (let ((start (elastic-table-find-safe-start start))
          (end (elastic-table-find-safe-end end)))
      (elastic-table-clear-region start end)
      (goto-char start)
      (while (re-search-forward elastic-table-line-regexp end :move-to-end)
        (beginning-of-line)
        (goto-char (elastic-table-do (point))))))

(defun elastic-table-clear-region (start end)
  (elastic-tools-clear-region-properties
   start end 'elastic-table-adjusted '(elastic-table-adjusted display)))

(defun elastic-table-clear-buffer ()
  (elastic-table-clear-region (point-min) (point-max)))

(provide 'elastic-table)
;;; elastic-table.el ends here
