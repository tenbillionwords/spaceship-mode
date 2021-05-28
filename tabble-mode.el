;; -*- lexical-binding: t; -*-

;; spaceship-mode and tabble-mode
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

;;; Terminology: a tab after a printing char on a line is a “tabble tab” and a
;;; line which has one is a “tabble line”.  A sequence of one or more
;;; consecutive lines which have tabble tabs is a single “tabble”.  See also the
;;; terminology note in spacious-mode.el

(require 'cl-lib)
(require 'spaceship-mode)

(defvar tabble-column-minimum-margin 12) ; in pixels

(define-minor-mode tabble-mode
  "Automatically adjust the width of tab characters which occur after the first
printing char on a line (henceforth: “tabble tabs”) so to allow forming a kind
of table (“tabbles”).  A tabble is formed by a sequence of consecutive lines
which each have tabble tabs and all have the same leading-space, and the
corresponding tabble tabs are adjusted so that the following text has the same
horizontal position on each line.

One consequence of these rules is that every tabble cell in the first column
must have an entry, to avoid ending the tabble.  Other columns can be
empty (which happens when there are consecutive tabble tabs)."
  :init-value nil :lighter nil :global nil
  (if tabble-mode
      (tabble-mode--enable)
    (tabble-mode--disable)))

(defun tabble-mode--enable ()
  (tabble-do-buffer)
  ;; append the change function, to ensure it comes after that of spaceship
  (add-hook
   'after-change-functions #'tabble-after-change-function t t))

(defun tabble-mode--disable ()
  (remove-hook
   'after-change-functions #'tabble-after-change-function t)
  (tabble-clear-buffer))

(cl-defstruct tabble rows (num-cols 0) (max-widths []))
(cl-defstruct tabble-cell start end width)

;; WARNING: under certain circumstances, these rules imply that line-trailing
;; whitespace is significant.  To some extent, this is unavoidable, because you
;; want the tabble to look right *as you're typing it*, including having the
;; cursor show up in the right place right after you enter a tab char.  But
;; another case is where a tabble is held together by a line whose only tabble tab
;; is at the end of the line.  It's probably bad style to do that, but we don't
;; want to forbid it either, because it would require an ad hoc exception to the
;; above rules making this code harder to implement correctly and maintain.

;; The rows of a tabble are its lines, and the cells of each row are the strings
;; separated by tabs, with enough implicit empty cells in each row to make the
;; number of columns consistent for the whole tabble.

(defconst non-tabble-line-regexp
  (rx bol
      (* blank)
      (? (group (not (any blank "\n")) ; after first printing char...
                (* (not (any "\t\n"))))) ; any tab would be a tabble tab
      eol))

(defconst tabble-line-regexp
  (rx bol
      (* blank)
      (not (any blank "\n"))
      (* (not (any "\t\n")))
      "\t"))

(defconst tabble-leading-space-regexp
  ;; this always matches
  (rx (* "\t") (* (group (+ "\s") (+ "\t")))))

(defun tabble-leading-space-string (pos)
  (save-excursion
    (goto-char pos)
    (looking-at tabble-leading-space-regexp)
    (match-string-no-properties 0)))

;; parse and propertize the tabble starting a start (assume start is correct)
(defun tabble-do (start)
  "Update alignment of the tabble starting at START."
  (save-excursion
    (goto-char start)
    (let* ((leading-space (tabble-leading-space-string (point)))
           (leading-space-len (length leading-space))
           (the-tabble (make-tabble)))
      (while (and (not (eobp))
                  (equal leading-space (tabble-leading-space-string (point)))
                  (looking-at tabble-line-regexp))
        (forward-char leading-space-len)
        (tabble-add-row the-tabble (point))
        (forward-line))
      ;; note that rows are in reverse order, currently this shouldn't matter
      (tabble-propertize the-tabble)
      (point))))

;; scan a row (line) and add it to the tabble, assuming pos is at end of
;; leading-space
(defun tabble-add-row (the-tabble pos)
  "Apply text properties to the tabble represented by THE-TABBLE after
calculating the correct widths needed to align the columns."
  (save-excursion
    (goto-char pos)
    (let ((line-end (line-end-position))
          (old-num-cols (tabble-num-cols the-tabble))
          cells len)
      (while (< (point) line-end)
        (looking-at "[^\t\n]*")
        (push (make-tabble-cell
               :start (point)
               :end (match-end 0)
               :width (spaceship-text-pixel-width (point) (match-end 0)))
              cells)
        (goto-char (match-end 0))
        (unless (eobp) (forward-char)))
      (setq len (length cells))
      (setq cells (nreverse cells))
      ;; add more columns to the tabble if needed
      (when (< old-num-cols len)
        (setf (tabble-max-widths the-tabble)
              (cl-concatenate 'vector
                              (tabble-max-widths the-tabble)
                              (make-vector (- len old-num-cols) 0)))
        (setf (tabble-num-cols the-tabble) len))
      ;; update the column widths
      (cl-loop for i below (tabble-num-cols the-tabble)
               for cell in cells
               when (< (aref (tabble-max-widths the-tabble) i)
                       (tabble-cell-width cell))
               do (setf (aref (tabble-max-widths the-tabble) i)
                        (tabble-cell-width cell)))
      ;; add the row
      (push cells (tabble-rows the-tabble))
      )))

(defun tabble-propertize (the-tabble)
  (with-silent-modifications
    (dolist (row (tabble-rows the-tabble))
      (cl-loop
       for cell in row
       for col from 0
       for pos = (tabble-cell-end cell)
       ;; avoid propertizing newline after last cell
       when (equal (char-after pos) ?\t)
       do (progn
            (put-text-property
             pos (+ pos 1) 'display
             (list 'space :width
                   (list (- (+ (aref (tabble-max-widths the-tabble) col)
                               tabble-column-minimum-margin)
                            (tabble-cell-width cell)))))
            (put-text-property
             pos (+ pos 1) 'tabble-adjusted
             t))))))

;; return start of a non-tabble line entirely before pos, if possible, or
;; beginning of buffer otherwise.  we need to see a non-tabble line to be safe in
;; case of changes on a line that affect a tabble which began earlier and used to
;; include this line but now doesn't.
(defun tabble-find-safe-start (pos)
  "Return start of a non-tabble line entirely before POS, or the beginning of
the buffer."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (or (re-search-backward non-tabble-line-regexp nil t)
        (point-min))))

(defun tabble-find-safe-end (pos)
  "Return end of a non-tabble line entirely after POS, or end of buffer."
  (save-excursion
    (goto-char pos)
    (forward-line)
    (or (re-search-forward non-tabble-line-regexp nil t)
        (point-max))))

(defun tabble-do-region (start end)
  "Update alignment of all tabbles intersecting the region between START and END
in current buffer."
  (spaceship-with-suitable-window
    (let ((start (tabble-find-safe-start start))
          (end (tabble-find-safe-end end)))
      (tabble-clear-region start end)
      (save-excursion
        (goto-char start)
        (re-search-forward tabble-line-regexp end :move-to-end)
        (while (< (point) end)
          (beginning-of-line)
          (goto-char (tabble-do (point)))
          (re-search-forward tabble-line-regexp end :move-to-end))))))

(defun tabble-do-buffer ()
  (interactive)
  (tabble-do-region (point-min) (point-max)))

;; this should happen *after* the spaceship change function, because we may need
;; to know spaceship adjusted space widths in calculating our column widths (but
;; there is no dependency the other way)
(defun tabble-after-change-function (start end len)
  (save-match-data
    (tabble-do-region start end)))

(defun tabble-clear-region (start end)
  (spaceship-clear-region-properties
   start end 'tabble-adjusted '(tabble-adjusted display)))

(defun tabble-clear-buffer ()
  (tabble-clear-region (point-min) (point-max)))

(provide 'tabble-mode)
