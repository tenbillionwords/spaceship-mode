;; elastable-mode --- alignment using tabs with variable pitch fonts. -*- lexical-binding: t; -*-
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

;;; Terminology: a tab after a printing char on a line is a “elastable tab” and a
;;; line which has one is a “elastable line”.  A sequence of one or more
;;; consecutive lines which have elastable tabs is a single “elastable”.

;;; Code:
(require 'cl-lib)
(require 'elastic-tools)

(defcustom elastable-column-minimum-margin 12
  "Minimum size of the space that replaces a tab.  Expressed in pixels."
  :type 'int :group 'elastable)

(defun elastable-mode-maybe ()
  "Function to put in hooks, for example `prog-mode-hook'."
  ;; See org-src-font-lock-fontify-block for buffer name.  elastable
  ;; isn't needed in fontification buffers. Fontification is called on
  ;; every keystroke (‽). Calling elastable-do-buffer on each
  ;; keystroke on the whole block is very slow.
  (unless (string-prefix-p " *org-src-fontification:" (buffer-name))
    (elastable-mode)))

(define-minor-mode elastable-mode
  "Mode for aligned tables with variable pitch fonts.
When `elastable-mode' is enabled, tabstops in consecutive lines are the same.

This is implemented by automatically adjusting the width of tab
characters which occur after the first printing char on a
line (henceforth: “elastable tabs”) so to allow forming a kind of
table (“elastables”) is adjusted for alignment.  A elastable is formed
by a sequence of consecutive lines which each have elastable tabs
and all have the same leading-space, and the corresponding elastable
tabs are adjusted so that the following text has the same
horizontal position on each line.

One consequence of these rules is that every elastable cell in the first column
must have an entry, to avoid ending the elastable.  Other columns can be
empty (which happens when there are consecutive elastable tabs)."
  :init-value nil :lighter nil :global nil
  (if elastable-mode
      (progn
        (elastable-do-buffer)
        (add-hook 'text-scale-mode-hook 'elastable-do-buffer nil t)
        (elastic-tools-add-handler 'elastable-do-region 90))
    (progn
      (elastic-tools-remove-handler 'elastable-do-region))
      (elastable-clear-buffer)))

(cl-defstruct elastable rows (num-cols 0) (max-widths []))
(cl-defstruct elastable-cell start end width)

;; WARNING: under certain circumstances, these rules imply that line-trailing
;; whitespace is significant.  To some extent, this is unavoidable, because you
;; want the elastable to look right *as you're typing it*, including having the
;; cursor show up in the right place right after you enter a tab char.  But
;; another case is where a elastable is held together by a line whose only elastable tab
;; is at the end of the line.  It's probably bad style to do that, but we don't
;; want to forbid it either, because it would require an ad hoc exception to the
;; above rules making this code harder to implement correctly and maintain.

;; The rows of a elastable are its lines, and the cells of each row are the strings
;; separated by tabs, with enough implicit empty cells in each row to make the
;; number of columns consistent for the whole elastable.

(defconst non-elastable-line-regexp
  (rx bol
      (* blank)
      (? (group (not (any blank "\n")) ; after first printing char...
                (* (not (any "\t\n"))))) ; any tab would be a elastable tab
      eol))

(defconst elastable-line-regexp
  (rx bol
      (* blank)
      (not (any blank "\n"))
      (* (not (any "\t\n")))
      "\t"))

(defconst elastable-leading-space-regexp
  ;; this always matches
  (rx (* "\t") (* (group (+ "\s") (+ "\t")))))

(defun elastable-leading-space-string (pos)
  (save-excursion
    (goto-char pos)
    (looking-at elastable-leading-space-regexp)
    (match-string-no-properties 0)))

(defun elastable-do (start)
  "Update alignment of the elastable starting at START.
Do this by parsing and propertizing the elastable.  We assume START
is correct."
  (save-excursion
    (goto-char start)
    (let* ((leading-space (elastable-leading-space-string (point)))
           (leading-space-len (length leading-space))
           (the-elastable (make-elastable)))
      (while (and (not (eobp))
                  (equal leading-space (elastable-leading-space-string (point)))
                  (looking-at elastable-line-regexp))
        (forward-char leading-space-len)
        (elastable-add-row the-elastable (point))
        (forward-line))
      ;; note that rows are in reverse order, currently this shouldn't matter
      (elastable-propertize the-elastable)
      (point))))

;; scan a row (line) and add it to the elastable, assuming pos is at end of
;; leading-space
(defun elastable-add-row (the-elastable pos)
  "Apply text properties to the elastable represented by THE-ELASTABLE after
calculating the correct widths needed to align the columns."
  (save-excursion
    (goto-char pos)
    (let ((line-end (line-end-position))
          (old-num-cols (elastable-num-cols the-elastable))
          cells len)
      (while (< (point) line-end)
        (looking-at "[^\t\n]*")
        (push (make-elastable-cell
               :start (point)
               :end (match-end 0)
               :width (elastic-tools-text-pixel-width (point) (match-end 0)))
              cells)
        (goto-char (match-end 0))
        (unless (eobp) (forward-char)))
      (setq len (length cells))
      (setq cells (nreverse cells))
      ;; add more columns to the elastable if needed
      (when (< old-num-cols len)
        (setf (elastable-max-widths the-elastable)
              (cl-concatenate 'vector
                              (elastable-max-widths the-elastable)
                              (make-vector (- len old-num-cols) 0)))
        (setf (elastable-num-cols the-elastable) len))
      ;; update the column widths
      (cl-loop for i below (elastable-num-cols the-elastable)
               for cell in cells
               when (< (aref (elastable-max-widths the-elastable) i)
                       (elastable-cell-width cell))
               do (setf (aref (elastable-max-widths the-elastable) i)
                        (elastable-cell-width cell)))
      ;; add the row
      (push cells (elastable-rows the-elastable)))))

(defface elastable-column-separator-face '((t)) "Face of column separators in a elastable.")

(defun elastable-cursor-sensor (_window _pos action)
  "Cursor sensor function for `elastable-mode'.
This defun is added to the cursor-sensor-functions properties of
elastable separators.  Depending on ACTION a elastable separator, show
or hide the separator boundaries by changing face attributes."
  (if (eq action 'entered)
      (face-spec-set 'elastable-column-separator-face '((t (:box (:line-width (-1 . 0))))))
      (face-spec-set 'elastable-column-separator-face '((t )))))

(defun elastable-propertize (the-elastable)
  (with-silent-modifications
    (dolist (row (elastable-rows the-elastable))
      (cl-loop
       for cell in row
       for col from 0
       for pos = (elastable-cell-end cell)
       ;; avoid propertizing newline after last cell
       when (equal (char-after pos) ?\t)
       do (progn
            (add-text-properties
             pos (1+ pos)
             (list 'display
                   (list 'space :width
                         (list (- (+ (aref (elastable-max-widths the-elastable) col)
                                     elastable-column-minimum-margin)
                                  (elastable-cell-width cell))))
                   'font-lock-face 'elastable-column-separator-face
                   'cursor-sensor-functions (list 'elastable-cursor-sensor)
                   'elastable-adjusted t)))))))

;; return start of a non-elastable line entirely before pos, if possible, or
;; beginning of buffer otherwise.  we need to see a non-elastable line to be safe in
;; case of changes on a line that affect a elastable which began earlier and used to
;; include this line but now doesn't.
(defun elastable-find-safe-start (pos)
  "Return start of a non-elastable line entirely before POS.
If such a like does not exist, return the beginning of the
buffer."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (or (re-search-backward non-elastable-line-regexp nil t)
        (point-min))))

(defun elastable-find-safe-end (pos)
  "Return end of a non-elastable line entirely after POS, or end of buffer."
  (save-excursion
    (goto-char pos)
    (forward-line)
    (or (re-search-forward non-elastable-line-regexp nil t)
        (point-max))))

(defun elastable-do-region (_ start end)
  "Update alignment of all elastables intersecting in the given region.
The region is between START and END in current buffer."
    (let ((start (elastable-find-safe-start start))
          (end (elastable-find-safe-end end)))
      (elastable-clear-region start end)
      (goto-char start)
      (while (re-search-forward elastable-line-regexp end :move-to-end)
        (beginning-of-line)
        (goto-char (elastable-do (point))))))

(defun elastable-do-buffer ()
  "Reajust elastables in the current buffer."
  (interactive)
  (elastic-tools-with-context
    (elastable-do-region nil (point-min) (point-max))))

(defun elastable-do-buffer-if-enabled ()
  "Call `elastable-do-buffer' if `elastable-mode' is enabled."
  (interactive)
  (when 'elastable-mode (elastable-do-buffer)))

(defun elastable-clear-region (start end)
  (elastic-tools-clear-region-properties
   start end 'elastable-adjusted '(elastable-adjusted display)))

(defun elastable-clear-buffer ()
  (elastable-clear-region (point-min) (point-max)))

(provide 'elastable-mode)
;;; elastable-mode.el ends here
