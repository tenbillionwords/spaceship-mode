;; tabble-mode --- alignment using tabs with variable pitch fonts. -*- lexical-binding: t; -*-
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
;;; terminology note in spaceship-mode.el

;;; Code:
(require 'cl-lib)
(require 'spaceship-mode)

(defcustom tabble-column-minimum-margin 12
  "Minimum size of the space that replaces a tab.  Expressed in pixels."
  :type 'int :group 'tabble)

(defun tabble-mode-maybe ()
  "Function to put in hooks, for example `prog-mode-hook'."
  ;; See org-src-font-lock-fontify-block for buffer name.  tabble
  ;; isn't needed in fontification buffers. Fontification is called on
  ;; every keystroke (‽). Calling tabble-do-buffer on each
  ;; keystroke on the whole block is very slow.
  (unless (string-prefix-p " *org-src-fontification:" (buffer-name))
    (tabble-mode)))

(define-minor-mode tabble-mode
  "Mode for aligned tables with variable pitch fonts.
When `tabble-mode' is enabled, tabstops in consecutive lines are the same.

This is implemented by automatically adjusting the width of tab
characters which occur after the first printing char on a
line (henceforth: “tabble tabs”) so to allow forming a kind of
table (“tabbles”) is adjusted for alignment.  A tabble is formed
by a sequence of consecutive lines which each have tabble tabs
and all have the same leading-space, and the corresponding tabble
tabs are adjusted so that the following text has the same
horizontal position on each line.

One consequence of these rules is that every tabble cell in the first column
must have an entry, to avoid ending the tabble.  Other columns can be
empty (which happens when there are consecutive tabble tabs)."
  :init-value nil :lighter nil :global nil
  (if tabble-mode
      (progn
        (tabble-do-buffer)
        ;; append the change function, to ensure it comes after that of spaceship
        (add-hook 'after-change-functions 'tabble-after-change-function t t)
        (add-hook 'text-scale-mode-hook 'tabble-do-buffer t t))
    (progn
      (remove-hook 'after-change-functions 'tabble-after-change-function t)
      (remove-hook 'text-scale-mode-hook 'tabble-do-buffer t)
      (tabble-clear-buffer))))

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

(defun tabble-do (start)
  "Update alignment of the tabble starting at START.
Do this by parsing and propertizing the tabble.  We assume START
is correct."
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

(defface tabble-column-separator-face '((t)) "Face of column separators in a tabble.")

(defun tabble-cursor-sensor (_window _pos action)
  "Cursor sensor function for `tabble-mode'.
This defun is added to the cursor-sensor-functions properties of
tabble separators.  Depending on ACTION a tabble separator, show
or hide the separator boundaries by changing face attributes."
  (if (eq action 'entered)
      (face-spec-set 'tabble-column-separator-face '((t (:box (:line-width (-1 . 0))))))
      (face-spec-set 'tabble-column-separator-face '((t )))))

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
            (add-text-properties
             pos (1+ pos)
             (list 'display
                   (list 'space :width
                         (list (- (+ (aref (tabble-max-widths the-tabble) col)
                                     tabble-column-minimum-margin)
                                  (tabble-cell-width cell))))
                   'font-lock-face 'tabble-column-separator-face
                   'cursor-sensor-functions (list 'tabble-cursor-sensor)
                   'tabble-adjusted t)))))))

;; return start of a non-tabble line entirely before pos, if possible, or
;; beginning of buffer otherwise.  we need to see a non-tabble line to be safe in
;; case of changes on a line that affect a tabble which began earlier and used to
;; include this line but now doesn't.
(defun tabble-find-safe-start (pos)
  "Return start of a non-tabble line entirely before POS.
If such a like does not exist, return the beginning of the
buffer."
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
  "Update alignment of all tabbles intersecting in the given region.
The region is between START and END in current buffer."
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
  "Reajust tabbles in the current buffer."
  (interactive)
  (tabble-do-region (point-min) (point-max)))

(defun tabble-do-buffer-if-enabled ()
  "Call `tabble-do-buffer' if `tabble-mode' is enabled."
  (interactive)
  (when 'tabble-mode (tabble-do-buffer)))

;; this should happen *after* the spaceship change function, because we may need
;; to know spaceship adjusted space widths in calculating our column widths (but
;; there is no dependency the other way)
(defun tabble-after-change-function (start end _len)
  ""
  (save-match-data
    (tabble-do-region start end)))

(defun tabble-clear-region (start end)
  (spaceship-clear-region-properties
   start end 'tabble-adjusted '(tabble-adjusted display)))

(defun tabble-clear-buffer ()
  (tabble-clear-region (point-min) (point-max)))

(provide 'tabble-mode)
;;; tabble-mode.el ends here
