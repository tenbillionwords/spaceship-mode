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

(require 'cl-lib)

;;; General terminological note: spaceship-mode is concerned with adjusting the
;;; width of only spaces and tabs which occur before a printing character (not
;;; space or tab) on a line.  To avoid any possible ambiguity, we do not use the
;;; word “indentation” (which might refer only to tabs, not to alignment spaces)
;;; and instead speak of “leading-space spaces” or “leading-space tabs”, where
;;; “leading-space” means the characters on a line before the first printing
;;; char.  It is ambiguous whether Unicode space characters other than space and
;;; (horizontal) tab should be considered part of the leading space or not, but
;;; in the code we assume it is only spaces and tabs, because there is no
;;; possible purpose that including other space characters could serve.  Thus
;;; spaceship-mode treats other space characters as printing characters.

;;; Code:

(defcustom spaceship-tab-pixel-width 20
  "‘spaceship-mode’ will adjust all leading-space tabs to have this width.")

(defcustom spaceship-auto-preserve nil
  "Whether ‘spaceship-mode’ should attempt to preserve the alignment of blocks
of code or text aligned to a line when that line is edited before the point of
alignment.")

(define-minor-mode spaceship-mode
  "Automatically adjust width of space characters in certain places to allow
better text alignment with a variable-width font.

More precisely, any space character on a line with no printing characters before
it will be matched up with a corresponding character on the previous line, if
there is one.  That character may itself be a width-adjusted space, meaning the
width ultimately comes from some other kind of character higher up.

As an exception, a space will not be matched with a tab, but a tab may be.  (A
tab does not match anything else.)  Once a mismatch occurs, no further
adjustments will happen on this line.

Due to technical limitations, this mode does not try to detect situations where
the font has changed but the text hasn't, which will mess up the alignment.  You
can run ‘spaceship-do-buffer’ to fix it."
  :init-value nil :lighter nil :global nil
  (if spaceship-mode
      (spaceship-mode--enable)
    (spaceship-mode--disable)))

(defun spaceship-mode--enable ()
  (spaceship-do-buffer)
  ;; add change function to beginning of list, to ensure it comes before that of
  ;; tabble
  (add-hook 'before-change-functions 'spaceship-before-change-function nil t)
  (add-hook 'after-change-functions 'spaceship-after-change-function nil t)
  (add-hook 'text-scale-mode-hook 'spaceship-do-buffer nil t))

(defun spaceship-mode--disable ()
  (remove-hook 'after-change-functions 'spaceship-after-change-function t)
  (remove-hook 'text-scale-mode-hook 'spaceship-do-buffer t)
  (spaceship-clear-buffer))

(defun spaceship-text-pixel-width (start end)
  "Return the pixel width of text between START and END in current buffer."
  (let ((c (car (window-text-pixel-size nil start end))))
    (if (> c 2) c ;; Emacs bug: sometimes the returned window-text-pixel-size is negative. In this case computing it indirectly like below seems to fix the issue.
      (- (car (window-text-pixel-size nil (1- start) end))
         (car (window-text-pixel-size nil (1- start) start))))))

(defun spaceship-char-pixel-width (pos)
  "Return the pixel width of char at POS."
  (if-let (p (get-char-property pos 'spaceship-width))
      p
    (let ((c (car (window-text-pixel-size nil pos (1+ pos)))))
      (if (> c 2) c ;; Emacs bug: sometimes the returned window-text-pixel-size is negative. In this case computing it indirectly like below seems to fix the issue.
      (- (car (window-text-pixel-size nil (1- pos) (1+ pos)))
         (car (window-text-pixel-size nil (1- pos) pos)))))))

(defun spaceship-show-text-pixel-width (start end)
  "Display pixel width of region text in minibuffer (debug utility for
spaceship-mode)"
  (interactive "r")
  (message "pixel width of region: %s dbg %s, prop %s"
           (car (window-text-pixel-size nil start end))
           (spaceship-text-pixel-width start end)))

(defun spaceship-show-char-pixel-width (pos)
  "Display pixel width of region char at POS .
(debug utility for spaceship-mode)"
  (interactive "d")
  (message "pixel width of region: %s, raw=%s, disp=%s, prop=%s"
           (spaceship-char-pixel-width pos)
           (car (window-text-pixel-size nil pos (1+ pos)))
           (get-char-property pos 'spaceship-width)
           (get-char-property pos 'display)))

(defmacro spaceship-with-suitable-window (&rest body)
  "Executes BODY in a context where current buffer is guaranteed to have a
window associated with it."
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

(defun spaceship-clear-region-properties (start end cue-prop props-to-remove)
  "Clear all properties in list PROPS-TO-REMOVE from all text between START and
END such that CUE-PROP is t."
  (with-silent-modifications
    (cl-do ((pos1 start) pos2) (nil)
      (setq pos1 (text-property-any pos1 end cue-prop t))
      (unless pos1 (cl-return))
      (setq pos2 (or (next-single-property-change
                      pos1 cue-prop nil end)
                     end))
      (remove-list-of-text-properties pos1 pos2 props-to-remove)
      (setq pos1 pos2))))

;; the following code was for displaying all tabs as a certain specific width,
;; rather than using tabstops, by adding a display-table entry for the tab char.
;; it turns out that this messes up the behavior of ‘window-text-pixel-size’ so
;; we don't use this method anymore.

;; (define-minor-mode spaceship-simple-tabs-mode
;;   "Display all tab characters with the same width regardless of
;; where they occur.  The width is determined by
;; ‘spaceship-simple-tabs-width’, which is an integer representing
;; the equivalent number of spaces (in current font).  The default
;; is 4."
;;   :init-value nil :lighter nil :global nil
;;   (if spaceship-simple-tabs-mode
;;       (spaceship-set-tab-display spaceship-simple-tabs-width)
;;     (spaceship-unset-tab-display)))

;; (defvar spaceship-simple-tabs-width 4
;;   "Controls the width of tab characters in
;; ‘spaceship-simple-tabs-mode’.  You must deactivate and reactivate
;; the mode for changes to take effect.")

;; (defun spaceship-set-tab-display (width)
;;   (unless buffer-display-table
;;     (setq buffer-display-table (make-display-table)))
;;   (set-char-table-range buffer-display-table
;;                         ?\t
;;                         (make-vector (or width tab-width)
;;                                      (make-glyph-code ?\s))))

;; (defun spaceship-unset-tab-display ()
;;   (when buffer-display-table
;;     (set-char-table-range buffer-display-table
;;                           ?\t
;;                           nil)))

(defun spaceship-do-tabs (start end)
  "Adjust width of all leading-space tabs to have the standard width."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (while (< (point) end)
        (cl-block inner-loop
          (let ((line-end (line-end-position)))
            (while t
              (cl-case (char-after)
                (?\t (put-text-property
                      (point) (+ (point) 1)
                      'display
                      (list 'space :width
                            (list spaceship-tab-pixel-width)))
                     (forward-char))
                (?\s (skip-chars-forward "\s" line-end)
                     (when (>= (point) line-end)
                       (cl-return-from inner-loop)))
                (t (cl-return-from inner-loop))))))
        (forward-line)))))

(defconst spaceship-safe-line-regexp
  (rx line-start (* "\t")
      (? (group (not (any "\s\t\n"))
                (* not-newline)))
      line-end)
  "Regexp to test whether a line is a safe starting point for the adjustments of
spaceship-mode, meaning it has no leading-space spaces.")

(defun spaceship-do (start)
  "Adjust width of leading-space spaces in current buffer starting at the line
with START and continuing to the next safe line."
  (with-silent-modifications
    (save-excursion
      (goto-char start)
      ;; we assume the first line does not need to be processed
      (let ((prev-line-start)
            prev-line-end
            cur-line-start cur-line-end
            char candidate candidate-pos)
        (setq prev-line-start (line-beginning-position))
        (setq prev-line-end (line-end-position))
        (forward-line)
        ;; loop over lines
        (while (not (or (eobp) ; in case buffer ended without newline
                        (looking-at spaceship-safe-line-regexp))) ; includes empty
          (setq cur-line-start (point))
          (setq cur-line-end (line-end-position))
          ;; loop over chars
          (cl-loop (unless (< (point) cur-line-end)
                     (cl-return))
                   (setq candidate-pos (+ prev-line-start
                                          (- (point) cur-line-start)))
                   ;; any more chars on previous line?
                   (unless (< candidate-pos prev-line-end)
                     (cl-return))
                   (setq char (char-after)
                         candidate (char-after candidate-pos))
                   ;; valid matching char?
                   (unless (or (and (eql char ?\s)
                                    (not (eql candidate ?\t)))
                               (and (eql char ?\t)
                                    (eql candidate ?\t)))
                     (cl-return))
                   ;; adjust width
                   (when (eql char ?\s)
                     (let ((w (spaceship-char-pixel-width candidate-pos)))
                       (put-text-property (point) (+ (point) 1)
                                          'display (list 'space :width (list w)))
                       (put-text-property (point) (+ (point) 1)
                                          'spaceship-adjusted t)
                       (put-text-property (point) (+ (point) 1)
                                          'spaceship-width w)))
                   ;; advance
                   (forward-char))
          (setq prev-line-start cur-line-start)
          (setq prev-line-end cur-line-end)
          (forward-line))
        ;; return start of first line we did not process, which is guaranteed to
        ;; have no matching chars (so may be passed back to this function).  if
        ;; we processed all lines in buffer, this will be end of buffer (not
        ;; necessarily start of line)
        (point)))))

;; a safe line is a line without a space before a printing char


;; (defconst spaceship-safe-line-regexp "^\t*\\([^\s\t\n][^\n]*\\)?$")

;; return start of a safe line (or first line) before or including pos
(defun spaceship-find-safe-start (pos)
  "Returns start of a safe line (or first line) before or including POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (if (looking-at spaceship-safe-line-regexp)
        (point)
      (or (re-search-backward spaceship-safe-line-regexp nil t)
          (point-min)))))

;; return end of a safe line (or last line) after and *excluding* pos.  this one
;; is exclusive because the after-change-function needs to consider the
;; possibility that a line that looks safe now still needs to be re-processed
;; because it has old adjustments that were spoiled by changes in the previous
;; line
(defun spaceship-find-safe-end (pos)
  "Return end of a safe line (or last line) after and excluding POS."
  (save-excursion
    (goto-char pos)
    (forward-line)
    (or (re-search-forward spaceship-safe-line-regexp nil t)
        (point-max))))

(defun spaceship-do-region (start end)
  "Adjust width of all leading-space spaces and tabs in given region.
The region is between START and END in current buffer"
  (interactive "r")
  (spaceship-with-suitable-window
    (let* ((start (spaceship-find-safe-start start))
           (end (spaceship-find-safe-end end))
           (pos start))
      (spaceship-clear-region start end)
      (spaceship-do-tabs start end)
      (while (and pos (< pos end))
        (setq pos (spaceship-do pos))))))

(defun spaceship-do-buffer ()
  "Adjust width of all leading-space spaces and tabs in current buffer."
  (interactive)
  (spaceship-do-region (point-min) (point-max)))

(defun spaceship-clear-region (start end)
  "Remove all spaceship-mode properties"
  (spaceship-clear-region-properties
   start end 'spaceship-adjusted '(spaceship-adjusted spaceship-width display)))

(defun spaceship-clear-buffer ()
  (spaceship-clear-region (point-min) (point-max)))

;;; the next section of code is for the experimental ‘spaceship-auto-preserve’
;;; feature which is off by default.  it uses additional change-function hooks
;;; to try to preserve the alignment of blocks of code when editing a line would
;;; otherwise probably ruin it.

;;; in addition to their use here, the functions ‘spaceship-get-block’ and
;;; ‘spaceship-put-block’ can be used to create a copy-paste utility for
;;; codeblocks that respects indentation.  (I implemented this on my own system
;;; but consider it beyond the scope of what I'm trying to support with the
;;; public relase of spaceship-mode/tabble-mode.)

(cl-defstruct spaceship-block lines prefix length)
;; we only use the ‘length’ field when saving an implied block for the
;; auto-preserve feature below.  in fact, for general blocks the length is
;; ambiguous because of the last-line-empty exception

(defun spaceship-get-matching-string (str)
  "Return the string of spaces and tabs which which match STR under
spaceship-mode rules."
  (if (string-match (rx line-start
                        ;; match leading spaces/tabs exactly
                        (group (* (any "\s\t")))
                        ;; after the first printing char, disallow tabs and
                        ;; match all with spaces
                        (? (group (not (any "\s\t\n"))
                                  (* (not (any "\t\n"))))))
                    str)
      (concat (match-string 1 str)
              (make-string (length (match-string 2 str)) ?\s))
    ;; failure to match means there was an illegal tab (e.g. a tabble tab) so
    ;; matching is not possible
    nil))

(defun spaceship-get-block (start end)
  "Return the spaceship-block between START and END in current buffer.  If any
of the lines before END fails to match the previous lines correctly, the block
will end there."
  (cl-block func
    (let (prefix prefix-len lines)
      (save-excursion
        (goto-char start)
        (setq prefix (spaceship-get-matching-string
                      (buffer-substring-no-properties
                       (line-beginning-position) (point))))
        (unless prefix (cl-return-from func nil))
        (setq prefix-len (length prefix))
        (cl-block loop
          (while t
            (push (buffer-substring-no-properties
                   (point) (min end (line-end-position)))
                  lines)
            (when (<= end (line-end-position))
              (cl-return-from loop))
            (forward-line)
            ;; exception for ending with an empty line (don't check indentation)
            (when (= (point) end)
              (push "" lines)
              (cl-return-from loop))
            (when (< (point) end (+ (point) prefix-len))
              (cl-return-from func nil))
            (unless (equal (buffer-substring-no-properties
                            (point) (+ (point) prefix-len))
                           prefix)
              ;; one bad line invalidates the whole block
              (cl-return-from func nil))
            (forward-char prefix-len)))
        (make-spaceship-block :lines (nreverse lines)
                             :prefix prefix
                             :length nil)))))

;; the idea of an implied block is that if I were to insert/delete a char at
;; point, what other stuff should also move left/right to stay aligned to
;; whatever it was aligned to?  it's exactly the block of all lines that have a
;; char which is aligned to something on this line after point, with the block
;; prefix being whatever is necessary to match this line before point

;; the procedure basically looks for lines with a matching prefix, adding them
;; to the block; but it aborts if it sees that no alignment is happening because
;; the start is at end of line, continuation line has nothing after prefix, or
;; both prefix and continuation line have no actual alignment spaces

(defun spaceship-get-implied-block (start)
  "Return the implied spaceship-block starting at START in current buffer.  The
implied block is the longest legal block starting from a given position which is
non-trivial in the sense that the prefix is non-empty.  It ends only when the
next line no longer non-trivially matches under `spaceship-mode' rules."
  (cl-block func
    (let (lines prefix prefix-len prefix-has-space possible-end)
      (save-excursion
        (goto-char start)
        (when (= (point) (line-end-position))
          ;; no implied block because there's nothing to align to
          (cl-return-from func nil))
        (setq prefix (spaceship-get-matching-string
                             (buffer-substring-no-properties
                              (line-beginning-position) (point))))
        (unless prefix (cl-return-from func nil))
        (setq prefix-len (length prefix))
        (setq prefix-has-space (seq-contains-p prefix ?\s))
        (cl-block loop
          (while t
            (push (buffer-substring-no-properties
                   (point) (line-end-position))
                  lines)
            (setq possible-end (line-end-position))
            (forward-line)
            (unless (and (<= (+ (point) prefix-len) (point-max))
                         (equal (buffer-substring-no-properties
                                 (point) (+ (point) prefix-len))
                                prefix))
              ;; prefix doesn't match
              (cl-return-from loop))
            (forward-char prefix-len)
            (when (= (point) (line-end-position))
              ;; nothing after prefix to actually be aligned
              (cl-return-from loop))
            (unless (or prefix-has-space
                        (looking-at "\t*\s"))
              ;; no alignment is actually happening in this line
              (cl-return-from loop))))
        (make-spaceship-block :lines (nreverse lines)
                             :prefix prefix
                             :length (- possible-end start))))))

(defun spaceship-adjust-block (start end old-prefix new-prefix)
  "Assuming the text between START and END forms a legal spaceship-block in
current buffer with prefix OLD-PREFIX, edit the buffer to replace this block
with the corresponding block that has NEW-PREFIX instead of OLD-PREFIX but with
the contents otherwise unchanged."
  (save-excursion
    (setq end (copy-marker end t))
    (goto-char start)
    (beginning-of-line)
    (let ((old-prefix-len (length old-prefix)))
      (while (< (point) end)
        (unless (equal (buffer-substring-no-properties
                        (point) (+ (point) old-prefix-len))
                       old-prefix)
          (message "pt: %s  end: %s  str: %S" (point) end
                   (buffer-substring-no-properties
                    (point) (+ (point) old-prefix-len)))
          (error "spaceship-adjust-block: old text doesn't match"))
        (delete-region (point) (+ (point) old-prefix-len))
        (insert new-prefix)
        (forward-line)))))

(defun spaceship-put-block (blk pos)
  "Paste spaceship-block BLK at position POS in current buffer."
  (save-excursion
    (goto-char pos)
    (let ((new-prefix (spaceship-get-matching-string
                       (buffer-substring-no-properties
                        (line-beginning-position) (point))))
          (lines (spaceship-block-lines blk)))
      (insert (car lines)) ; first line
      (dolist (line (cdr lines))
        (insert "\n" new-prefix line)))))

(defvar spaceship-preserved-block nil
  "The spaceship-block saved for the purpose of having its alignment preserved.")

(defvar spaceship-saved-change-end nil
  "The end position of the spaceship-block trying to be preserved.")

(defun spaceship-try-preserve-block (start end len)
  "Attempt to restore the alignment of the saved block."
  (when (and (not undo-in-progress)
             spaceship-auto-preserve
             spaceship-preserved-block
             spaceship-saved-change-end
             (= spaceship-saved-change-end (+ start len)))
    (let* ((line-start (save-excursion (goto-char end)
                                       (line-beginning-position)))
           (new-prefix (spaceship-get-matching-string
                        (buffer-substring-no-properties
                         line-start
                         end)))
           (block-start (save-excursion (goto-char end)
                                        (line-beginning-position 2)))
           (block-end (+ end (spaceship-block-length spaceship-preserved-block)))
           (old-prefix (spaceship-block-prefix spaceship-preserved-block)))
      (spaceship-adjust-block block-start block-end old-prefix new-prefix)))
  (setq spaceship-preserved-block nil
        spaceship-saved-change-end nil))

(defmacro spaceship-with-reported-errors (name &rest body)
  "Execute BODY, trapping and reporting any errors which occur.
This is a debugging utility for `spaceship-mode' and
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

(defun spaceship-before-change-function (_start end)
  (save-match-data
    (when (and spaceship-auto-preserve
               (not undo-in-progress))
      (let ((blk (spaceship-get-implied-block end)))
        (if (and blk (cadr (spaceship-block-lines blk))) ; got nontrivial block
            (setq spaceship-preserved-block blk
                  spaceship-saved-change-end end)
          (setq spaceship-preserved-block nil))))))

(defun spaceship-after-change-function (start end len)
  (save-match-data
    (spaceship-try-preserve-block start end len)
    (spaceship-do-region start end)))

;;; the following two functions are intentionally simplistic UI fixes to make
;;; spaceship-mode easier to demo in stock emacs

(defun spaceship-simple-indent-line-function ()
  "Copy the previous line's indentation.
This is a simple function to use as value of
‘indent-line-function’ to prevent Emacs from messing up the
‘spaceship-mode’ conventions."
  (let (prev-start prev-end prev-indentation end)
    (save-excursion
      (beginning-of-line 0)
      (setq prev-start (point))
      (setq prev-end (re-search-forward "[ \t]*"))
      (setq prev-indentation (buffer-substring prev-start prev-end)))
    (back-to-indentation)
    (setq end (point))
    (beginning-of-line)
    (delete-region (point) end)
    (insert prev-indentation)))

(defun spaceship-in-indentation-p ()
  (save-match-data
    (let ((line-start (line-beginning-position)))
      (and (< line-start (point))
           (string-match "\\`[ \t]*\\'"
                         (buffer-substring-no-properties
                          line-start (point)))))))

(defun spaceship-delete-indentation-or-word ()
  (interactive)
  (if (spaceship-in-indentation-p)
      (delete-region (line-beginning-position) (point))
    (backward-kill-word 1)))

(provide 'spaceship-mode)
