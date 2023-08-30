;; elastic-tools  --- tools for variable-pitch fonts. -*- lexical-binding: t; -*-
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

;;; This file provides tools for elastic-indent and elastic-table

(require 'dash)
(require 'cl-lib)

;;; Code:

(defgroup elastic nil "Customization of elastic spacing (helps alignment in presence of proportional fonts)."
  :group 'programming :group 'convenience)

(defvar-local elastic-tools-handlers nil
  "Alist mapping handler function symbols to a depth.")

(defvar-local elastic-tools-deleted-newline nil
  "Have we just deleted a newline character?")

(defun elastic-tools-add-handler (handler depth)
  "Register HANDLER at given DEPTH.
Lower DEPTH means executed first."
  (setf (alist-get handler elastic-tools-handlers) depth)
  (setq elastic-tools-handlers (-sort (-on #'< #'cdr) elastic-tools-handlers))
    (add-hook 'text-scale-mode-hook #'elastic-tools-queue-buffer t)
    (add-hook 'post-command-hook #'elastic-tools-handle-queue nil t)
    (add-hook 'before-change-functions #'elastic-tools-before-change-function nil t)
    (add-hook 'after-change-functions #'elastic-tools-after-change-function nil t)
    ;; Queue handling the buffer so it's taken care of by the new
    ;; handler.  Note that when activating the modes (e.g. upon buffer
    ;; creation, with this method, the buffer will be handled just
    ;; once, and zero times if the buffer is not active at the end of
    ;; the command. For instance, org-babel native fontification will
    ;; create a temporary buffer that is never active, and therefore
    ;; will not be handled. This is a good thing, because org-babel
    ;; creates such a buffer at each keystroke, and handling it all
    ;; every time is very slow.
    (elastic-tools-queue-buffer))

(defun elastic-tools-remove-handler (handler)
  "Unregister HANDLER."
  (setq elastic-tools-handlers (assq-delete-all handler elastic-tools-handlers))
  (unless elastic-tools-handlers
    (remove-hook 'text-scale-mode-hook #'elastic-tools-queue-buffer)
    (remove-hook 'post-command-hook #'elastic-tools-handle-queue)
    (remove-hook 'before-change-functions #'elastic-tools-before-change-function t)
    (remove-hook 'after-change-functions #'elastic-tools-after-change-function t)))

(defmacro elastic-tools-with-suitable-window (&rest body)
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

(defun elastic-tools-before-change-function (start end)
  "Queue a call to `elastic-tools-do-region' for START and END."
  (setq elastic-tools-deleted-newline
        (save-excursion
          (goto-char start)
          (and (search-forward "\n" end t)
               t))) ; forget the actual position (for tidyness)
  ;; (message "etbcf: %s %s-%s" elastic-tools-deleted-newline start end)
  )

(defvar-local elastic-tools-queue nil
  "Queue of changes to handle.
We need queueing because some commands (for instance
`fill-paragraph') will cause many changes, which may individually
propagate down the buffer.  Doing all this work many times can
cause visible slowdowns.")

(defun elastic-tools-push (task hook)
  "Push TASK in the queue for HOOK."
  (push task (alist-get hook elastic-tools-queue)))

(defun elastic-tools-after-change-function (start end _len)
  "Queue a change between START and END to be handled by `elastic-tools-handlers'."
  ;; (message "etacf: %s %s-%s" elastic-tools-deleted-newline start end)
  (elastic-tools-push (list elastic-tools-deleted-newline (copy-marker start) (copy-marker end t))
                 (car (car elastic-tools-handlers))))

(defmacro elastic-tools-with-context (&rest body)
  "Run BODY in a context which is suitable for applying our adjustments."
  (declare (indent 0))
  `(save-match-data ; just in case
     (elastic-tools-with-suitable-window ; we need a window to compute the character widths.
      (save-excursion
        (without-restriction ; because changes may propagate beyond the restriction.
          (with-silent-modifications
            ,@body))))))


(defun elastic-tools-do-buffer ()
  "Call each `elastic-tools-handlers' on the whole buffer."
  (interactive)
  (elastic-tools-with-context
    (dolist (hook (-map #'car elastic-tools-handlers))
      (funcall hook t (point-min) (point-max)))))


(defun elastic-tools-queue-buffer ()
  "Queue handling the whole buffer."
  (elastic-tools-after-change-function (point-min) (point-max) nil))

(defun elastic-tools-handle-queue ()
  "Take care of intervals in queue.
If input comes before the work can be finished, then stop and
continue the work later, when idle."
  (let ((hooks (-map #'car elastic-tools-handlers)))
    (while hooks
      (let ((hook (pop hooks)))
        ;; (message "elastic-tools: dealing with %s q=%s" hook (alist-get hook elastic-tools-queue))
        (setf (alist-get hook elastic-tools-queue)
              (--sort (or (< (cadr it) (cadr other)) (car it)) (alist-get hook elastic-tools-queue)))
        (elastic-tools-with-context
          (goto-char (point-min))
          (while-no-input ;  see post-command-hook documentation.
            (while (alist-get hook elastic-tools-queue)
              (pcase (car (alist-get hook elastic-tools-queue))
                (`(,force-propagate ,start ,end)
                 (when (> end (point)) ; otherwise the change has already been taken care of.
                   (let ((actual-start (max (point) start))) ; portion before point was already done.
                     (funcall hook force-propagate actual-start end)
                     ;; (message "elastic-tools: running %s %s %s %s" hook force-propagate actual-start end)
                     (when hooks
                       ;; next layer needs to deal with the change.
                       (elastic-tools-push (list t actual-start (copy-marker (max end (point))))
                                      (car hooks)))))))
              ;; pop only when we're done so we don't forget something
              (pop (alist-get hook elastic-tools-queue))))
          (when (alist-get hook elastic-tools-queue)
            ;; input came: we continue later.
            (setq hooks nil) ; stop the outer loop
            (run-with-idle-timer 0.2 nil #'elastic-tools-handle-queue)))))))

(defun elastic-tools-clear-region-properties (start end cue-prop props-to-remove)
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


(defun elastic-tools-on-col-2 (pos)
  "Is POS on 2nd column?"
  (save-excursion
    (goto-char pos)
    (and (not (bolp))
         (progn (forward-char -1)
                (bolp)))))

(defun elastic-tools-text-pixel-width (start end)
  "Return the pixel width of text between START and END in current buffer."
  (let ((c (car (window-text-pixel-size nil start end))))
    (if (> c 2) c ;; Emacs bug: sometimes the returned window-text-pixel-size is negative. In this case computing it indirectly like below seems to fix the issue.
      (- (car (window-text-pixel-size nil (1- start) end))
         (car (window-text-pixel-size nil (1- start) start))))))

(defun elastic-tools-char-pixel-width (pos)
  "Return the pixel width of char at POS."
  (if-let (p (get-text-property pos 'elastindent-width))
      p
    ;; Emacs bug: sometimes the returned window-text-pixel-size is
    ;; wrong. In this case computing it indirectly like below
    ;; seems to fix the issue.
    (let ((c (car (window-text-pixel-size nil pos (1+ pos)))))
      (if (or (<= c 1) ; suspicious
              (elastic-tools-on-col-2 pos)) ; emacs is often wrong on that column, for some reason.
          (- (car (window-text-pixel-size nil (1- pos) (1+ pos)))
             (car (window-text-pixel-size nil (1- pos) pos)))
        c))))


(defun elastic-tools-show-char-pixel-width (pos)
  "Display pixel width of region char at POS.
This is a debug utility for `elastindent-mode'"
  (interactive "d")
  (message "pixel width of region: %s, raw=%s, disp=%s, prop=%s"
           (elastic-tools-char-pixel-width pos)
           (car (window-text-pixel-size nil pos (1+ pos)))
           (get-char-property pos 'elastindent-width)
           (get-char-property pos 'display)))


(provide 'elastic-tools)
;;; elastic-tools.el ends here
