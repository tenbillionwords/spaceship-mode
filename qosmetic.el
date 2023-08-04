(require 'dash)
(require 'cl-lib)

(defvar-local qosmetic-handlers nil) ; alist mapping function symbols to a depth
(defvar-local qosmetic-deleted-newline nil)

(defun qosmetic-add-handler (function depth)
  (setf (alist-get function qosmetic-handlers) depth)
  (setq qosmetic-handlers (-sort (-on #'< #'cdr) qosmetic-handlers))
  (add-hook 'post-command-hook 'qosmetic-handle-queue nil t)
  (add-hook 'before-change-functions 'qosmetic-before-change-function nil t)
  (add-hook 'after-change-functions 'qosmetic-after-change-function nil t))

(defun qosmetic-remove-handler (function)
  (setq qosmetic-handlers (assq-delete-all function qosmetic-handlers))
  (unless qosmetic-handlers
    (remove-hook 'post-command-hook 'qosmetic-handle-queue)
    (remove-hook 'before-change-functions 'qosmetic-before-change-function t)
    (remove-hook 'after-change-functions 'qosmetic-after-change-function t)))

(defmacro qosmetic-with-suitable-window (&rest body)
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

(defun qosmetic-before-change-function (start end)
  "Queue a call to `qosmetic-do-region' for START and END."
  (setq qosmetic-deleted-newline
        (save-excursion
          (goto-char start)
          (and (search-forward "\n" end t)
               t)))) ; forget the actual position for cleanup

(defvar-local qosmetic-queue nil
  "Queue of changes to handle.
We need queueing because some commands (for instance
`fill-paragraph') will cause many changes, which may individually
propagate down the buffer.  Doing all this work many times can
cause visible slowdowns.")

(defun qosmetic-push (task hook)
  "Push TASK in the queue for HOOK."
  (push task (alist-get hook qosmetic-queue)))

(defun qosmetic-after-change-function (start end _len)
  "Queue a change between START and END to be handled by `qosmetic-handlers'."
  (qosmetic-push (list qosmetic-deleted-newline (copy-marker start) (copy-marker end t))
                 (car (car qosmetic-handlers))))

(defmacro qosmetic-with-context (&rest body)
  "Run BODY in a context which is suitable for applying our adjustments."
  (declare (indent 0))
  `(save-match-data ; just in case
     (qosmetic-with-suitable-window ; we need a window to compute the character widths.
      (save-excursion
        (without-restriction ; because changes may propagate beyond the restriction.
          (with-silent-modifications
            ,@body))))))

(defun qosmetic-handle-queue ()
  "Take care of intervals in queue.
If input comes before the work can be finished, then stop and
continue the work later, when idle."
  (let ((hooks (-map #'car qosmetic-handlers)))
    (while hooks
      (let ((hook (pop hooks)))
        ;; (message "qosmetic: dealing with %s q=%s" hook (alist-get hook qosmetic-queue))
        (setf (alist-get hook qosmetic-queue)
              (-sort (-on #'< #'cadr) (alist-get hook qosmetic-queue)))
        (qosmetic-with-context
          (goto-char (point-min))
          (while-no-input ;  see post-command-hook documentation.
            (while (alist-get hook qosmetic-queue)
              (pcase (car (alist-get hook qosmetic-queue))
                (`(,force-propagate ,start ,end)
                 (when (> end (point)) ; otherwise the change has already been taken care of.
                   (let ((actual-start (max (point) start))) ; portion before point was already done.
                     (funcall hook force-propagate actual-start end)
                     ;; (message "qosmetic: running %s %s %s %s" hook force-propagate actual-start end)
                     (when hooks
                       ;; next layer needs to deal with the change.
                       (qosmetic-push (list t actual-start (copy-marker (max end (point))))
                                      (car hooks)))))))
              ;; pop only when we're done so we don't forget something
              (pop (alist-get hook qosmetic-queue))))
          (when (alist-get hook qosmetic-queue)
            ;; input came: we continue later.
            (setq hooks nil) ; stop the outer loop
            (run-with-idle-timer 0.2 nil #'qosmetic-handle-queue)))))))

(defun qosmetic-clear-region-properties (start end cue-prop props-to-remove)
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


(defun qosmetic-on-col-2 (pos)
  "Is POS on 2nd column?"
  (save-excursion
    (goto-char pos)
    (and (not (bolp))
         (progn (forward-char -1)
                (bolp)))))

(defun qosmetic-text-pixel-width (start end)
  "Return the pixel width of text between START and END in current buffer."
  (let ((c (car (window-text-pixel-size nil start end))))
    (if (> c 2) c ;; Emacs bug: sometimes the returned window-text-pixel-size is negative. In this case computing it indirectly like below seems to fix the issue.
      (- (car (window-text-pixel-size nil (1- start) end))
         (car (window-text-pixel-size nil (1- start) start))))))

(defun qosmetic-char-pixel-width (pos)
  "Return the pixel width of char at POS."
  (if-let (p (get-text-property pos 'elastindent-width))
      p
    ;; Emacs bug: sometimes the returned window-text-pixel-size is
    ;; wrong. In this case computing it indirectly like below
    ;; seems to fix the issue.
    (let ((c (car (window-text-pixel-size nil pos (1+ pos)))))
      (if (or (<= c 1) ; suspicious
              (qosmetic-on-col-2 pos)) ; emacs is often wrong on that column, for some reason.
          (- (car (window-text-pixel-size nil (1- pos) (1+ pos)))
             (car (window-text-pixel-size nil (1- pos) pos)))
        c))))


(defun qosmetic-show-char-pixel-width (pos)
  "Display pixel width of region char at POS.
This is a debug utility for `elastindent-mode'"
  (interactive "d")
  (message "pixel width of region: %s, raw=%s, disp=%s, prop=%s"
           (qosmetic-char-pixel-width pos)
           (car (window-text-pixel-size nil pos (1+ pos)))
           (get-char-property pos 'elastindent-width)
           (get-char-property pos 'display)))


(provide 'qosmetic)
;;; qosmetic.el ends here
