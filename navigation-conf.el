(defun navigation-configs ()
  ;; move at actual beginning of the line (past indentation), not in the head
  (global-set-key (kbd "C-a") 'back-to-indentation)

  ;; multiple cursors support
  (require 'multiple-cursors)
  (global-set-key (kbd "s-m") 'mc/edit-lines)
  (global-set-key (kbd "C-M-<mouse-1>") 'mc/add-cursor-on-click)

  (global-set-key (kbd "M-<up>") 'move-text-up)
  (global-set-key (kbd "M-<down>") 'move-text-down)
  )

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))
