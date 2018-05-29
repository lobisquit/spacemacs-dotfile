;; -*- mode: emacs-lisp -*-

;; switch to other window (faster than C-x o)
(global-set-key (kbd "s-o") 'other-window)

;; do winner also when C is kept pressed
(global-set-key (kbd "C-c C-<left>") 'winner-undo)
(global-set-key (kbd "C-c C-<right>") 'winner-redo)

;; move at actual beginning of the line (after indentation)
(global-set-key (kbd "<home>") 'smart-line-beginning)

(defun smart-line-beginning ()
  "Jump to indentation or beginning of line if already there."
  (interactive "^")
  ;; handle eshell differently
  (if (bound-and-true-p eshell-mode)
      (eshell-bol)
    ;; normal behaviour
    (let ((current-point (point)))
      (beginning-of-line-text)
      (when (eq current-point (point))
        (beginning-of-line)))))
