;; -*- mode: emacs-lisp -*-

;; custom toggle comment
(global-set-key (kbd "M-\\") 'comment-line)

;; custom delete word, instead of killing
(global-set-key (kbd "<C-backspace>") 'delete-word)
(global-set-key (kbd "<C-delete>")    'delete-word)
(global-set-key (kbd "M-DEL")         'delete-word)

(defun delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
   With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; custom kill, copy
(global-set-key (kbd "C-w") 'kill-line-or-region)

(defun kill-line-or-region ()
  "Kill region if active only or kill line (smartly)"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'smart-kill-whole-line)))

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key (kbd "M-w") 'copy-line-or-region)

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun copy-line-or-region ()
  "Copy region if active only or copy whole line"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (call-interactively 'copy-line)))
