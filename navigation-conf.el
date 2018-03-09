(defun navigation-configs ()
  ;; move at actual beginning of the line (past indentation), not in the head

  ;; (global-unset-key (kbd "<home>"))
  ;; (global-set-key (kbd "<home>") 'smart-line-beginning)

  ;; multiple cursors support

  ;; move enabled / disabled commands file in custom place
  ;; NOTE must be set *before require*
  (setq mc/list-file "~/.spacemacs.d/mc-commands.el")

  (require 'multiple-cursors)
  (global-set-key (kbd "C-M-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "s-m") 'mc/mark-next-like-this)
  (define-key mc/keymap (kbd "<return>") nil)

  (global-set-key (kbd "M-<up>") 'move-text-up)
  (global-set-key (kbd "M-<down>") 'move-text-down)

  (global-set-key (kbd "C-w") 'kill-line-or-region)
  (global-set-key (kbd "M-w") 'copy-line-or-region)

  (global-set-key [C-f1] 'kill-file-name)

  ;; treemacs toggle
  (global-set-key (kbd "s-t") 'treemacs-toggle)

  ;; switch to other window (faster than C-x o)
  (global-set-key (kbd "s-o") 'other-window)

  ;; custom bindings for <menu> key
  (define-prefix-command 'movement)
  (define-key movement (kbd "<up>") 'beginning-of-buffer)
  (define-key movement (kbd "<down>") 'end-of-buffer)
  (define-key movement (kbd "<right>") 'move-end-of-line)
  (define-key movement (kbd "<left>") 'move-beginning-of-line)

  ;; make the menu key as leader key of movement section
  (global-set-key (kbd "<menu>") 'movement)

  ;; custom delete word, instead of killing
  (global-set-key (kbd "<C-backspace>") 'delete-word)
  (global-set-key (kbd "<C-delete>") 'delete-word)
  )

(defun delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun smart-line-beginning ()
  "Jump to indentation or beginning of line if already there."
  (interactive)
  (let ((current-point (point)))
    (beginning-of-line-text)
    (when (eq current-point (point))
      (beginning-of-line))))

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
  (move-text-internal arg)
  (indent-according-to-mode))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg))
  (indent-according-to-mode))

;; kill modification

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun kill-line-or-region ()
  "Kill region if active only or kill line (smartly)"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'smart-kill-whole-line)))

;; copy modification

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

;; get file names easily

(defun kill-file-name ()
  "Get file name as last element in kill ring, i.e. copy file name to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message (concat "\"" file-name "\" copied to clipboard"))
          (kill-new file-name))
      (message "Current buffer is not related to any file"))
    ))
