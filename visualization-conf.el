(defun visualization-configs ()
  ;; wrap lines at word boundaries, not in any char
  (global-visual-line-mode 1)

  ;; origami key bindings
  (global-set-key (kbd "C-$") 'origami-recursively-toggle-node)

  ;; split windows always vertically
  (setq split-height-threshold 70) ;; <- here nil keeps splitting
  (setq split-width-threshold 0)

  ;; wrap columns at 80 (if toggled by spacemacs/toggle-truncate-lines)
  (setq-default fill-column 80)

  ;; linum mode kbd
  (global-set-key (kbd "s-l") 'linum-mode)

  ;; whitespace configurations
  (require 'whitespace)
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           ;; empty
                           ;; space-mark
                           tab-mark
                           ))

  (setq whitespace-space-regexp "\\(^ +\\)")
  (global-whitespace-mode)

  ;; shortcut for whitespace options
  (global-set-key (kbd "s-o") 'whitespace-toggle-options)

  ;; no whitespace in line-numbering
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (let ((w (+ 1 (length (number-to-string (count-lines (point-min) (point-max)))))))
                (setq linum-format
                      `(lambda (line)
                         (propertize (concat
                                      (truncate-string-to-width
                                       "" (- ,w (length (number-to-string line)))
                                       nil ?\x2007)
                                      (number-to-string line))
                                     'face 'linum))))))
)
