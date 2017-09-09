(defun whitespace-configurations ()
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
