(defun ess-configs ()
  ;; disable underscore as assign, use custom kbd
  (add-hook 'ess-mode-hook
            (lambda ()
              ;; remove underscore as assign symbol, put proper shortcut
              (local-set-key (kbd "C-.")  (lambda () (interactive) (insert " <- ")))
              (ess-toggle-underscore nil)
              (ess-toggle-S-assign nil)
              (ess-toggle-S-assign nil)
              )
  ))
