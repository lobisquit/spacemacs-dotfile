(defun matlab-configs ()
  ;; disable splash in matlab command
  (setq matlab-shell-command-switches '("-nodesktop" "-nosplash"))

  ;; disable underscore as assign, use custom kbd
  (add-hook 'matlab-mode-hook
            (lambda ()
              (local-set-key (kbd "<C-return>") 'matlab-shell-run-region-or-line)

              (local-set-key (kbd "M-s")
                             (lambda ()
                               (interactive)
                               (if (matlab-shell-active-p)
                                   (matlab-show-matlab-shell-buffer)
                                 (matlab-shell))
                               ))

              (setq matlab-return-add-semicolon t)

              ;; enable basic autocompletion
              (company-mode t)
              )))
