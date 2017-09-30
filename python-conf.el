(defun python-configs ()
  ;; python tabs settings
  (add-hook 'python-mode-hook
            (lambda ()
              (setq python-indent-offset 4)
              (setq indent-tabs-mode t)
              (setq tab-width (default-value 'tab-width))))

  ;; isort on save of python file
  (require 'py-isort)
  (add-hook 'before-save-hook 'py-isort-before-save)
)
