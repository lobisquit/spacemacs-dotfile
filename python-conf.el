;; -*- mode: emacs-lisp -*-

;; python tabs settings
(add-hook 'python-mode-hook
          (lambda ()
            ;; (setq indent-tabs-mode t)
            (setq python-indent-offset 4)))

;; isort on save of python file
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)
