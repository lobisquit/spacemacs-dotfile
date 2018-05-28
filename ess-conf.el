;; -*- mode: emacs-lisp -*-

;; disable underscore as assign, use custom kbd
(add-hook 'ess-mode-hook
          (lambda ()
            ;; remove underscore as assign symbol, put unused semicolon
            (setq ess-smart-S-assign-key ";")

            (ess-toggle-underscore nil)
            (ess-toggle-S-assign nil)
            (ess-toggle-S-assign nil)))
