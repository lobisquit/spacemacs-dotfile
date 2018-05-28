;; -*- mode: emacs-lisp -*-

;; default copy path is in the buffer open alongside current one
(setq dired-dwim-target t)

;; set dired details
(setq dired-listing-switches "-al")

;; add keybind to wdired
(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key
             (kbd "W")
             'wdired-change-to-wdired-mode)))

;; add all-the-icons in dired
(add-hook 'dired-mode-hook
          'all-the-icons-dired-mode)
