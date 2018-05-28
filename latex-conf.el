;; -*- mode: emacs-lisp -*-

;; disable auto fill
(remove-hook 'LaTeX-mode-hook 'latex/auto-fill-mode)

;; latex normal text (no big titles, neither formulas trick)
(setq font-latex-fontify-sectioning 'color)
(setq font-latex-fontify-script nil)

;; make printer mode the default one when opening a pdf
(add-hook 'pdf-view-mode-hook 'pdf-view-printer-minor-mode)

;; use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; update PDF after build
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
          #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; tabs settings
            (setq tab-width (default-value 'tab-width))

            ;; items are indented too
            (setq LaTeX-item-indent 0)

            ;; alternative latex build & view command kbd
            (local-set-key
             (kbd "s-e")
             (lambda ()
               (interactive)
               (let ((TeX-save-query nil)) (TeX-command-sequence t t))))))

;; always ask for master file
(setq TeX-master nil)
