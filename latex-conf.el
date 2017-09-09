(defun latex-configurations ()
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

  ;; latex align tables, found here https://thenybble.de/projects/inhibit-auto-fill.html
  (defun LaTeX-collapse-table ()
    "Properly format table"
    (interactive)
    (save-excursion
      (LaTeX-mark-environment)
      (while (re-search-forward "[[:space:]]+\\(&\\|\\\\\\\\\\)" (region-end) t)
        (replace-match " \\1"))))

  (defun LaTeX-align-environment (arg)
    "Align fields in table"
    (interactive "P")
    (if arg (LaTeX-collapse-table)
      ;; use spaces for alignment, even if tabs are preferred
      (let ((indent-tabs-mode nil))
        (save-excursion
          (LaTeX-mark-environment)
          (align (region-beginning) (region-end))))))

  (add-hook 'LaTeX-mode-hook
            (lambda () (local-set-key (kbd "C-c f") 'LaTeX-align-environment)))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;; tabs settings
              (setq indent-tabs-mode t)
              (setq tab-width (default-value 'tab-width))

              ;; alternative latex build & view command kbd
              (local-set-key (kbd "s-e")
                             (lambda ()
                               (interactive)
                               (let ((TeX-save-query nil))
                                 (TeX-command-sequence t t))
                               ))
              ;; align tables shortcut
              (local-set-key (kbd "C-c f") 'LaTeX-align-environment)))

  ;; items are indented too
  (setq LaTeX-item-indent 0)

  ;; always ask for master file
  (setq TeX-master nil)

  ;; ;; LaTeX with lua inside
  ;; (setq mmm-global-mode 'maybe)
  ;; (setq mmm-submode-decoration-level 2)
  ;; (mmm-add-classes
  ;;  '((mmm-objc-latex
  ;;     :submode lua-mode
  ;;     :front "^\\\\startluacode\n"
  ;;     :back "^\\\\startluacode"
  ;;     )))
  )
