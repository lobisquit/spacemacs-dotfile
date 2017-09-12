(defun org-mode-configs ()
  ;; org mode
  (require 'org)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-log-done t)

  (defvar main-org-file)
  (setq main-org-file "~/Archivi/org/tasks.org")
  (setq org-agenda-files (list main-org-file))

  ;; fast kbd to open main org file
  (global-set-key (kbd "C-c o")
                  (lambda () (interactive) (find-file main-org-file)))
  )
