(defun org-mode-configs ()
  ;; org mode
  (require 'org)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-log-done t)

  (setq org-agenda-files (list "~/Archivi/org/tasks.org"))
  )
