(defun org-mode-configs ()
  ;; org mode
  (require 'org)

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)

  (setq org-time-stamp-rounding-minutes '(0 15))
  (setq org-log-done t)

  (defvar main-org-file)
  (defvar main-org-file)
  (setq org-directory "~/Archivi/org/")
  (setq main-org-file (concat org-directory "tasks.org"))

  (setq org-agenda-files (list main-org-file))

  ;; fast kbd to open main org file
  (global-set-key (kbd "C-c o")
                  (lambda () (interactive) (find-file main-org-file)))

  ;; (setq notes-org-file (concat org-directory "notes.org"))

  ;; ;; set default notes file
  ;; (setq org-default-notes-file notes-org-file)
  ;; (global-set-key (kbd "C-c n")
  ;;                 (lambda () (interactive) (find-file notes-org-file)))

  (require 'org-projectile)
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "notes.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))

  (global-set-key (kbd "C-c c") 'org-projectile-goto-location-for-project)
  (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)
  )
