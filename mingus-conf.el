(defun mingus-configs ()
  ;; mingus startup (mpd controller)
  (global-set-key (kbd "<f5>") 'mingus)

  ;; nice Fn shortcuts
  (global-set-key (kbd "<XF86AudioPlay>") 'mingus-toggle)
  (global-set-key (kbd "<XF86AudioStop>") 'mingus-stop)
  (global-set-key (kbd "<XF86AudioPrev>") 'mingus-prev)
  (global-set-key (kbd "<XF86AudioNext>") 'mingus-next)

  ;; alternative save command
  (global-set-key (kbd "s-w") 'save-buffer)

  ;; disable overwrite mode when pressing Ins button
  (define-key global-map [(insert)] nil)
  )
