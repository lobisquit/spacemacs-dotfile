(defun eshell-configs ()
  (require 'dash)
  (require 's)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                   (-> ,ICON
                      (concat esh-section-delim ,FORM)
                      (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  ;; Separator between esh-sections
  (setq esh-sep "  ")  ; or " | "

  ;; Separator between an esh-section icon and form
  (setq esh-section-delim " ")

  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.

  ;; Eshell prompt header (with defaule theme face)
  (setq esh-header (with-face "\n┌─ " '()))
  (setq eshell-prompt-string (with-face "└─→ " '()))
  (setq eshell-prompt-regexp eshell-prompt-string)

  (esh-section esh-dir
               "\xe2c7"  ;  (faicon folder)
               (concat (abbreviate-file-name (eshell/pwd)) "/")
               '(:foreground "#ff9900"))

  (esh-section esh-git
               "\xe907"  ;  (git icon)
               (magit-get-current-branch)
               '(:foreground "#ff3300"))

  ;; use this packet for python virtualenv detection
  (require 'virtualenvwrapper)
  (venv-initialize-eshell)

  ;; redefine workon function, to use current directory as default location
  (defun eshell/workon ()
    (let ((venv-location (eshell/pwd)))
      (venv-workon)))

  (esh-section esh-python
               "\xe928"  ;  (python icon)
               venv-current-name
               '(:foreground "#2eae85"))

  (esh-section esh-clock
               "\xf046"  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "#41a5ff"))

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

  ;; navigate as in shell mode
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<up>") 'previous-line)
              (define-key eshell-mode-map (kbd "<down>") 'next-line)
              (define-key eshell-mode-map (kbd "<C-return>") 'eshell-next-prompt)
              (define-key eshell-mode-map (kbd "<C-down>") 'eshell-next-input)
              (define-key eshell-mode-map (kbd "<C-up>") 'eshell-previous-input)
              ))
  )
