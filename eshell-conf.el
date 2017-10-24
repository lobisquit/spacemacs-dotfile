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

;; open specified file, defaulting to current directory
(defun eshell/open (&optional file)
  ;; test wheater optional argument is set or not
  (if (and file (not (consp file)))
      (find-file file)
    (find-file ".")))

(require 'cl)

(defun argument-set-p (arg)
  "Assert if function argument is set or not"
  (and arg (not (consp arg))))

(defun eshells-buffers ()
  "Retrieve all eshell buffers"
  (remove-if-not
   (lambda (buffer)
     (string-prefix-p "*eshell" (buffer-name buffer)))
   (buffer-list)))

(defun eshells-format (name)
  "Format name as a proper eshell buffer name"
  (cond ((stringp name)  (format "*eshell*<%s>" name))
        ((integerp name) (format "*eshell*<%d>" name))
        (t (error "Given name is nor a string not an integer"))))

(defun eshells-buffer-p (name)
  "Assert if an eshell buffer with given name exists"
  (member
   (eshells-format name)
   (mapcar (function buffer-name) (eshells-buffers))))

(defun eshells-get-new-name (&optional number)
  "Make up a new (exclusive) eshell buffer name"
  (let ((number (if (argument-set-p number) number 1)))
        (if (eshells-buffer-p number)
            (eshells-get-new-name (+ number 1))
          number)))

(defun eshells-get-new-named (&optional name)
  "Open new eshell buffer, optionally with name"
  (let* ((name (cond
                ;; if name is not provided, choose a default one
                ((not (argument-set-p name)) (eshells-get-new-name))
                ;; otherwise, continue
                (t name)))
         (eshell-buffer-name (eshells-format name)))

    ;; switch to specified buffer if it already exists
    (if (eshells-buffer-p name)
        (switch-to-buffer eshell-buffer-name))
    (eshell)))

(defun eshells-new ()
  "Open new eshell buffer in current buffer path, asking user a name for it"
  (interactive)
  (let ((candidate (read-string "Eshell buffer name (RET for default choice): ")))
    (if (string= candidate "")
        (eshells-get-new-named)
      (eshells-get-new-named candidate))))

(defun eshells-new-here ()
  "Open (possibly new) eshell in current directory path"
  (interactive)
  (let* (
         ;; note that buffer-file-name doesn't work with dired
         (file-name (or buffer-file-name default-directory))
         (path (if file-name (file-name-directory file-name)))
         ;; path of current eshells instances
         (eshells-paths (mapcar
                         (lambda (buffer) (with-current-buffer buffer (eshell/pwd)))
                         (eshells-buffers)))
         )
    (cond
     ((not file-name) (error "Current buffer is not related to any file"))
     ((member path eshells-paths) (switch-to-buffer path))
     ;; note that new eshells open automatically in current buffer path
     (t (eshells-get-new-named (file-name-base (directory-file-name path)))))))

(defun eshells-choose ()
  "Choose among available eshell buffers"
  (interactive)
  (let* (
         ;; get max width of buffer names, to align the second field
         (max-width (apply 'max
                           (mapcar (lambda (buffer) (length (buffer-name buffer)))
                                   (eshells-buffers))))

         ;; return a list of entries (prompt string, buffer name)
         (choices (mapcar (lambda (buffer)
                            (let ((name (buffer-name buffer))
                                  (path (concat (with-current-buffer buffer
                                                  (abbreviate-file-name (eshell/pwd)))
                                                "/")))
                              (list
                               ;; align prompt string elements
                               (format (format "%%-%ds%%s" (+ 4 max-width)) name path)
                               name)))
                          (eshells-buffers)))

         ;; make user choose a prompt string
         (choice (completing-read
                 "Select eshell buffer: "
                 (mapcar (function car) choices)))

         ;; retrieve first (and only) matching choices entry
         (chosen-buffer-name (nth 1 (car (remove-if-not (lambda (entry) (string= choice (car entry)))
                                                        choices))))
         )
    (switch-to-buffer chosen-buffer-name)))

(defun eshells-configs ()
  ;; unset (unused since there is End button) C-e kbd
  (global-unset-key (kbd "C-e"))

  ;; eshells kbds
  (global-set-key (kbd "C-e C-n") 'eshells-new)
  (global-set-key (kbd "C-e C-c") 'eshells-choose)
  (global-set-key (kbd "C-e C-h") 'eshells-new-here)
  )
