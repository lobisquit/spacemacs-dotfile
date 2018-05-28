;; -*- mode: emacs-lisp -*-

;; origami key bindings
(global-set-key (kbd "C-$") 'origami-recursively-toggle-node)

;; powerline settings
(setq powerline-default-separator 'arrow)

;; wrap lines at word boundaries, not in any char
(global-visual-line-mode t)

;; split windows always vertically
(setq split-height-threshold 50)
(setq split-width-threshold 70)

;; golden ratio mode: autosize windows to make current bigger
(require 'golden-ratio)
(golden-ratio-mode t)

;; disable for ivy occur windows
(add-to-list 'golden-ratio-exclude-buffer-regexp " *ivy-occur swiper \\.*")
(add-to-list 'golden-ratio-exclude-buffer-regexp "\\*gud-.*\\*")
(add-to-list 'golden-ratio-exclude-buffer-regexp "\\*xref\\*")
spacemacs.
