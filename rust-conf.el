;; -*- mode: emacs-lisp -*-

;; workaround to rust racer bad performace
(setq rust-match-angle-brackets nil)
(setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")

;; disable compile (similar to cargo build)
(add-hook 'rust-mode-hook
          (lambda ()
            (define-key global-map (kbd "M-RET c C") nil)))
