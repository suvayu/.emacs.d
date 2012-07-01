;; -*- mode: emacs-lisp -*-
;;; minimal-commit.el
;; Minimal settings for making git commits

;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; load custom file
(setq custom-file "~jallad/.emacs.d/emacs-custom.el")
(load-file custom-file)

;; keybindings and text-mode settings
(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/text-mode-config.el")
