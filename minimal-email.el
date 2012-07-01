;; -*- mode: emacs-lisp -*-
;;; minimal-email.el
;; Minimal settings for composing emails or messages from browsers

;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; load custom file
(setq custom-file "~jallad/.emacs.d/emacs-custom.el")
(load-file custom-file)

;; keybindings and other settings
(load-file "~/.emacs.d/gui-config.el")
(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/text-mode-config.el")
(load-file "~/.emacs.d/email-config.el")
