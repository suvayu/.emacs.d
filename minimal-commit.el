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

;; FIXME: Temporarily until stability of git-commit-mode is tested again

;; mode to edit git commit message
(autoload 'git-commit-mode "git-commit"
  "Major mode for editing git commit messages." t)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))
(add-hook 'git-commit-mode-hook
	  (lambda () (turn-on-orgstruct++)))
