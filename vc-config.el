;; -*- mode: emacs-lisp; -*-
;;; vc-config.el

;; ;; ;; light weight additions and remapping to vc-git (ELPA)
;; (load-library "gitty")

;; auto-revert-mode for files under version control
(add-hook 'find-file-hook
	  (lambda ()
	    (if (vc-working-revision (buffer-file-name)) ; (egg-buf-git-name)
		(progn
		  (auto-revert-mode t)
		  ;; (eval-after-load 'gitty (gitty-mode 1))
		  ))
	    ))

;; FIXME: When last tested, git-commit-mode was unstable.  It would
;; crash Emacs with C-g in the terminal.  This makes it using
;; emacsclient unsuitable for git commits from the command line.

;; ;; mode to edit git commit message
;; (autoload 'git-commit-mode "git-commit"
;;   "Major mode for editing git commit messages." t)

;; (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))
;; (add-hook 'git-commit-mode-hook
;; 	  (lambda () (turn-on-orgstruct++)))
