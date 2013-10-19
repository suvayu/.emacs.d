;; -*- mode: emacs-lisp; -*-
;;; vc-config.el

;; ;; light weight additions and remapping to vc-git (ELPA)
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

;; mode to edit git commit message
(autoload 'git-commit-mode "git-commit"
  "Major mode for editing git commit messages." t)
(add-hook 'git-commit-mode-hook (lambda () (orgstruct++-mode t)))

(require 'magit)
(define-key magit-log-mode-map (kbd "TAB") 'magit-goto-next-section)
(define-key magit-log-mode-map (kbd "<backtab>") 'magit-goto-previous-section)
