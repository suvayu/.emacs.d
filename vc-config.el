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

(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key (kbd "C-x v d")
		(lambda ()
		  (interactive)
		  (magit-diff-unstaged)
		  (switch-to-buffer-other-window "*magit-diff*" t)))
(global-set-key (kbd "C-x v D")
		(lambda ()
		  (interactive)
		  (magit-diff-staged)
		  (switch-to-buffer-other-window "*magit-diff*" t)))
(global-set-key (kbd "C-x v l") 'magit-file-log)
(global-set-key (kbd "C-x v L")
		(lambda (&optional arg)
		  "With prefix `magit-log-long', `magit-log' w/o."
		  (interactive "P")
		  (if arg (magit-log-long)
		    (magit-log))))
;; A better binding might be
;; - file log: l - short, L - long (don't know how to get this)
;; - with prefix, repo log: same

(require 'magit-filenotify)
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(require 'magit-blame)
