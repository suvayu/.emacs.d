;;; vc-config.el --- Version control configuration

;;; Commentary:

;;; Code:
;; ;; light weight additions and remapping to vc-git (ELPA)
;; (load-library "gitty")

;; auto-revert-mode for files under version control
(add-hook 'find-file-hook
	  (lambda ()
	    (condition-case nil
		(if (vc-working-revision (buffer-file-name))
		    (auto-revert-mode t))
	      (error nil))))

;; mode to edit git commit message, load after `orgalist'
(autoload 'git-commit-mode "git-commit"
  "Major mode for editing git commit messages." t)
(add-hook 'git-commit-mode-hook (lambda () (orgalist-mode t)))

(require 'magit)

(defun sa-magit-log (files &optional long)
    "My Magit log function.

FILES are passed on as is, when LONG show a more verbose git log."
    (interactive "P")
    (magit-log-current
     nil (if (not long) '("--graph")
	   '("--graph" "--format=medium" "--stat"))
     files))

(define-key magit-log-mode-map (kbd "TAB") 'magit-goto-next-section)
(define-key magit-log-mode-map (kbd "<backtab>") 'magit-goto-previous-section)

(global-set-key (kbd "C-x v s") 'magit-status)
(global-set-key (kbd "C-x v d") 'magit-diff-unstaged)
(global-set-key (kbd "C-x v D") 'magit-diff-staged)
(global-set-key (kbd "C-x v l")
                (lambda (&optional long)
                  (interactive "P")
                  (sa-magit-log nil long)))
(global-set-key (kbd "C-x v L")
                (lambda (&optional long)
                  (interactive "P")
                  (sa-magit-log (list (buffer-file-name)) long)))
;; A better binding might be
;; - file log: l - short, L - long (don't know how to get this)
;; - with prefix, repo log: same

;; (require 'magit-filenotify)
;; (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
(require 'magit-blame)
;;; vc-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
