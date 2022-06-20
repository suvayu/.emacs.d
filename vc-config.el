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

;; mode to edit git commit message
(use-package git-commit
  :ensure t
  :after orgalist
  :hook (git-commit-mode . (lambda () (orgalist-mode t))))

(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0") ;seen auto-revert msg
  (defun sa-magit-log (files &optional long)
    "My Magit log function.

FILES are passed on as is, when LONG show a more verbose git log."
    (interactive "P")
    (magit-log-current
     nil (if (not long) '("--graph")
	   '("--graph" "--format=medium" "--stat"))
     files))
  :bind
  (("C-x v s" . magit-status)
   ("C-x v d" . magit-diff-unstaged)
   ("C-x v D" . magit-diff-staged)
   ;; A better binding might be
   ;; - file log: l - short, L - long (don't know how to get this)
   ;; - with prefix, repo log: same
   ("C-x v l" . (lambda (&optional long)
		  (interactive "P")
		  (sa-magit-log nil long)))
   ("C-x v L" . (lambda (&optional long)
		  (interactive "P")
		  (sa-magit-log (list (buffer-file-name)) long)))
   :map magit-log-mode-map		;FIXME: review
   ("<tab>" . magit-goto-next-section)
   ("<backtab>" . magit-goto-previous-section)
   )
  )

(use-package magit-blame :after (magit))

;; (use-package magit-filenotify
;;   :after (magit)
;;   :hook (magit-status-mode . magit-filenotify-mode)
;;   )

;;; vc-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
