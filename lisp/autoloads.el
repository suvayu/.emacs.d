;; my autoloads

;; CEDET setup
(defun setup-cedet ()
  "Setup CEDET if it is already not loaded"
  (interactive)
  (unless (or (featurep 'cedet) 
	      (eq major-mode 'lisp-interaction-mode))
    (message "Loading CEDET settings.")
    ;; (when (string= (getenv "USER") "jallad")
    (load-file "~/.emacs.d/elisp/cedet-configs.el");)
    (if (or (eq major-mode 'emacs-lisp-mode)
	    (eq major-mode 'lisp-mode))
	(semantic-default-elisp-setup))))

