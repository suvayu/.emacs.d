;; my autoloads

;; CEDET setup
(defun setup-cedet ()
  "Setup CEDET if it is already not loaded"
  (interactive)
  (unless (or (featurep 'cedet) 
	      (eq major-mode 'lisp-interaction-mode))
    (message "Loading CEDET settings.")
    (load-file "~/.emacs.d/lisp/cedet-configs.el")
    ;; setup CEDET for lisp
    (if (or (eq major-mode 'emacs-lisp-mode)
	    (eq major-mode 'lisp-mode))
	(semantic-default-elisp-setup))
    ;; setup CEDET for C/C++
    (if (or (eq major-mode 'c-mode)
	    (eq major-mode 'c++-mode))
	(semantic-default-c-setup))
    ))
