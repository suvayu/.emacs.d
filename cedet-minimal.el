;;; Minimal CEDET configuration

;;; Code
(load-file "minimal-init.el")

;; CEDET load
(unless (or (featurep 'cedet)
	      (eq major-mode 'lisp-interaction-mode))
    (message "Loading CEDET settings.")
    (load-file "~/.emacs.d/lisp/cedet/common/cedet.elc")
    ;; (load-file "~/.emacs.d/lisp/cedet-configs.el")
    ;; setup CEDET for lisp
    (if (or (eq major-mode 'emacs-lisp-mode)
	    (eq major-mode 'lisp-mode))
	(semantic-default-elisp-setup))
    ;; setup CEDET for C/C++
    (if (or (eq major-mode 'c-mode)
	    (eq major-mode 'c++-mode))
	(semantic-default-c-setup))
    )
