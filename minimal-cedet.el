;;; minimal-cedet.el -- for minimal cedet (BZR) configuration

;;; Code:
(load-file "~/.emacs.d/minimal-init.el")

;;; Minimal setup to load latest `cedet'
(load-file "~/.emacs.d/lisp/cedet/cedet-devel-load.el")

;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; Enable Semantic
(semantic-mode 1)

;;; minimal-cedet.el ends here
