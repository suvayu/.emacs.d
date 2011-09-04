;;; minimal-cedet.el -- for minimal cedet (BZR) configuration

;;; Code:
(load-file "~/.emacs.d/minimal-init.el")

;;; Minimal setup to load latest `cedet'
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/cedet"))
(load-library "~/.emacs.d/lisp/cedet/common/cedet.elc")

;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

(setq semantic-load-turn-useful-things-on t)
(semantic-load-enable-gaudy-code-helpers)

;;; minimal-cedet.el ends here
