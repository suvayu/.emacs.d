;; -*- mode: emacs-lisp -*-
;;; Code: C/C++ customisations
;; associate arduino sketches to c-mode
(add-to-list 'auto-mode-alist (cons "\\.ino\\'" 'c-mode))

;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))
;; and CUDA source & header files to c++-mode
(add-to-list 'auto-mode-alist (cons "\\.cuh?\\'" 'c++-mode))

;; flycheck
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook
	  (lambda ()
	    ;; NOTE: *-language-standard: when set globally, breaks C mode
	    (setq flycheck-clang-language-standard "c++17"
		  flycheck-gcc-language-standard "c++17"
		  )))

;; format on save
(require 'clang-format-lite)
(add-hook 'c++-mode-hook #'clang-format-lite-save-hook)
(add-hook 'c-mode-hook #'clang-format-lite-save-hook)

;;; Keybindings
;; TODO: isearch through symbol names

;; rebind as c-electric overrides global binding
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (flycheck-mode 1)
	    ;; want auto indentation more than parenthesis pairing
	    ;; however, is there a way to have my cake and eat it too?
	    (local-set-key "(" 'skeleton-pair-insert-maybe)
	    (local-set-key "{" 'skeleton-pair-insert-maybe)
	    (local-set-key "[" 'skeleton-pair-insert-maybe)
	    (local-set-key (kbd "M-RET") 'newline-and-indent)
	    ;; (c-toggle-auto-newline 1)
	    (yas-minor-mode 1)
	    ))

(require 'dap-lldb)

;;; cpp-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
