;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on bhishma

;;; Code: C++ customisations

(use-package c-mode
  :mode ("\\.ino\\'" . c-mode)		;arduino sketches to c-mode
  :hook
  (c-mode-common . (lambda ()
		     (flycheck-mode 1)
		     ;; for semantic imenu
		     (setq imenu-create-index-function 'semantic-create-imenu-index)
		     (imenu-add-to-menubar "C++-Tags")
		     ;; rebind as c-electric overrides global binding
		     (define-key c-mode-base-map (kbd "C-c d") 'semantic-ia-show-doc)
		     (define-key c-mode-base-map (kbd "C-c s") 'semantic-ia-show-summary)
		     ;; want auto indentation more than parenthesis pairing
		     ;; however, is there a way to have my cake and eat it too?
		     (local-set-key "(" 'skeleton-pair-insert-maybe)
		     (local-set-key "{" 'skeleton-pair-insert-maybe)
		     (local-set-key "[" 'skeleton-pair-insert-maybe)
		     (local-set-key (kbd "M-RET") 'newline-and-indent)
		     ;; (c-toggle-auto-newline 1)
		     ))
  )

(use-package c++-mode
  :mode
  (("\\.h\\'" . c++-mode)		;force `c++-mode' for `*.h' header files
   ("\\.cuh?\\'" . c++-mode)		;CUDA source & header files
   )
  :config
  ;; (setq root-include (let ((rootsys (getenv "ROOTSYS")))
  ;; 		       (if rootsys (concat rootsys "/include")
  ;; 			 "/usr/include/root")))
  ;; (setq other-includes (list "/opt/data-an/include"))
  ;; (dolist (dir (cons root-include other-includes))
  ;;   (semantic-add-system-include dir 'c++-mode))
  :hook
  ((c++-mode . (lambda ()
		 (lsp-deferred)
		 ;; NOTE: *-language-standard: when set globally, breaks C mode
		 (setq flycheck-clang-language-standard "c++17"
		       ;; flycheck-clang-include-path (cons root-include other-includes)
		       flycheck-gcc-language-standard "c++17"
		       ;; flycheck-gcc-include-path (cons root-include other-includes)
		       )))
   )
  )

(use-package cc-mode
  :config
  (progn
    (load-library "semantic/ia")
    (load-library "semantic/senator")))

(require 'dap-lldb)

;; FIXME: Doesn't quite do semantic isearch, could be bug in Emacs

;; Fool around with these:
;; defuns: senator-isearch-search-fun, senator-isearch-mode-hook
;; hooks:
;; (add-hook 'isearch-mode-hook     'senator-isearch-mode-hook)
;; (add-hook 'isearch-mode-end-hook 'senator-isearch-mode-hook)

(defun sa-senator-search-forward(string &optional bound noerror count)
  "This is convenience wrapper for `senator-search-forward'."
  (interactive "sSemantic search: ")
  (unless (featurep 'semantic/senator)
    (load-library "semantic/senator"))
  (let* ((senator-isearch-semantic-mode t))
    (senator-search-forward string bound noerror count)))

;;; cpp-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
