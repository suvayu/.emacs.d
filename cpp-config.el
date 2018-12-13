;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on bhishma

;;; Code: C++ customisations

;; associate arduino sketches to c-mode
(add-to-list 'auto-mode-alist (cons "\\.ino\\'" 'c++-mode))

;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

;; flycheck
(add-hook 'c++-mode-hook
	  (lambda ()
	    ;; NOTE: *-language-standard: when set globally, breaks C mode
	    (setq flycheck-clang-language-standard "c++17"
		  flycheck-clang-include-path '("/usr/include/root")
		  flycheck-gcc-language-standard "c++17"
		  flycheck-gcc-include-path '("/usr/include/root")
		  )))

;; semantic
(setq root-include (let ((rootsys (getenv "ROOTSYS")))
		     (if rootsys (concat rootsys "/include")
		       "/usr/include/root")))
(semantic-add-system-include root-include 'c++-mode)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (flycheck-mode 1)
	    ;; for semantic imenu
	    (setq imenu-create-index-function 'semantic-create-imenu-index)
	    (imenu-add-to-menubar "C++-Tags")))

(eval-after-load "cc-mode"
  (progn
    (load-library "semantic/ia")
    (load-library "semantic/senator")))


;; TODO: reimplement `cedet-m3-menu-kbd' (cedet-m3.el) using popup.el


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


;; ;; C/C++ language templates
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (unless (featurep 'yasnippet) (load-library "yasnippet"))))

;; templates depend on mode
(add-hook 'c-mode-hook
	  (lambda ()
	    (yas-minor-mode-on)))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (yas-minor-mode-on)))


;;; Keybindings
;; rebind as c-electric overrides global binding
(add-hook 'c-mode-common-hook
	  (lambda ()
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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
