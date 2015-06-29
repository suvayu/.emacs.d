;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on bhishma

;;; Code: C++ customisations

;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

;; flycheck
(setq flycheck-clang-args '("-std=c++11")
      flycheck-clang-include-path '("/usr/include/root")
      flycheck-gcc-args '("-std=c++11")
      flycheck-gcc-include-path '("/usr/include/root")
      )

;; semantic
(setq root-include (let ((rootsys (getenv "ROOTSYS")))
		     (if rootsys (concat rootsys "/include")
		       "/usr/include/root")))
(semantic-add-system-include root-include 'c++-mode)

;; disable semantic in all non C/C++ buffers
(add-to-list 'semantic-inhibit-functions
	     (lambda () (not (member major-mode '(c-mode c++-mode)))))

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
;; (require 'templates)		; skeletons and templates


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
	    ;; ;; keybindings for skeletons
	    ;; (define-key c-mode-base-map (kbd "C-c f") 'c++-for-skel)
	    ;; (define-key c-mode-base-map (kbd "C-c i") 'c++-if-skel))
	    ))
