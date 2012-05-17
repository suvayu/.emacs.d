;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on bhishma

;;; Code:

;; C++ customisations
;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs pimped up! (development tools)			   ;;
;; Collection of Emacs Development Environment Tools       ;;
;; Skeletons, Templates, Abbreviations and Keyboard Macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Semantic built into Emacs
;; (semantic-mode 1)
(semantic-add-system-include "/usr/include/root/" 'c++-mode)

;; disable semantic in all non C/C++ buffers
(add-to-list 'semantic-inhibit-functions
	     (lambda () (not (member major-mode '(c-mode c++-mode)))))

(define-key c-mode-base-map (kbd "C-c ?") 'semantic-ia-complete-symbol)
(define-key c-mode-base-map (kbd "C-c t") 'semantic-ia-complete-tip)
(define-key c-mode-base-map (kbd "C-c v") 'semantic-ia-show-variants)
(define-key c-mode-base-map (kbd "C-c d") 'semantic-ia-show-doc)
(define-key c-mode-base-map (kbd "C-c s") 'semantic-ia-show-summary)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; for semantic imenu
	    (setq imenu-create-index-function 'semantic-create-imenu-index)
	    (imenu-add-to-menubar "C++-Tags")))

;; FIXME: Doesn't quite do semantic isearch, could be bug in Emacs
(defun sa-senator-search-forward(string &optional bound noerror count)
  "This is convenience wrapper for `senator-search-forward'."
  (interactive "sSemantic search: ")
  (let* ((senator-isearch-semantic-mode t))
    (senator-search-forward string bound noerror count)))

;; ;; Load CEDET (do not move later, conflicts with eieio bundled with Emacs 24)
;; (load-file "~/.emacs.d/lisp/cedet-configs.el")

;; rebind as c-electric overrides global binding
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (local-set-key "(" 'skeleton-pair-insert-maybe)
	    (local-set-key "{" 'skeleton-pair-insert-maybe)
	    (local-set-key "[" 'skeleton-pair-insert-maybe)
	    (local-set-key (kbd "M-RET") 'newline-and-indent)
	    ;; (c-toggle-auto-newline 1)
	    ))

;; keybindings
;; (defun c-mode-common-keymaps()
;;   "C/C++ mode keybindings for my skeletons."
;;   ;; keybindings for skeletons
;;   (define-key c-mode-base-map (kbd "C-c f") 'c++-for-skel)
;;   (define-key c-mode-base-map (kbd "C-c i") 'c++-if-skel))

;; hooks
;; (add-hook 'c-mode-common-hook 'c-mode-common-keymaps)


;; Documentation tools
;; doxymacs
(require 'doxymacs)
;; (eval-after-load "doxymacs"
;;   (define-key c-mode-base-map '[(C-c d)] doxymacs-))
