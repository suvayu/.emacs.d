;;; bzr-cedet.el -- for full cedet (BZR) configuration

;;; Code:
(load-file "~/.emacs.d/minimal-init.el")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development tools:				     ;;
;; Collection of Emacs Development Environment Tools ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Setup to load latest `cedet'
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/cedet"))
(load-file "~/.emacs.d/lisp/cedet/cedet-devel-load.el")

;; (setq semantic-load-turn-useful-things-on t)
;; (semantic-load-enable-minimum-features)
(semantic-load-enable-gaudy-code-helpers)
(semantic-add-system-include "/usr/include/root/" 'c++-mode)
(semantic-add-system-include "/usr/lib/gcc/x86_64-redhat-linux/4.5.1/include/" 'c++-mode)

;; (add-to-list 'eassist-header-switches '("C" "h"))
;; (add-to-list 'eassist-header-switches '("cxx" "hxx"))
;; (add-to-list 'eassist-header-switches '("cpp" "hpp"))
;; (add-to-list 'eassist-header-switches '("cxx" "h"))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (define-key c-mode-base-map (kbd "C-c ?") 'semantic-ia-complete-symbol)
	    (define-key c-mode-base-map (kbd "C-c t") 'semantic-ia-complete-tip)
	    (define-key c-mode-base-map (kbd "C-c v") 'semantic-ia-show-variants)
	    (define-key c-mode-base-map (kbd "C-c d") 'semantic-ia-show-doc)
	    (define-key c-mode-base-map (kbd "C-c s") 'semantic-ia-show-summary)
	    (define-key c-mode-base-map (kbd "C-c c") 'semantic-ia-complete-symbol-menu)
	    (define-key c-mode-base-map (kbd "C-c m") 'eassist-list-methods)))

;;; bzr-cedet.el ends here
