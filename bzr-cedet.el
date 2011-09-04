;;; bzr-cedet.el -- for full cedet (BZR) configuration

;;; Code:
(load-file "~/.emacs.d/minimal-init.el")
;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))


;; colour themes using color-theme.el
(load-file "~/.emacs.d/lisp/color-theme-dark-emacs.el")

(eval-after-load "color-theme"
  (progn
    (setq color-theme-is-global nil)
    (when (window-system) ; needed for the first frame
      (color-theme-dark-emacs)
      )))

(add-hook 'after-make-frame-functions
	  '(lambda (f)
	     (with-selected-frame f
	       (if (window-system f)
		   (color-theme-dark-emacs)
		 ))))


;; hostname and buffer-name in frame title
;; code originally written by Michael Albinus
;; and a post in emacs-fu.blogspot.com (dotemacs trickery)
(setq-default frame-title-format
	      '(:eval
		(if (string-match-p "^\\*.+\\*$" (buffer-name)) "%b" ; buffer name
		  (format "%s:%s"
			  (or (file-remote-p default-directory 'host) system-name)
			  (buffer-name)))))
		  ;; (format "%s@%s:%s"
		  ;; 	(or (file-remote-p default-directory 'user) user-login-name)


;; `minimal-mode' customisation
(load-library "minimal")
(setq minimal-zap-mode-line nil)
(setq minimal-zap-menu-bar nil)
(minimal-mode)


;; Navigation
;; side scrolling on
(put 'scroll-left 'disabled nil)
;; narrow-to-region enabled
(put 'narrow-to-region 'disabled nil)
;; minibuffer history completion
(mapc
    '(lambda (map)
       (define-key map [(meta p)] 'previous-complete-history-element)
       (define-key map [(meta n)] 'next-complete-history-element))
    (nconc (list minibuffer-local-completion-map
                 minibuffer-local-isearch-map
                 minibuffer-local-map
                 minibuffer-local-must-match-map
		 minibuffer-local-ns-map)))
;; `occur-mode' customisations
(define-key occur-mode-map (kbd "TAB") 'occur-mode-display-occurrence)


;; Editing
;; prefer utf-8
(prefer-coding-system 'utf-8)
;; enabling disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Skeletons (and abbrev customisations)
(load-file "~/.emacs.d/lisp/skeletons.el")

;; rebind as c-electric overrides global binding
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (local-set-key "(" 'skeleton-pair-insert-maybe)
	    (local-set-key "{" 'skeleton-pair-insert-maybe)
	    (local-set-key (kbd "M-RET") 'newline-and-indent)
	    (c-toggle-auto-newline 1)))

;; FIXME: abbrev_defs not being evaluated
;; Abbreviations
(setq abbrev-mode t
      save-abbrevs nil)

;; this hook wraps around the `expand-abbrev' function call
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (add-hook 'abbrev-expand-functions 'expand-abbrev-in-context nil t)))

;; Don't use Egg as cannot jump to source from diff
;; version control related customisations
;; (setq vc-handled-backends '(RCS CVS SVN SCCS Bzr Hg Mtn Arch))
;; Emacs Got Git (git frontend, magit fork)
;; (require 'egg)
;; (load-library "egg-grep")

;; auto-revert-mode for files under version control
(add-hook 'find-file-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)))
	    (if (vc-working-revision (buffer-file-name)) ; (egg-buf-git-name)
		(auto-revert-mode t))
	    ))


;; dired
(put 'dired-find-alternate-file 'disabled nil)

;; `dired-details' & `dired-details+' by Drew Adams
;; reported a bug for this 1 ;) (not in vanilla Emacs)
(require 'dired-details)
(require 'dired-details+)
(require 'dired-sort-menu)
(require 'dired-sort-menu+)
(require 'dired-sort-map)

;; `dired-mode' key bindings
(define-key dired-mode-map (kbd "C-<down>") 'dired-next-subdir)
(define-key dired-mode-map (kbd "C-<up>") 'dired-prev-subdir)
(define-key dired-mode-map (kbd "C-<left>") 'dired-tree-up)
(define-key dired-mode-map (kbd "C-<right>") 'dired-tree-down)
(define-key dired-mode-map (kbd "<tab>") 'dired-hide-subdir)

;; session management (not in vanilla Emacs)
(setq session-use-package t
      session-set-file-name-exclude-regexp
      "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|root.*/include/.+\\|/usr/include/.+\\|~/.mozilla.*itsalltext.*\\|.+\\.eml")

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; coding system to use when writing `session-save-file'
(setq session-save-file-coding-system 'utf-8)

;; Show clock in the modeline
(display-time-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development tools:				     ;;
;; Collection of Emacs Development Environment Tools ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Setup to load latest `cedet'
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/cedet"))
(load-library "~/.emacs.d/lisp/cedet/common/cedet.elc")

;; force `c++-mode' for `*.h' header files
(add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

;; (setq semantic-load-turn-useful-things-on t)
;; (semantic-load-enable-minimum-features)
(semantic-load-enable-gaudy-code-helpers)
(semantic-add-system-include "/usr/include/root/" 'c++-mode)
(semantic-add-system-include "/usr/lib/gcc/x86_64-redhat-linux/4.5.1/include/" 'c++-mode)
(semantic-add-system-include "~/codebaby/B2DXFitters" 'c++-mode)

;; (add-to-list 'eassist-header-switches '("C" "h"))
;; (add-to-list 'eassist-header-switches '("cxx" "hxx"))
(add-to-list 'eassist-header-switches '("cpp" "hpp"))
(add-to-list 'eassist-header-switches '("cxx" "h"))

(add-to-list 'semantic-inhibit-functions
	     (lambda () (not (member major-mode '(c-mode c++-mode)))))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (define-key c-mode-base-map (kbd "C-c ?") 'semantic-ia-complete-symbol)
	    (define-key c-mode-base-map (kbd "C-c c") 'semantic-ia-complete-symbol-menu)
	    (define-key c-mode-base-map (kbd "C-c t") 'semantic-ia-complete-tip)
	    (define-key c-mode-base-map (kbd "C-c v") 'semantic-ia-show-variants)
	    (define-key c-mode-base-map (kbd "C-c d") 'semantic-ia-show-doc)
	    (define-key c-mode-base-map (kbd "C-c s") 'semantic-ia-show-summary)
	    (define-key c-mode-base-map (kbd "C-c m") 'eassist-list-methods)))


;;; bzr-cedet.el ends here
