;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on bhishma.homelinux.net
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; libraries in use:							 ;;
;; 	dired-details, dired-details+,					 ;;
;; 	dired-sort-menu, dired-sort-menu+, dired-sort-map		 ;;
;; libraries loaded as required:					 ;;
;; 	cedet, semantic, easist						 ;;
;; libraries ready to use:						 ;;
;; 	auto-install, ecb						 ;;
;; 									 ;;
;; NB:	the fn to set $ROOTSYS works only for network mounted		 ;;
;; 	directories. have to append user@host in front to make		 ;;
;; 	it work for all situations					 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(setq debug-on-error t)
      ;; debug-on-signal t
      ;; debug-on-quit t)


;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/contrib/lisp"))

;; ;; Info directory
;; (add-to-list 'Info-default-directory-list
;; 	     (expand-file-name "/opt/emacs-lisp/share/info"))

;; Emacs C source directory
(setq find-function-C-source-directory "~/build/emacs/src")


;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon"))))
 '(info-menu-header ((t (:bold t :family "Sans Serif" :foreground "tomato" :weight bold))) t)
 '(info-node ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))) t)
 '(info-xref ((t (:inherit link :foreground "powder blue" :weight bold))) t)
 '(info-xref-visited ((t (:foreground "violet" :underline t :weight bold))) t)
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :background "snow" :foreground "DodgerBlue4" :weight bold))) t)
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "sandy brown" :weight bold))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(woman-bold ((t (:bold t :weight bold :foreground "forest green"))))
 '(woman-italic ((t (:italic t :slant italic :foreground "salmon")))))

;; if $TERM=xterm-256color
;; '(mode-line ((t (:background "brightwhite" :foreground "black" :box (:line-width -1 :style released-button)))))


;; configs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(browse-url-browser-function (quote (("http.*emacswiki.org/.*" . w3m-browse-url) ("." . browse-url-default-browser))))
 '(calendar-date-style (quote iso))
 '(case-fold-search nil)
 '(column-number-mode t)
 '(dabbrev-case-replace nil)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(ediff-custom-diff-options "-u")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :hunk) (egg-commit-buffer-mode . :hunk))))
 '(ibuffer-mode-hook (quote (ibuffer-auto-mode)))
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "british")
 '(iswitchb-mode t)
 '(mouse-avoidance-mode (quote exile) nil (avoid))
 '(mouse-yank-at-point t)
 '(notmuch-saved-searches (quote (("Orgmode-recent" . "emacs-orgmode.gnu.org and \"$(($(date +%s)-5270400))..$(date +%s)\"") ("Orgmode-new" . "emacs-orgmode.gnu.org and is:unread") ("Orgmode" . "emacs-orgmode.gnu.org") ("inbox" . "tag:inbox") ("unread" . "tag:unread"))))
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("ELPA" . "http://tromey.com/elpa/"))))
 '(safe-local-variable-values (quote ((default-input-method . TeX) (org-export-allow-BIND . t))))
 '(savehist-mode t nil (savehist))
 '(sentence-end-double-space nil)
 '(session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|root.*/include/.+")
 '(session-use-package t nil (session))
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(speedbar-use-images nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(w3m-use-cookies t)
 '(windmove-wrap-around t))


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

;; ;; alternate theming API, supported by native Emacs >=24
;; ;; colour theme using internal emacs theming
;; (load-theme 'dark-emacs)

;; (when (window-system) ; needed for the first frame
;;   (load-theme 'dark-emacs))

;; (add-hook 'after-make-frame-functions
;; 	  '(lambda (f)
;; 	     (with-selected-frame f
;; 	       (if (window-system f)
;; 		   (load-theme 'dark-emacs)
;; 		 ))))


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
(load-library "nifty")

;; Navigation
;; side scrolling on
(put 'scroll-left 'disabled nil)
;; narrow-to-region enabled
(put 'narrow-to-region 'disabled nil)
;; navigate thru windows using M-<arrow>
(windmove-default-keybindings 'meta)
;; Move point to the first non-whitespace character on this line.
(define-key global-map (kbd "s-a") 'back-to-indentation)
;; Completion
(define-key global-map (kbd "s-<tab>") 'completion-at-point)
;; ;; mouse support on an xterm
;; (xterm-mouse-mode t)

;; `occur-mode' customisations
(define-key occur-mode-map (kbd "TAB") 'occur-mode-display-occurrence)

;; Editing
;; prefer utf-8
(prefer-coding-system 'utf-8)
;; enabling disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; tabs with respect to the previous non-blank line
;; (define-key global-map (kbd "S-<iso-lefttab>") 'indent-relative)
(define-key global-map (kbd "<backtab>") 'indent-relative)
;; (global-set-key '[(backtab)] 'indent-relative)

;; undo-tree (not in vanilla Emacs)
(require 'undo-tree)
(global-undo-tree-mode)

;; browse-kill-ring (not in vanilla Emacs)
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; ;; FIXME:
;; (defadvice isearch-yank-kill
;; (around toggle-case-fold-search-maybe)
;; "If case-fold-search is t toggle it. Restore it after isearch finishes."
;; (let ((tmp-case-fold-search case-fold-search))
;;   (if case-fold-search
;;       (progn (setq case-fold-search nil)
;; 	     ad-do-it)
;;     ad-do-it)
;;   (setq case-fold-search tmp-case-fold-search)))
;; (ad-activate 'isearch-yank-kill)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs pimped up!					   ;;
;; Skeletons, Templates, Abbreviations and Keyboard Macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Load CEDET (do not move later, conflicts with eieio bundled with Emacs 24)
;; (load-file "~/.emacs.d/lisp/cedet-configs.el")

;; Skeletons (and abbrev customisations)
(load-file "~/.emacs.d/lisp/skeletons.el")

;; Abbreviations
;; location of the abbreviation definition file
(setq abbrev-file-name (expand-file-name "~/.emacs.d/lisp/abbrev_defs"))
(setq save-abbrevs nil)

;; this hook wraps around the `expand-abbrev' function call
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (add-hook 'abbrev-expand-functions 'expand-abbrev-in-context nil t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode hooks and other mode specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `text-mode' and `org-mode' madness ;)
(load-file "~/.emacs.d/lisp/org-mode-settings.el")
(load-file "~/.emacs.d/lisp/text-mode-like-settings.el")

;; (add-hook 'muse-mode-hook 'my-text-mode-hook)
(add-hook 'text-mode-hook 'my-text-mode-hook)
;; File associations (email)
(add-to-list 'auto-mode-alist '("/mutt-" . message-mode)) ; mutt
(add-to-list 'auto-mode-alist '("/tmpmsg." . message-mode)) ; claws
(add-to-list 'auto-mode-alist (cons "\\.eml\\'" 'message-mode)) ; GMail w/ "It's all text!"

;; read emails with notmuch
(require 'notmuch)

;; w3m
(add-hook 'w3m-mode-hook 'my-w3m-mode-hook)

;; shell-script-mode customisations
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ANSI colours in Emacs shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; version control related customisations

;; auto-revert-mode for files under version control
(add-hook 'find-file-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)))
	    (if (vc-working-revision (buffer-file-name))
		(auto-revert-mode t))
	    ))

;; mode to edit git commit message
(autoload 'git-commit-mode "git-commit"
  "Major mode for editing git commit messages." t)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))
(add-hook 'git-commit-mode-hook
	  (lambda ()
	    (turn-on-orgstruct++)))

;; Emacs Got Git (git frontend, magit fork)
(require 'egg)
(require 'egg-grep)
;; (setq egg-auto-update t)

;; ;; special buffers
;; (setq special-display-buffer-names
;;	 '("*grep*" "*tex-shell*" "*compilation*" "*find*"))

;; start a server and make sure it has a name
;; (require 'server)
;; (setq server-host (system-name)
;; 	       server-use-tcp t)
;; (server-start)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; onyanyo jhinchak stuff:			;;
;; auto-install, dired-details, dired-details+, ;;
;; icicles, dired-sort-menu, dired-sort-menu+	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-install settings (not in vanilla Emacs)
(eval-after-load 'auto-install
  (setq auto-install-directory "~/.emacs.d/lisp/"))
;; the "/" at the end of the path is absolutely essential,
;; otherwise the elisp files are saved as elisp*.el instead of elisp/*.el


;; dired
(put 'dired-find-alternate-file 'disabled nil)

;; `dired-details' & `dired-details+' by Drew Adams
;; reported a bug for this 1 ;) (not in vanilla Emacs)
(require 'dired-details)
(require 'dired-details+)
(require 'dired-sort-menu)
(require 'dired-sort-menu+)
(require 'dired-sort-map)

;; ;; load dired-x in dired (enables omitting files)
;; (add-hook 'dired-load-hook
;;	  (lambda ()
;;	    (require 'dired-x)))
;; ;; turns omiting on by default
;; (setq dired-omit-mode t)

;; `dired-mode' key bindings
(define-key dired-mode-map (kbd "C-<down>") 'dired-next-subdir)
(define-key dired-mode-map (kbd "C-<up>") 'dired-prev-subdir)
(define-key dired-mode-map (kbd "C-<left>") 'dired-tree-up)
(define-key dired-mode-map (kbd "C-<right>") 'dired-tree-down)
(define-key dired-mode-map (kbd "<tab>") 'dired-hide-subdir)


;; lazy-bones
(defalias 'yes-or-no-p 'y-or-n-p)
;; session management (not in vanilla Emacs)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; coding system to use when writing `session-save-file'
(setq session-save-file-coding-system 'utf-8)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development tools:				     ;;
;; Collection of Emacs Development Environment Tools ;;
;; & Emacs Code Browser	and many more customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Python customisations

;; Pymacs setup
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))


;; Lisp/Elisp customisations
(defun my-lisp-mode-hook ()
  (eldoc-mode t))
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)


;; C++ customisations
;; force `c++-mode' for `*.h' header files
;; (add-to-list 'auto-mode-alist (cons "\\.h\\'" 'c++-mode))

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


;; ATLAS specific modes
;; `cmt-mode' for CMT requirements files
(autoload 'cmt-mode "cmt-mode"
  "Mode to fontify and syntax highlight buffer while editing
 CMT requirements file." t)
(add-to-list 'auto-mode-alist (cons "\\requirements\\'" 'cmt-mode))

;; ;; `han-mode' for HAN configuration files
;; (autoload 'han-mode "han-mode"
;;   "Major mode for editing HAN configuration files" t)
;; (add-to-list 'auto-mode-alist
;; 	     (cons "\\(run\\|minutes[0-9]\\{1,3\\}\\).config\\'" 'han-mode))


;; get directory of a file in the current buffer
;; (file-name-directory buffer-file-name)

;;; .emacs ends here
