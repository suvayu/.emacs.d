;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on kuru

;;; Code:
(setq debug-on-error t)
      ;; debug-on-signal t
      ;; debug-on-quit t)

;; set PATH to use standalone texlive instead
(setenv "PATH" "/opt/texlive/2011/bin/x86_64-linux:$PATH" t)

;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/contrib/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lhcb"))

;; ;; Info directory
;; (add-to-list 'Info-default-directory-list
;; 	     (expand-file-name "/opt/emacs-lisp/share/info"))

;; ;; Other info files
;; (add-to-list 'Info-additional-directory-list
;; 	     (expand-file-name "~/.emacs.d/info-manuals"))

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
 '(font-lock-warning-face ((((type tty)) (:inherit error :weight bold))))
 '(info-menu-header ((t (:bold t :family "Sans Serif" :foreground "tomato" :weight bold))))
 '(info-node ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))
 '(info-xref ((t (:inherit link :foreground "powder blue" :weight bold))))
 '(info-xref-visited ((t (:foreground "violet" :underline t :weight bold))))
 '(link ((t (:foreground "cyan" :underline t :weight extra-bold))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :background "snow" :foreground "DodgerBlue4" :weight bold))) t)
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-inlinetask ((t (:inherit org-level-8 :slant oblique))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "sandy brown" :weight bold))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(woman-bold ((t (:bold t :weight bold :foreground "forest green"))) t)
 '(woman-italic ((t (:italic t :slant italic :foreground "salmon"))) t))

;; if $TERM=xterm-256color
;; '(mode-line ((t (:background "brightwhite" :foreground "black" :box (:line-width -1 :style released-button)))))


;; configs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("~jallad/.emacs.d/info-manuals")))
 '(abbrev-mode t t)
 '(browse-url-browser-function (quote (("http.*emacswiki.org/.*" . w3m-browse-url) ("." . browse-url-default-browser))))
 '(calendar-date-style (quote iso))
 '(case-fold-search nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dark-emacs)))
 '(custom-safe-themes (quote ("9d04c19fa8d227f59234596060d285282599de77e0ee4f418aae5191a3bbf6bd" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dabbrev-case-replace nil)
 '(default-input-method "TeX")
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
 '(notmuch-saved-searches (quote (("Inbox-unread" . "tag:inbox and is:unread") ("NIKHEF" . "tag:nikhef") ("CERN" . "tag:cern") ("lists" . "tag:list") ("Bfys" . "tag:bfys") ("Orgmode" . "tag:org") ("Inbox" . "tag:inbox") ("unread" . "tag:unread"))))
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("ELPA" . "http://tromey.com/elpa/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(safe-local-variable-values (quote ((org-latex-to-pdf-process "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f") (default-input-method . TeX) (org-export-allow-BIND . t))))
 '(save-abbrevs nil)
 '(savehist-mode t nil (savehist))
 '(semantic-mode t)
 '(sentence-end-double-space nil)
 '(session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|\\(/.+/include/.+\\|~/\\.mozilla.*itsalltext.*\\|.*COMMIT_EDITMSG\\)")
 '(session-use-package t nil (session))
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(speedbar-use-images nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "Suvayu.Ali@cern.ch")
 '(vc-handled-backends (quote (Git RCS CVS SVN SCCS Bzr Hg Mtn Arch)))
 '(w3m-use-cookies t)
 '(windmove-wrap-around t))


;; turn on ibuffer by default
(progn (ibuffer)
       (switch-to-buffer "*scratch*"))

;; Colour theme and other gui related config
(load-file "~/.emacs.d/gui-config.el")	; requires color-theme.el

;; Important movement and editing config with some essential libraries
(load-file "~/.emacs.d/keybindings.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode hooks and other mode specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C/C++
(load-file "~/.emacs.d/cpp-config.el")

;; `text-mode' and `org-mode' madness ;)
(load-file "~/.emacs.d/org-mode-config.el")
(load-file "~/.emacs.d/text-mode-config.el") ; has some org dependence

;; use with mutt, ItsAllText and eml files
(load-file "~/.emacs.d/email-config.el") ; has some org dependence

;; mode to edit markdown files (e.g. StackOverflow answers with ItsAllText)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))

;; Lisp/Elisp customisations
(defun sa-lisp-mode-hook ()
  (eldoc-mode t))
(add-hook 'lisp-mode-hook 'sa-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'sa-lisp-mode-hook)

;; Python
(load-file "~/.emacs.d/py-config.el")

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/dict")

(setq ac-ignore-case nil
      ac-auto-start 3
      ac-delay 0.2)

(add-hook 'c-mode-common-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-semantic)))

;; FIXME: How to update ac-python to make it work with new python.el?
;; python.el: Currently completion candidates are generated by
;; completion-completion-complete-at-point. This requires an inferior
;; python process to work. For some reason doesn't quite import the
;; modules as in the buffer.
(add-hook 'python-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-semantic)))

(require 'ac-math)
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-latex-commands)
	    (add-to-list 'ac-sources 'ac-source-math-latex)))
(add-to-list 'ac-modes 'latex-mode)

;; project aware buffer management (uses vc directories)
(require 'projectile)
(projectile-global-mode)

;; w3m key bindings
(defun sa-w3m-mode-hook ()
  "Set up some w3m tabbed browsing key bindings."
  (define-key w3m-mode-map (kbd "<up>") 'previous-line)
  (define-key w3m-mode-map (kbd "<down>") 'next-line)
  (define-key w3m-mode-map (kbd "<left>") 'left-char)
  (define-key w3m-mode-map (kbd "<right>") 'right-char)
  (define-key w3m-mode-map (kbd "C-<tab>") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "C-<backtab>") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "C-S-<iso-lefttab>") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "s-<tab>") 'w3m-select-buffer)
  (toggle-truncate-lines t))
(add-hook 'w3m-mode-hook 'sa-w3m-mode-hook)

;; shell-script-mode customisations
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ANSI colours in Emacs shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Emacs Got Git (git frontend, magit fork)
;; (require 'egg)
;; (load-library "egg-grep")
;;(require 'egg-grep) ; require doesn't work as library is not "provided"
;; (setq egg-auto-update t)

;; ;; special buffers
;; (setq special-display-buffer-names
;;	 '("*grep*" "*tex-shell*" "*compilation*" "*find*"))

;; start a server and make sure it has a name
;; (require 'server)
;; (setq server-host (system-name)
;; 	       server-use-tcp t)
;; (server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional libraries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; help enhancements by Drew Adams
;; (require 'help-mode+)
;; (require 'help+)
;; (require 'help-fns+)

;; auto-install.el
(autoload 'auto-install-from-emacswiki "auto-install"
  "Install an elisp file from EmacsWiki.org." t)
(autoload 'auto-install-from-url "auto-install"
  "Install an elisp file from a given url." t)
;; auto-install settings (not in vanilla Emacs)
(eval-after-load 'auto-install
  (setq auto-install-directory "~/.emacs.d/lisp/"))
;; the "/" at the end of the path is absolutely essential,
;; otherwise the elisp files are saved as elisp*.el instead of elisp/*.el



;;;;;;;;;;;;;;;;;;;;;;;;
;; HEP specific modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; `cmt-mode' for CMT requirements files
(autoload 'cmt-mode "cmt-mode"
  "Mode to fontify and syntax highlight buffer while editing
 CMT requirements file." t)
(add-to-list 'auto-mode-alist (cons "\\requirements\\'" 'cmt-mode))

;; Mode for EvtGen decay files
(load-library "lhcb-dec")
(add-to-list 'auto-mode-alist (cons "\\.dec\\'" 'lhcb-dec-mode))

;; ;; `han-mode' for HAN configuration files
;; (autoload 'han-mode "han-mode"
;;   "Major mode for editing HAN configuration files" t)
;; (add-to-list 'auto-mode-alist
;; 	     (cons "\\(run\\|minutes[0-9]\\{1,3\\}\\).config\\'" 'han-mode))


;;; .emacs ends here
