;; set PATH to use standalone texlive instead
(setenv "PATH" "/opt/texlive/2011/bin/x86_64-linux:$PATH" t)

;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Info directory
(add-to-list 'Info-default-directory-list
	     (expand-file-name "/opt/emacs-lisp/share/info"))


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
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :background "snow" :foreground "DodgerBlue4" :weight bold))) t)
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-inlinetask ((t (:inherit org-level-8 :slant oblique))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "sandy brown" :weight bold))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(woman-bold ((t (:bold t :weight bold :foreground "forest green"))))
 '(woman-italic ((t (:italic t :slant italic :foreground "salmon")))))


;; configs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(calendar-date-style (quote iso))
 '(case-fold-search nil)
 '(column-number-mode t)
 '(dabbrev-case-replace nil)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(ediff-custom-diff-options "-u")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(ibuffer-mode-hook (quote (ibuffer-auto-mode)))
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "british")
 '(iswitchb-mode t)
 '(mouse-avoidance-mode (quote exile) nil (avoid))
 '(mouse-yank-at-point t)
 '(notmuch-saved-searches (quote (("Inbox-unread" . "tag:inbox and is:unread") ("NIKHEF" . "tag:nikhef") ("CERN" . "tag:cern") ("lists" . "tag:list") ("Bfys" . "tag:bfys") ("Orgmode" . "tag:org") ("Inbox" . "tag:inbox") ("unread" . "tag:unread"))))
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(save-abbrevs nil)
 '(savehist-mode t nil (savehist))
 '(sentence-end-double-space nil)
 '(session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|root.*/include/.+\\|/usr/include/.+\\|~/.mozilla.*itsalltext.*\\|.+\\.eml")
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
 '(windmove-wrap-around t))


(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/text-mode-config.el")
