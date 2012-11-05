;; -*- mode: emacs-lisp -*-
;;; Emacs custom file

;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-5-face ((t (:foreground "color-21" :weight bold))) t)
 '(match ((t (:background "brightyellow" :foreground "black"))))
 '(message-header-cc ((t (:foreground "color-40"))))
 '(message-header-other ((t (:foreground "color-166"))))
 '(message-header-subject ((t (:foreground "color-75" :weight bold))))
 '(message-header-to ((t (:foreground "color-75" :weight bold))))
 '(message-header-xheader ((t (:foreground "color-23")))))

;; diff-context
;; diff-header
;; diff-file-header
;; diff-added
;; diff-indicator-added same but bold
;; diff-removed
;; diff-indicator-removed same but bold

;; if $TERM=xterm-256color
;; '(mode-line ((t (:background "brightwhite" :foreground "black" :box (:line-width -1 :style released-button)))))

;; FIXME: temporary hack for bug in semantic-mode
(load-library "semantic/senator")

;; configs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("~jallad/.emacs.d/info-manuals")))
 '(abbrev-mode t t)
 '(blink-cursor-mode t)
 '(browse-url-browser-function (quote (("http.*emacswiki.org/.*" . w3m-browse-url) ("." . browse-url-default-browser))))
 '(calendar-date-style (quote iso))
 '(case-fold-search nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dark-emacs)))
 '(custom-safe-themes (quote ("b9da76477fdb289c9183bb55aec65df26bc0feaddbdff12b548465af42f1f4e7" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dabbrev-case-replace nil)
 '(default-input-method "TeX")
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
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
 '(notmuch-address-command "nottoomuch-addresses")
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-fcc-dirs (quote (("fatkasuvayu+linux@gmail.com" . "") (".\\+" . "sent"))))
 '(notmuch-message-replied-tags (quote ("+replied")))
 '(notmuch-saved-searches (quote (("Inbox-unread" . "tag:inbox and is:unread") ("NIKHEF" . "tag:nikhef") ("CERN" . "tag:cern") ("lists" . "tag:list") ("Bfys" . "tag:bfys") ("Orgmode" . "tag:org") ("Inbox" . "tag:inbox") ("unread" . "tag:unread"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-multipart/alternative-parts nil)
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("ELPA" . "http://tromey.com/elpa/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(safe-local-variable-values (quote ((org-latex-to-pdf-process "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f") (default-input-method . TeX) (org-export-allow-BIND . t))))
 '(save-abbrevs nil)
 '(savehist-mode t nil (savehist))
 '(semantic-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space t)
 '(session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|\\(/.+/include/.+\\|~/\\.mozilla.*itsalltext.*\\|.*COMMIT_EDITMSG\\)")
 '(session-use-package t nil (session))
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(speedbar-use-images nil)
 '(tramp-default-method "ssh")
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "Suvayu.Ali@cern.ch")
 '(vc-handled-backends (quote (Git RCS CVS SVN SCCS Bzr Hg Mtn Arch)))
 '(w3m-use-cookies t)
 '(windmove-wrap-around t))
