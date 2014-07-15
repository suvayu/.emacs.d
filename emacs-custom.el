;; -*- mode: emacs-lisp -*-
;;; Emacs custom file

;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-tag-face ((t (:background "blue" :weight bold)))))

;; configs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (quote ("~/.emacs.d/info-manuals")))
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(abbrev-mode t t)
 '(blink-cursor-mode t)
 '(browse-url-browser-function
   (quote
    (("http.*emacswiki.org/.*" . w3m-browse-url)
     ("." . browse-url-default-browser))))
 '(calendar-date-style (quote iso))
 '(case-fold-search nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dark-emacs)))
 '(custom-safe-themes
   (quote
    ("bb69c69b187f6c402c6570501267d3ad86235ef1edd663f2feeeef8669a36657" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dabbrev-case-replace nil)
 '(default-input-method "TeX")
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
 '(ediff-custom-diff-options "-u")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(egg-buffer-hide-section-type-on-start
   (quote
    ((egg-status-buffer-mode . :hunk)
     (egg-commit-buffer-mode . :hunk))))
 '(ibuffer-mode-hook (quote (ibuffer-auto-mode)))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   (quote
    (("sf"
      ((filename . "simplefit")))
     ("noroot"
      ((not filename . "/opt/root/")))
     ("root"
      ((filename . "/opt/root/")))
     ("rootpy"
      ((filename . "rootpy")
       (saved . "py")))
     ("noworg"
      ((not filename . "org/Worg")))
     ("worg"
      ((filename . "org/Worg")))
     ("dev"
      ((or
	(saved . "py")
	(saved . "cpp"))))
     ("lisp"
      ((used-mode . emacs-lisp-mode)))
     ("py"
      ((used-mode . python-mode)))
     ("config"
      ((used-mode . conf-unix-mode)))
     ("org"
      ((used-mode . org-mode)))
     ("cpp"
      ((used-mode . c++-mode))))))
 '(inhibit-startup-screen t)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "british")
 '(iswitchb-mode t)
 '(message-elide-ellipsis "
 [...chomp...chomp...chomp...]

")
 '(mouse-avoidance-mode (quote exile) nil (avoid))
 '(mouse-yank-at-point t)
 '(notmuch-address-command "nottoomuch-addresses")
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-fcc-dirs
   (quote
    (("fatkasuvayu+linux@gmail.com" . "")
     (".\\+" . "sent"))))
 '(notmuch-message-replied-tags (quote ("+replied")))
 '(notmuch-saved-searches
   (quote
    (("Inbox" . "is:inbox")
     ("Inbox (unread)" . "is:inbox and is:unread")
     ("Bfys (unread)" . "tag:bfys and is:unread")
     ("ANA (unread)" . "folder:gwt and is:unread")
     ("me@FOSS" . "(from:suvayu or to:suvayu) and is:foss"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-multipart/alternative-parts nil)
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(org-export-backends (quote (ascii beamer html latex org)))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("ELPA" . "http://tromey.com/elpa/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(safe-local-variable-values
   (quote
    ((local-abbrev-table . latex-mode-abbrev-table)
     (org-latex-to-pdf-process "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")
     (default-input-method . TeX)
     (org-export-allow-BIND . t))))
 '(save-abbrevs nil)
 '(savehist-mode t nil (savehist))
 '(semantic-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space t)
 '(session-set-file-name-exclude-regexp
   "\\.session\\|News/\\|/\\(usr\\|opt/.+\\)/include/\\|~/\\.mozilla.*itsalltext.*\\|.*COMMIT_EDITMSG\\|/tmp/\\(mutt-\\|alot\\.\\).*")
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
 '(wget-download-directory "~/dloads")
 '(windmove-wrap-around t))
