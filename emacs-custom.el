;; -*- mode: emacs-lisp -*-
;;; Emacs custom file

;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:foreground "red" :weight bold))))
 '(magit-diff-none ((t (:inherit diff-context)))))

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
    ("b07cdea316b1e887f55e0ae350c53973dcf65517a6b3d4619d51a847ad66f194" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dabbrev-case-replace nil)
 '(default-input-method "TeX")
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
 '(ediff-custom-diff-options "-u")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
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
 '(ido-mode (quote buffer) nil (ido))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "british")
 '(message-elide-ellipsis "
 [...chomp...chomp...chomp...]

")
 '(mouse-avoidance-mode (quote exile) nil (avoid))
 '(mouse-yank-at-point t)
 '(notmuch-address-command "nottoomuch")
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-fcc-dirs
   (quote
    (("fatkasuvayu+linux@gmail.com" . "")
     (".\\+" . "Gmail/mysent"))))
 '(notmuch-message-replied-tags (quote ("+replied")))
 '(notmuch-saved-searches
   (quote
    ((:name "Inbox" :query "is:inbox")
     (:name "This fortnight" :query "date:2weeks..today not (is:foss or folder:Gmail/ROOT or (is:lhcb or is:tagging or is:online or is:davinci or is:ganga or is:gauss or is:stats))")
     (:name "Guruji" :query "is:guru")
     (:name "Inbox (unread)" :query "is:inbox and is:unread")
     (:name "Bfys (unread)" :query "tag:bfys and is:unread")
     (:name "ANA (unread)" :query "(folder:Gmail/CP-gwt or is:velodq) and is:unread")
     (:name "me @ ANA" :query "from:suvayu and (folder:Gmail/CP-gwt or is:velodq or is:dsk)")
     (:name "me @ FOSS" :query "(from:suvayu and is:foss) or is:list"))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-multipart/alternative-parts nil)
 '(notmuch-tag-formats
   (quote
    (("unread" "✉"
      (propertize tag
		  (quote face)
		  (quote
		   (:foreground "red"))))
     ("flagged" "!"
      (propertize tag
		  (quote face)
		  (quote
		   (:foreground "blue"))))
     ("signed")
     ("inbox" "i")
     ("attachment" "@")
     ("replied" "↻")
     ("sent" "me")
     ("travel" "✈")
     ("patch" "∓")
     ("guru" "★"
      (notmuch-tag-format-image-data tag
				     (notmuch-tag-star-icon)))
     ("thesis"
      (notmuch-tag-format-image-data tag
				     (notmuch-tag-tag-icon)))
     ("[-0-9]\\{4,7\\}"))))
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(org-export-backends (quote (ascii beamer html latex org)))
 '(org-notmuch-open-function (quote org-notmuch-tree-follow-link))
 '(org-notmuch-search-open-function (quote org-notmuch-tree-follow-link))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("ELPA" . "http://tromey.com/elpa/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
    (ace-link cdlatex vlf kill-ring-search debbugs chess auctex ascii-art-to-unicode ac-dabbrev)))
 '(printer-name "n2")
 '(read-buffer-completion-ignore-case t)
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
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "Suvayu.Ali@cern.ch")
 '(vc-handled-backends (quote (Git RCS CVS SVN Bzr Hg)))
 '(w3m-use-cookies t)
 '(wget-download-directory "~/dloads")
 '(windmove-wrap-around t))
