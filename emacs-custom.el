;;; emacs-custom.el --- custom file

;;; Commentary:

;;; Code:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("~/.emacs.d/info-manuals"))
 '(TeX-PDF-mode t)
 '(TeX-engine 'xetex)
 '(abbrev-mode t t)
 '(blacken-executable "~/.local/bin/black")
 '(blacken-line-length 'fill)
 '(blink-cursor-mode t)
 '(calendar-date-style 'iso)
 '(case-fold-search t)
 '(column-number-mode t)
 '(custom-enabled-themes '(dark-emacs))
 '(custom-safe-themes
   '("6f5095fe825fcc7457ce0f3468adb55b35f1e0a2f5d69b1590452fe2331cf499" default))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dabbrev-case-replace nil)
 '(default-input-method "TeX")
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
 '(ediff-custom-diff-options "-u")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(git-ps1-mode-lighter-text-format "[%s]")
 '(git-ps1-mode-ps1-file "/usr/share/git-core/contrib/completion/git-prompt.sh")
 '(ibuffer-mode-hook '(ibuffer-auto-mode))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("sf"
      ((filename . "simplefit")))
     ("noroot"
      ((not filename . "/opt/root/")))
     ("root"
      ((filename . "/opt/root/")))
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
      ((used-mode . c++-mode)))))
 '(ido-mode 'buffer nil (ido))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ispell-dictionary "british")
 '(magit-save-repository-buffers nil)
 '(message-elide-ellipsis "
 [...chomp...chomp...chomp...]

")
 '(mouse-avoidance-mode 'exile nil (avoid))
 '(mouse-yank-at-point t)
 '(notmuch-address-command "nottoomuch")
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-fcc-dirs
   '(("fatkasuvayu+linux@gmail.com" . "")
     (".\\+" . "Gmail/mysent")))
 '(notmuch-message-replied-tags '("+replied"))
 '(notmuch-saved-searches
   '((:name "Inbox" :query "is:inbox and not (from:youtube or from:strava or from:nasa or from:eso or from:ledger or from:cleartax)")
     (:name "Inbox (unread)" :query "is:inbox and is:unread")
     (:name "Fortnight" :query "date:2weeks..today not is:foss")
     (:name "me @ FOSS" :query "(from:suvayu and is:foss) or is:list")
     (:name "Arrow" :query "is:arrow")))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-all-multipart/alternative-parts nil)
 '(notmuch-tag-formats
   '(("unread" "✉"
      (propertize tag 'face
		  '(:foreground "red")))
     ("flagged" "!"
      (propertize tag 'face
		  '(:foreground "blue")))
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
     ("[-0-9]\\{4,7\\}")
     ("new")))
 '(occur-mode-hook '(turn-on-font-lock next-error-follow-minor-mode))
 '(org-export-backends '(ascii beamer html latex org))
 '(org-link-travis/user-name "suvayu")
 '(org-notmuch-open-function 'org-notmuch-follow-link)
 '(org-notmuch-search-open-function 'org-notmuch-tree-follow-link)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(ac-c-headers blacken gitlab magit-lfs magithub csv-mode anaconda-mode ac-math auto-complete yaml-mode undo-tree d-mode flycheck-dmd-dub flycheck-cython flycheck-julia magit magit-filenotify magit-svn yasnippet yasnippet-snippets json-mode flycheck-scala-sbt sbt-mode scala-mode git-commit cython-mode flycheck-pos-tip lua-mode git-ps1-mode rich-minority neotree smartparens rust-mode sx cmake-mode idomenu hydra flycheck cdlatex vlf kill-ring-search chess auctex ascii-art-to-unicode ac-dabbrev))
 '(printer-name "HP_LaserJet_Pro_MFP_M126nw")
 '(python-shell-interpreter "python3")
 '(read-buffer-completion-ignore-case t)
 '(safe-local-variable-values
   '((eval auto-save-mode nil)
     (backup-inhibited . t)
     (flycheck-gcc-include-path . ".")
     (local-abbrev-table . latex-mode-abbrev-table)
     (org-latex-to-pdf-process "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")
     (default-input-method . TeX)
     (org-export-allow-BIND . t)))
 '(save-abbrevs nil)
 '(savehist-mode t nil (savehist))
 '(search-upper-case t)
 '(semantic-idle-scheduler-idle-time 3)
 '(semantic-mode t)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space t)
 '(session-set-file-name-exclude-regexp
   "\\.session\\|News/\\|/\\(usr\\|opt/.+\\)/include/\\|~/\\.mozilla.*itsalltext.*\\|.*COMMIT_EDITMSG\\|/tmp/\\(mutt-\\|alot\\.\\).*")
 '(session-use-package t nil (session))
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(sp-base-key-bindings 'sp)
 '(speedbar-use-images nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(user-mail-address "fatkasuvayu+linux@gmail.com")
 '(vc-handled-backends '(Git SVN Hg))
 '(windmove-wrap-around t))

;;; emacs-custom.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
