;;; emacs-custom.el --- custom file

;;; Commentary:

;;; Code:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit mode-line :background "dim gray" :inverse-video nil :underline t))))
 '(italic ((t (:inherit default :foreground "blanched almond" :slant normal))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#5fd700"))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "#af00af" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "#005fff" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "#ff00ff" :weight bold))))
 '(lsp-headerline-breadcrumb-path-face ((t (:foreground "white smoke"))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit default)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("~/.emacs.d/info"))
 '(TeX-PDF-mode t)
 '(TeX-engine 'xetex)
 '(abbrev-mode t t)
 '(blacken-line-length nil)
 '(blink-cursor-mode t)
 '(calendar-date-style 'iso)
 '(case-fold-search t)
 '(column-number-mode t)
 '(custom-enabled-themes '(dark-emacs))
 '(custom-safe-themes
   '("d9ab4c91aad0b36fd549cd03816965e7c0dc5bf6c95db781ecd2ead701aa6e3c" default))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dabbrev-case-replace nil)
 '(default-input-method "TeX")
 '(diff-switches "-u")
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-listing-switches "-alh")
 '(display-line-numbers-type 'relative)
 '(display-time-mode t)
 '(ediff-custom-diff-options "-u")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-rst-sphinx-executable "~/.virtualenvs/sphinx/bin/sphinx-build")
 '(git-ps1-mode-lighter-text-format "[%s]")
 '(git-ps1-mode-ps1-file "/usr/share/git-core/contrib/completion/git-prompt.sh")
 '(grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))
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
 '(lsp-ui-doc-delay 3)
 '(lsp-ui-doc-position 'bottom)
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
 '(org-indent-indentation-per-level 0)
 '(org-link-travis/user-name "suvayu")
 '(org-notmuch-open-function 'org-notmuch-follow-link)
 '(org-notmuch-search-open-function 'org-notmuch-tree-follow-link)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(worf tide web-mode helm-rg rg ack helm company dap-mode lsp-pyright lsp-ui flycheck-popup-tip lsp-mode rustic conda use-package flycheck-mypy orgalist ac-c-headers blacken gitlab magit-lfs csv-mode anaconda-mode ac-math auto-complete yaml-mode undo-tree flycheck-cython flycheck-julia magit magit-filenotify yasnippet yasnippet-snippets json-mode sbt-mode scala-mode git-commit cython-mode flycheck-pos-tip lua-mode git-ps1-mode rich-minority neotree smartparens sx cmake-mode idomenu hydra flycheck cdlatex vlf kill-ring-search chess auctex ascii-art-to-unicode))
 '(printer-name "HP_LaserJet_Pro_MFP_M126nw")
 '(python-shell-interpreter "python3")
 '(read-buffer-completion-ignore-case t)
 '(rg-ignore-ripgreprc nil)
 '(rustic-lsp-server 'rust-analyzer)
 '(safe-local-variable-values
   '((flycheck-disabled-checkers emacs-lisp emacs-lisp-checkdoc)
     (blacken-mode)
     (auto-save-mode)
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
 '(semantic-mode nil)
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
 '(tramp-default-method "ssh")
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(user-mail-address "fatkasuvayu+linux@gmail.com")
 '(vc-annotate-color-map
   '((20 . "#00AF00")
     (40 . "#00AF5F")
     (60 . "#00875F")
     (80 . "#008700")
     (100 . "#005F00")
     (120 . "#005F87")
     (140 . "#005FAF")
     (160 . "#005FD7")
     (180 . "#005FFF")
     (200 . "#0000FF")
     (220 . "#0000D7")
     (240 . "#0000AF")
     (260 . "#000087")
     (280 . "#00005F")
     (300 . "#080808")
     (320 . "#1C1C1C")
     (340 . "#303030")
     (360 . "#444444")))
 '(vc-handled-backends '(Git SVN Hg))
 '(windmove-wrap-around t))


(defun sa-install-packages (pkgs)
  "Initial install of packages from ELPA

Eval: (my-install-packages '(pkg1 pkg2))
"
  (cl-loop for pkg in pkgs
	do (package-install pkg)))

;;; emacs-custom.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
