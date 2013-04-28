;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on kuru

;;; Code:
(setq debug-on-error t)
      ;; debug-on-signal t
      ;; debug-on-quit t)

;; set PATH to use standalone texlive instead
(setenv "PATH" "/opt/texlive/2012/bin/x86_64-linux:$PATH" t)

;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lhcb"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))

;;; Commented out since included in customize
;; ;; Info directory
;; (add-to-list 'Info-default-directory-list
;; 	     (expand-file-name "/opt/emacs-lisp/share/info"))

;; ;; Other info files
;; (add-to-list 'Info-additional-directory-list
;; 	     (expand-file-name "~/.emacs.d/info-manuals"))

;; Emacs C source directory
(setq find-function-C-source-directory "~/build/emacs/src")

;; load custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load-file custom-file)

;; turn on ibuffer by default
(progn (ibuffer)
       (switch-to-buffer "*scratch*"))

;; Colour theme and other gui related config
(load-file "~/.emacs.d/gui-config.el")	; requires Emacs 24 themes

;; Important movement and editing config with some essential libraries
(load-file "~/.emacs.d/keybindings.el")


;; font-lock customisations
(defface sa-global-todo-face
  '((t (:background "royalblue4" :foreground "thistle" :inherit (bold))))
  "Face for TODO keywords globally."
  :group 'sa-faces)

(add-hook 'find-file-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIXME\\)\\>" 1 'font-lock-warning-face prepend)
		   ))
	    ;; (font-lock-add-keywords
	    ;;  nil '(("\\<(\\?)\\>" 1 'font-lock-warning-face prepend)
	    ;; 	   ))
	    ;; (unless (eq major-mode 'org-mode)
	    ;;   (font-lock-add-keywords
	    ;;    nil '(("\\<\\(NB\\|NOTE\\|TODO\\)\\>" 1 'sa-global-todo-face prepend)
	    ;; 	     )))
	    ))

;; ;; Example from Seb
;; (defvar lvn/highlight-org-regexps
;;   "\\(FIXME\\|BUG\\|XXX\\|[Ee]rror\\|[Ww]arning\\|WARNING\\)"
;;   "Patterns to highlight (for Org mode only).")
;;
;; (dolist (mode '(org-mode))
;;   (font-lock-add-keywords mode
;;    `((,lvn/highlight-org-regexps 1 'lvn/highlight-face prepend))))

;; ;; Example from Jambunathan
;;      (font-lock-add-keywords
;;       'org-mode `(("\\(?:^\\(?1:\\*+\\)[[:blank:]]\\)"
;; 		   (0 (progn (compose-region
;; 			      (match-beginning 1) (match-end 1)
;; 			      (pcase (length (match-string 1))
;; 				(1 ?\u2219)
;; 				(2 ?\u2022)
;; 				(3 ?\u25c9)
;; 				(_ ?\u25CB)))
;; 			     nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode hooks and other mode specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Version control related customisations
(load-file "~/.emacs.d/vc-config.el")

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
(load-file "~/.emacs.d/ac-config.el")


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
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

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

;; ;; Mode for EvtGen decay files
;; (load-library "lhcb-dec")
;; (add-to-list 'auto-mode-alist (cons "\\.dec\\'" 'lhcb-dec-mode))

;; ;; `han-mode' for HAN configuration files
;; (autoload 'han-mode "han-mode"
;;   "Major mode for editing HAN configuration files" t)
;; (add-to-list 'auto-mode-alist
;; 	     (cons "\\(run\\|minutes[0-9]\\{1,3\\}\\).config\\'" 'han-mode))


;;; .emacs ends here
