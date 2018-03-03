;; -*- mode: emacs-lisp -*-
;;; .emacs --- my uber kewl .emacs on kuru

;;; Code:
(setq debug-on-error t)
      ;; debug-on-signal t
      ;; debug-on-quit t)

;; set PATH to use standalone texlive instead
(setenv "PATH" "/opt/texlive/2015/bin/x86_64-linux:$PATH" t)

;; load path for elisp files
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/lhcb"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; kill old org before adding new org path
(load-file "~/.emacs.d/kill-old-org.el")
(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))

;;; Load development CEDET
(load-file "~/.emacs.d/lisp/cedet/cedet-devel-load.el")

;; disable semantic in all non C/C++ buffers
(add-to-list 'semantic-inhibit-functions
	     (lambda () (not (member major-mode '(c-mode c++-mode)))))

;; `semantic-idle-scheduler-idle-time' is set to 3 secs in customize
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)

;; Emacs C source directory
(setq find-function-C-source-directory "~/build/emacs/src")

;; load custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load-file custom-file)
(package-initialize)

;; turn on ibuffer by default
(progn (ibuffer) (switch-to-buffer "*scratch*"))

;; Colour theme and other gui related config
(load-file "~/.emacs.d/gui-config.el")	; requires Emacs 24 themes

;; Important movement and editing config with some essential libraries
(load-file "~/.emacs.d/keybindings.el")


;; font-lock customisations
(defface sa-global-todo-face '((t (:inherit (org-todo))))
  "Face for TODO keywords globally."
  :group 'sa-faces)

(add-hook 'find-file-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIXME\\)\\>:" . (1 'font-lock-warning-face prepend))
		   ;; ("\\<(\\?)\\>" . (1 'font-lock-warning-face prepend))
		   ))
	    (unless (eq major-mode 'org-mode)
	      (font-lock-add-keywords
	       nil '(("\\<\\(NB\\|QN\\|NOTE\\|TODO\\)\\>:" . (1 'sa-global-todo-face prepend)))))))

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

;; `taskjuggler-mode' for tjp files
(autoload 'taskjuggler-mode "taskjuggler-mode"
  "Major mode for editing TaskJuggler input files." t)
(add-to-list 'auto-mode-alist (cons "\\.tjp\\'" 'taskjuggler-mode))

;; Lisp/Elisp customisations
(defun sa-lisp-mode-hook ()
  (eldoc-mode t))
(add-hook 'lisp-mode-hook 'sa-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'sa-lisp-mode-hook)

;; Python
(load-file "~/.emacs.d/py-config.el")

;; auto-complete
(load-file "~/.emacs.d/ac-config.el")

;; shell-script-mode customisations
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ANSI colours in Emacs shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

;; NOTE: to customise behaviour of special buffers, see:
;; `display-buffer-alist' & `display-buffer').  E.g. special buffers:
;; '("*grep*" "*tex-shell*" "*compilation*" "*find*")

;;; init.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
