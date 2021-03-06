;;; init.el --- my uber kewl init.el

;;; Commentary:

;;; Code:
(setq debug-on-error t)
      ;; debug-on-signal t
      ;; debug-on-quit t)

;; set PATH to use standalone texlive instead
(setenv "PATH" "/opt/texlive/2020/bin/x86_64-linux:$PATH" t)

;; load path for elisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; kill old org before adding new org path
(load-file "~/.emacs.d/kill-old-org.el")
(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))

(semantic-mode)

;; disable semantic in all non C/C++ buffers
(setq semantic-new-buffer-setup-functions
      (let ((my-modes '(c-mode c++-mode python-mode)))
	(remove-if-not (lambda (el) (member (car el) my-modes))
		       semantic-new-buffer-setup-functions)))

;; `semantic-idle-scheduler-idle-time' is set to 3 secs in customize
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)

;; Emacs C source directory
(setq find-function-C-source-directory "~/build/emacs/src")

;; load custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load-file custom-file)

;; turn on ibuffer by default
(progn (ibuffer) (switch-to-buffer "*scratch*"))

;; Colour theme and other gui related config
(load-file "~/.emacs.d/ui-config.el")	; requires Emacs 24 themes

;; Important movement and editing config with some essential libraries
(load-file "~/.emacs.d/keybindings.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode hooks and other mode specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/lsp-config.el")

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
  "Emacs list mode hook."
  ;; (paredit-mode t)
  (eldoc-mode t))
(add-hook 'lisp-mode-hook 'sa-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'sa-lisp-mode-hook)

;; Python
(load-file "~/.emacs.d/py-config.el")

;; auto-complete
(load-file "~/.emacs.d/ac-config.el")

;; Flycheck
(load-file "~/.emacs.d/flyc-config.el")

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
