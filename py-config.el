;; -*- mode: emacs-lisp -*-

;; ;; for special navigation commands
;; (require 'nifty)

;; setup keybindings in mode hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (flycheck-mode 1)
	    (blacken-mode 1)
	    (set-fill-column 79)
	    (local-set-key "[" 'skeleton-pair-insert-maybe)
	    (local-set-key "'" 'skeleton-pair-insert-maybe)
	    (local-set-key (kbd "M-RET") 'newline-and-indent)
	    ;; (define-key python-mode-map (kbd "C-a") 'sa-python-nav-beginning-of-statement-special)
	    ;; (define-key python-mode-map (kbd "C-e") 'sa-python-nav-end-of-statement-special)
	    (define-key python-mode-map (kbd "M-a") 'python-nav-beginning-of-block)
	    (define-key python-mode-map (kbd "M-e") 'python-nav-end-of-block)
	    (define-key python-mode-map (kbd "C-<up>") 'python-nav-backward-block)
	    (define-key python-mode-map (kbd "C-<down>") 'python-nav-forward-block)
	    ;; (define-key python-mode-map (kbd "C-M-a") 'python-nav-beginning-of-defun)
	    ;; (define-key python-mode-map (kbd "C-M-e") 'python-nav-end-of-defun)
	    (define-key python-mode-map (kbd "C-b") 'python-nav-backward-defun)
	    (define-key python-mode-map (kbd "C-f") 'python-nav-forward-defun)
	    (define-key python-mode-map (kbd "C-p") 'python-nav-backward-statement)
	    (define-key python-mode-map (kbd "C-n") 'python-nav-forward-statement)
	    ;; (define-key python-mode-map (kbd "C-M-b") 'python-nav-backward-sexp)
	    ;; (define-key python-mode-map (kbd "C-M-f") 'python-nav-forward-sexp)
	    ;; (define-key python-mode-map (kbd "C-M-u") 'python-nav-backward-up-list)
	    ;; (define-key python-mode-map (kbd "C-M-d") 'python-nav-up-list)
	    ))

(autoload 'cython-mode "cython-mode"
  "Major mode for editing Cython files" t)
(add-to-list 'auto-mode-alist (cons "\\.pyx\\'" 'cython-mode))
(add-to-list 'auto-mode-alist (cons "\\.pxd\\'" 'cython-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development tools: Pymacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: experiment with features provided by `ein'

;; ;; Pymacs setup
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; ;;(eval-after-load "pymacs"
;; ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; ;; Rope setup
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
