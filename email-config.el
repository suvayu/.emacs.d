;; -*- mode: emacs-lisp -*-
;;; email-comfig.el --- on bhishma

;;; Code:
;; (require 'orgalist)

;; File associations (email)
(add-to-list 'auto-mode-alist '("/tmp/neomutt-" . message-mode)) ; mutt
(add-to-list 'auto-mode-alist '("alot\\." . message-mode)) ; alot
(add-to-list 'auto-mode-alist '("tmpmsg\\." . message-mode)) ; claws
(add-to-list 'auto-mode-alist '("\\.eml\\'" . message-mode)) ; GMail w/ "It's all text!"

(add-hook 'message-mode-hook
	  (lambda ()
	    ;; turn off automatic filling
	    (auto-fill-mode -1)
	    (orgalist-mode t)
	    ;; (flyspell-mode 1)
	    ;; ;; see FAQ:
	    ;; ;; http://auto-complete.org/doc/manual.html#flyspell-mode-bug
	    ;; (ac-flyspell-workaround)
	    ;; dynamic abbreviations for org-mode
	    (setq local-abbrev-table message-mode-abbrev-table)))

;; read emails with notmuch
;; (require 'notmuch)

;; local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
