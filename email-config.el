;; -*- mode: emacs-lisp -*-
;;; email-comfig.el --- on bhishma

;;; Code:

;; File associations (email)
(add-to-list 'auto-mode-alist '("/mutt-" . message-mode)) ; mutt
(add-to-list 'auto-mode-alist '("/tmpmsg." . message-mode)) ; claws
(add-to-list 'auto-mode-alist '("\\.eml\\'" . message-mode)) ; GMail w/ "It's all text!"

(require 'external-abook)
(setq external-abook-command "nottoomuch-addresses %s")

(add-hook 'message-mode-hook
	  (lambda ()
	    (turn-on-orgstruct++)
	    (define-key message-mode-map (kbd "C-c C-SPC")
	      'external-abook-try-expand)
	    ;; dynamic abbreviations for org-mode
	    (setq local-abbrev-table message-mode-abbrev-table)))

;; read emails with notmuch
(require 'notmuch)
;; nice notmuch threading interface
(require 'notmuch-pick)
