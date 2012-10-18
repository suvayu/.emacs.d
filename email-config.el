;; -*- mode: emacs-lisp -*-
;;; email-comfig.el --- on bhishma

;;; Code:

;; File associations (email)
(add-to-list 'auto-mode-alist '("/mutt-" . message-mode)) ; mutt
(add-to-list 'auto-mode-alist '("/tmpmsg." . message-mode)) ; claws
(add-to-list 'auto-mode-alist '("\\.eml\\'" . message-mode)) ; GMail w/ "It's all text!"


(load-library "external-abook")
(setq external-abook-command "nottoomuch-addresses %s")
(define-key message-mode-map (kbd "C-c C-SPC") 'external-abook-try-expand)
