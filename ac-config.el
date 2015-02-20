;; -*- mode: emacs-lisp -*-

;;; auto-complete setup
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/dict")

(setq ac-ignore-case nil
      ac-auto-start 3
      ac-delay 1)


;; NOTE: no need to add programming modes to ac-modes (done by default)

;;; for C/C++
(add-hook 'c-mode-common-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-semantic)))

;;; for Python
;; FIXME: How to update ac-python to make it work with new python.el?
;; python.el: Currently completion candidates are generated by
;; completion-completion-complete-at-point. This requires an inferior
;; python process to work. For some reason doesn't quite import the
;; modules as in the buffer.
(add-hook 'python-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-semantic)))

;; text-mode
(add-to-list 'ac-modes 'text-mode)
(add-hook 'text-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-math-unicode)))

;; message-mode
(add-to-list 'ac-modes 'message-mode)
(add-hook 'message-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-math-unicode)))
