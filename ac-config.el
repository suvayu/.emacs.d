;; -*- mode: emacs-lisp -*-

;;; auto-complete setup
(require 'auto-complete-config)
(ac-config-default)

(defun sa-banish-yasnippet-from-ac-sources ()
  (setq ac-sources (delete 'ac-source-yasnippet ac-sources)))
(add-hook 'auto-complete-mode-hook 'sa-banish-yasnippet-from-ac-sources)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/dict") ; already default
(setq ac-ignore-case 'smart
      ac-auto-start 3
      ac-candidate-limit 10
      ac-delay 1)


;; NOTE: no need to add programming modes to ac-modes (done by default)

;;; for C/C++
(require 'auto-complete-clang)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-semantic)
	    (add-to-list 'ac-sources 'ac-source-clang)
	    ;; ;; stop completing in std namespace, too many candidates
	    ;; (make-local-variable 'ac-stop-words)
	    ;; (add-to-list 'ac-stop-words "std")
	    ))

;;; for Python
;; FIXME: Currently in python.el completion candidates are generated
;; by python-completion-at-point. This requires an inferior python
;; process to work. For some reason doesn't quite import the modules
;; as in the buffer.
(if (>= emacs-major-version 25)
    (ac-define-source python
      '((candidates . (python-completion-at-point))
	))
  (ac-define-source python
    '((candidates . (python-completion-complete-at-point))
      )))
(add-hook 'python-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-python)))

;;; math support for text composition modes
(require 'ac-math)

;; latex-mode
(add-to-list 'ac-modes 'latex-mode)
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (add-to-list 'ac-sources 'ac-source-latex-commands)
	    (add-to-list 'ac-sources 'ac-source-math-latex)
	    (add-to-list 'ac-sources 'ac-source-math-unicode)))

;; add unicode math source for text modes
(defun sa-ac-math-unicode()
  (add-to-list 'ac-sources 'ac-source-math-unicode))

(mapcar (lambda (mode)
	  (add-to-list 'ac-modes mode)
	  (add-hook (intern (format "%s-hook" mode)) 'sa-ac-math-unicode))
	'(text-mode message-mode org-mode))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
