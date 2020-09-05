;;; ui-config.el --- Emacs UI config

;;; Commentary:

;;; Code:
;; font-lock customisations
(defface sa-global-todo-face '((t (:inherit (org-todo))))
  "Face for TODO keywords globally."
  :group 'sa-faces)

(add-hook 'find-file-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIXME\\)\\>" . (1 'font-lock-warning-face prepend))
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


;; hostname and buffer-name in frame title
;; code originally written by Michael Albinus
;; and a post in emacs-fu.blogspot.com (dotemacs trickery)
(setq-default frame-title-format
	      '(:eval
		(if (string-match-p "^\\*.+\\*$" (buffer-name)) "%b" ; buffer name
		  (format "%s:%s"
			  (or (file-remote-p default-directory 'host) system-name)
			  (buffer-name)))))
		  ;; (format "%s@%s:%s"
		  ;; 	(or (file-remote-p default-directory 'user) user-login-name)


;; `minimal-mode' customisation
(require 'minimal)
(setq minimal-zap-mode-line nil)
(setq minimal-zap-menu-bar nil)
(minimal-mode)


;; more concise minor-mode list
(require 'rich-minority)
(rich-minority-mode t)
(setf rm-blacklist
      " \\(Undo-Tree\\|Abbrev\\|min\\|SP\\|FlyC.*\\|ElDoc\\|AC\\|Server\\)")


;; window opacity utilities
(require 'nifty)

;; C-+ will increase opacity (== decrease transparency)
(global-set-key (kbd "C-=")
		'(lambda()
		   (interactive)
		   (sa-opacity-modify)))

;; C-- will decrease opacity (== increase transparency
(global-set-key (kbd "C--")
		'(lambda()
		   (interactive)
		   (sa-opacity-modify t)))

;; C-0 will returns the state to normal
(global-set-key (kbd "C-0")
		'(lambda()
		   (interactive)
		   (modify-frame-parameters nil `((alpha . 100)))))

;; accessibility
(global-set-key '[(C-mouse-4)] 'text-scale-increase) ; scroll up
(global-set-key '[(C-mouse-5)] 'text-scale-decrease) ; scroll down

;;; ui-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
