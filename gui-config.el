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
      " \\(Undo-Tree\\|Abbrev\\|min\\|SP\\|FlyC.+\\|ElDoc\\|AC\\|Server\\)")


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
