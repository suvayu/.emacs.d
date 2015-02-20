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
