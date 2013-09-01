;;
;; Kill any old org remnants (thanks to Achim)
;;

;; clean load-path
(setq load-path
      (delq nil (mapcar
		 (function (lambda (p)
			     (unless (string-match "lisp/org$" p)
			       p)))
		 load-path)))
;; remove property lists to defeat cus-load and remove autoloads
(mapatoms (function  (lambda (s)
		       (let ((sn (symbol-name s)))
			 (when (string-match "^\\(org\\|ob\\|ox\\)-?" sn)
			   (setplist s nil)
			   (when (autoloadp s)
			     (unintern s)))))))
