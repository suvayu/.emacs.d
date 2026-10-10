;;; templates.el

;;; C/C++ skeleton commands
;; for loop
(define-skeleton c++-for-skel
  "Insert a for loop in `c++-mode'." nil
  "for (" > _ ")" > \n 
  ?\{ > \n
  > _ \n
  ?} > )

;; if conditional
(define-skeleton c++-if-skel
  "Insert if block in `c++-mode'." nil
  "if (" > _ ")" > \n 
  ?\{ > \n
  > _ \n
  ?} > )


;; (defadvice delete-backward-char (before delete-empty-pair activate)
;;   (if (eq (cdr (assq (char-before) skeletons-alist)) (char-after))
;;       (and (char-after) (delete-char 1))))

;; (defadvice backward-kill-word (around delete-pair activate)
;;   (if (eq (char-syntax (char-before)) ?\()
;;       (progn
;; 	(backward-char 1)
;; 	(save-excursion
;; 	  (forward-sexp 1)
;; 	  (delete-char -1))
;; 	(forward-char 1)
;; 	(append-next-kill)
;; 	(kill-backward-chars 1))
;;     ad-do-it))

(provide 'templates)
;;; end of templates.el
