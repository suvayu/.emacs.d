;;; skeletons.el

;;; skeleton definitions

;; skeleton pair insert
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; (defvar skeletons-alist
;;   '((?\( . ?\))
;;     (?{  . ?})
;;     (?[  . ?])
;;     (?\" . ?\")))

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


;; C/C++ skeleton commands
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


;;; abbreviation customisations

;; expand abbrevs in context
(defun sa-expand-abbrev-in-context (expand)
  "Expands abbreviations according to the context. Determines whether within
comments or source by looking at the face name. If within comments the
`text-mode-abbrev-table' is used, the major mode abbrev-table is used otherwise.

Expansion is done by the function passed as the argument. This is controlled by
the \"abnormal\" hook `abbrev-expand-functions'."
;; backward-char checks if end-of-buffer as when point at e-o-b face is `nil'
;; the function call expand does the expansion, usually `expand-abbrev'
  (if (not (save-excursion
	     (string-match "comment\\|string"
			   (symbol-name (if (< (point) (point-max))
					    (face-at-point)
					  (backward-char)
					  (face-at-point))))))
      (funcall expand)
    (let ((local-abbrev-table basic-text-mode-abbrev-table))
      (funcall expand))))


;;; end of skeletons.el
