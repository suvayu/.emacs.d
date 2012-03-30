;; -*- mode: emacs-lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains some nifty lisp    ;;
;; functions I wrote for my convenience  ;;
;; or I got it from somewhere (credited) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; For MakeClass code
(defun sa-conv(beg end)
  "Add SetBranchAddress(...)."
  (interactive "r")
  (save-excursion
    (if (> beg end) (let* (mid) (setq mid beg beg end end mid)))
    (goto-char beg)
    (while (re-search-forward
	    "\\( \+\\)T\\(Branch\\) \+\\*\\(b_\\)\\([a-zA-Z_0-9]\+\\)\\(\\[[\]\[0-9]\+]\\)\*;" nil t)
      (replace-match "\\1fChain->Set\\2Address(\"\\4\", &\\4, &\\3\\4);" t))))

;; make ATLAS GoodRunList
(defun make-GRL (beg end)
  "Convert the marked region of a ATLAS GoodRunList XML to C
if..else source blocks."
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
	 "\\(<LumiBlockCollection>\n.+<Run>\\)\\([0-9]+\\)\\(</Run>\\)" nil t)
      (replace-match "if ( Run == \\2 ) {" nil nil))
    (goto-char beg)
    (while (re-search-forward
	    "\\(^ +\\)</LumiBlockCollection>"
	    nil t)
      (replace-match "\\1  return false;\n\\1}" nil nil))
    (goto-char beg)
    (while (re-search-forward
	    "\\(<LBRange Start=\"\\)\\([0-9]+\\)\\(\" End=\"\\)\\([0-9]+\\)\\(\"/>\\)"
	    nil t)
      (replace-match "if ( LumiBlock >= \\2 && LumiBlock <= \\4) return true;" nil nil))
    (remove-overlays beg end)
    (message "Coverted GRL XML to C if..else blocks.")
    ))


;; modify opacity of emacs frame
;; works with compositing capable window managers
;; Source: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 5% steps."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil
		     100))
	 (newalpha (if dec (- oldalpha 5)
		     (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit)
	       (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-+ will increase opacity (== decrease transparency)
(global-set-key (kbd "C-=")
		'(lambda()
		   (interactive)
		   (djcb-opacity-modify)))

 ;; C-- will decrease opacity (== increase transparency
(global-set-key (kbd "C--")
		'(lambda()
		   (interactive)
		   (djcb-opacity-modify t)))

 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-0")
		'(lambda()
		   (interactive)
		   (modify-frame-parameters nil `((alpha . 100)))))


;; Transpose org table (from Worg/org-hacks.org by Juan Pechiar)
(defun org-transpose-table-at-point ()
  "Transpose orgmode table at point, eliminate hlines"
  (interactive)
  (let ((contents
         (apply #'mapcar* #'list
                ;; remove 'hline from list
                (remove-if-not 'listp
                               ;; signals error if not table
                               (org-table-to-lisp)))))
    (delete-region (org-table-begin) (org-table-end))
    (insert (mapconcat
	     (lambda (x)
	       (concat "| " (mapconcat 'identity x " | " ) "  |\n" ))
	     contents ""))
    (org-table-align)))


;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
(defun find-org-file-recursively (directory &optional filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory name: ")
  ;; Bind variables
  ;; (if (not (boundp 'directory))
  ;;     (setq directory (read-directory-name "Directory to search: ")))
  (let* (org-file-list
	 (case-fold-search t)		; filesystems are case sensitive
	 (fileregex (if filext (format "^[^.#].*\\.\\(%s$\\)" filext)
		      "^[^.#].*\\.\\(org$\\|org_archive$\\)"))
	 (cur-dir-list (directory-files directory t "^[^.#].*"))) ; exclude .*
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
	(if (string-match fileregex file-or-dir) ; org files
	    (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
	(dolist (org-file (find-org-file-recursively file-or-dir filext)
			  org-file-list) ; add files found to result
	  (add-to-list 'org-file-list org-file)))))))


(provide 'nifty)

;;; end of nifty.el
