;; -*- mode: emacs-lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains some nifty lisp    ;;
;; functions I wrote for my convenience  ;;
;; or I got it from somewhere (credited) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sa-search-n-comment(str)
  "Search for string and comment line"
  (interactive "sString: ")
  (let ((repeat t))
    (while repeat
      (search-forward str)
      (comment-region (line-beginning-position) (line-end-position))
      (next-line)
      (setf repeat (y-or-n-p "Repeat? ")))))

;; insert gmane http link from message id
(defun sa-insert-gmane-link(msgid)
  "Insert gmane http link at point.  Promptsx for message id."
  (interactive "sMessage ID: ")
  (insert (format "<http://mid.gmane.org/%s>" msgid)))


;; FIXME: add uniquify support
;; (uniquify-item-buffer
;;  (uniquify-make-item (buffer-name) default-directory indirect-buffer))
(defun sa-make-indirect-buffer()
  "Make indirect buffer to current buffer and switch to it."
  (interactive)
  (let ((ibuf (make-indirect-buffer
	       (current-buffer)
	       (generate-new-buffer-name (buffer-name)) t)))
    (switch-to-buffer ibuf)))


;; isearch wrappers to search in other window
;; Source: http://www.unixuser.org/~ysjj/emacs/lisp/misc-funcs.el
(defun sa-isearch-backward-other-window (arg)
  "Do incremental search backward on the ARG'th different window
of this frame."
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-backward)
    (other-window (- arg))))

(defun sa-isearch-forward-other-window (arg)
  "Do incremental search forward on the ARG'th different window
of this frame."
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-forward)
    (other-window (- arg))))

(defun sa-isearch-backward-regexp-other-window (arg)
  "Do incremental search backward for regular expression on the
ARG'th different window of this frame.  Like ordinary incremental
search except that your input is treated as a regexp"
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-backward-regexp)
    (other-window (- arg))))

(defun sa-isearch-forward-regexp-other-window (arg)
  "Do incremental search forward for regular expression on the
ARG'th different window of this frame.  Like ordinary incremental
search except that your input is treated as a regexp"
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-forward-regexp)
    (other-window (- arg))))


;; Function to add duplicate org-mode properties
(defun sa-org-entry-put-dupe (pom property value)
  "Set PROPERTY to VALUE for entry at point-or-marker POM.

Original function: `org-entry-put'.

NB: Allows duplicate properties.  In case of duplicates, they are inserted
before the existing entry.  Use with caution."
  (org-with-point-at pom
    (org-back-to-heading t)
    (let ((beg (point)) (end (save-excursion (outline-next-heading) (point)))
	  range)
      (let ((buffer-invisibility-spec (org-inhibit-invisibility))) ; Emacs 21
	(setq range (org-get-property-block beg end 'force))
	(goto-char (car range))
	(progn
	  (if (re-search-forward
	       (org-re-property-keyword property) (cdr range) t)
	      (goto-char (match-beginning 0))
	    (goto-char (cdr range)))
	  (insert "\n")
	  (backward-char 1)
	  (org-indent-line))
	(insert ":" property ":")
	(and value (insert " " value))
	(org-indent-line)))
    (run-hook-with-args 'org-property-changed-functions property value)))


;; my beamer options template for the new exporter
(defun sa-org-e-beamer-insert-options-template (&optional kind)
  "Insert a settings template, to make sure users do this right.

Original function: `org-e-beamer-insert-options-template'.

NB: This is a custom version that ignores duplicate \"EXPORT_LaTeX_HEADER+\"
entries when inserting the template for a subtree.  In this case, calling this
function repeatedly will keep adding duplicate EXPORT_LaTeX_HEADER+ entries."
  (interactive (progn
		 (message "Current [s]ubtree or [g]lobal?")
		 (if (eq (read-char-exclusive) ?g) (list 'global)
		   (list 'subtree))))
  (if (eq kind 'subtree)
      (progn
	(org-back-to-heading t)
	(org-reveal)
	(org-entry-put nil "EXPORT_LaTeX_CLASS" "beamer")
	(org-entry-put nil "EXPORT_LaTeX_CLASS_OPTIONS" "[presentation,smaller]")
	(org-entry-put nil "EXPORT_FILE_NAME" "Suvayu_presentation.pdf")
	(org-entry-put nil "EXPORT_BEAMER_THEME" "Montpellier")
	(org-entry-put nil "EXPORT_BEAMER_COLOR_THEME" "orchid")
	(org-entry-put nil "EXPORT_LaTeX_HEADER" "\\usepackage{appendixnumberbeamer}")
	;; NB: duplicate checks are not done for the following 4 lines
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\usepackage{libertineotf}")
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\setbeamertemplate{navigation symbols}{}")
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\setbeamertemplate{footline}[page number]")
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\institute[Nikhef]{FOM-Nikhef, Amsterdam}")
	(org-entry-put nil "EXPORT_AUTHOR" user-full-name)
	(org-entry-put nil "EXPORT_DATE" "\\today")
	(org-entry-put nil "EXPORT_OPTIONS" "H:1 ^:t")
	(when org-e-beamer-column-view-format
	  (org-entry-put nil "COLUMNS" org-e-beamer-column-view-format))
	(org-entry-put nil "BEAMER_col_ALL" org-e-beamer-column-widths))
    (insert "#+LaTeX_CLASS: beamer\n")
    (insert "#+LaTeX_CLASS_OPTIONS: [presentation,smaller]\n")
    (insert "#+BEAMER_THEME: Montpellier\n")
    (insert "#+BEAMER_COLOR_THEME: orchid\n")
    (insert "#+LaTeX_HEADER: \\usepackage{appendixnumberbeamer}\n")
    (insert "#+LaTeX_HEADER: \\usepackage{libertineotf}\n")
    (insert "#+LaTeX_HEADER: \\setbeamertemplate{navigation symbols}{}\n")
    (insert "#+LaTeX_HEADER: \\setbeamertemplate{footline}[page number]\n")
    (insert "#+LaTeX_HEADER: \\institute[Nikhef]{FOM-Nikhef, Amsterdam}\n")
    (when org-e-beamer-column-view-format
      (insert "#+COLUMNS: " org-e-beamer-column-view-format "\n"))
    (insert "#+PROPERTY: BEAMER_col_ALL " org-e-beamer-column-widths "\n")))


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


;; Modify emacs frame opacity. Works with compositing capable window
;; managers.
;; Source: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun sa-opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 5% steps."
  (interactive "p")
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil
		     100))
	 (newalpha (if dec (- oldalpha 5)
		     (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit)
	       (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commonly used HEP utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defun sa-make-GRL (beg end)
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


(provide 'nifty)

;;; end of nifty.el
