;;; nifty --- nifty lisp functions for convenience.

;;; Commentary:
;; This file contains some nifty Lisp functions I wrote for my
;; convenience or I got it from somewhere (credited).

;;; Code:
(require 'org)
;; (require 'notmuch)

;;; Navigation utilities
;; isearch wrappers to search in other window
;; Source: http://www.unixuser.org/~ysjj/emacs/lisp/misc-funcs.el
(defun sa-isearch-backward-other-window (arg)
  "Incrementally search backward the ARG'th window of this frame."
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-backward)
    (other-window (- arg))))

(defun sa-isearch-forward-other-window (arg)
  "Do incremental search forward the ARG'th window of this frame."
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-forward)
    (other-window (- arg))))

(defun sa-isearch-backward-regexp-other-window (arg)
  "Incrementally search regex backward the ARG'th window of this frame."
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-backward-regexp)
    (other-window (- arg))))

(defun sa-isearch-forward-regexp-other-window (arg)
  "Incrementally search regex forward the ARG'th window of this frame."
  (interactive "p")
  (save-excursion
    (other-window arg)
    (isearch-forward-regexp)
    (other-window (- arg))))

;;; buffer utils
;; FIXME: add uniquify support
;; (uniquify-item-buffer
;;  (uniquify-make-item (buffer-name) default-directory indirect-buffer))
(defun sa-make-indirect-buffer ()
  "Make indirect buffer for the current buffer and switch to it."
  (interactive)
  (let ((ibuf (make-indirect-buffer
	       (current-buffer)
	       (generate-new-buffer-name (buffer-name)) t)))
    (switch-to-buffer ibuf)))

(defun sa-switch-buffers (&optional relposn)
  "Interchange buffers with window `RELPOSN' (default 1)."
  (interactive "^p")
  (let* ((this-buf (window-buffer))
	 (this-win (get-buffer-window this-buf))
	 (target-win (next-window))
	 (target-buf (window-buffer target-win)))
    (save-excursion
      (select-window target-win)
      (switch-to-buffer this-buf nil t)
      (select-window this-win)
      (switch-to-buffer target-buf nil t))
    (select-window this-win)))

(defun sa-vc-wdiff (&optional historic not-urgent)
  "Use `vc-diff' in word diff mode.
`HISTORIC' and `NOT-URGENT' are passed on to `vc-diff'."
  (interactive)
  (let ((vc-git-diff-switches (list "--word-diff")))
    (vc-diff historic not-urgent)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Editing utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Written by Michael Heerdegen, see:
;; (notmuch-show "id:87d1x5lrvw.fsf@web.de")
(defun sa-yank-reset-yank-pointer ()
  "Reset `kill-ring-yank-pointer'."
  (unless (eq last-command #'yank)
    (setq kill-ring-yank-pointer kill-ring)))

(defun sa-yank--before-ad (&rest _args)
  "Before advice function for `yank'.
1. Avoid persistent change of `kill-ring-yank-pointer' after
`yank-pop', and before next kill.
2. For `yank-pop', move yanked text to the beginning of the kill ring."
  (unless (eq kill-ring kill-ring-yank-pointer)
    (let ((last-yank (car kill-ring-yank-pointer)))
      (when last-yank
        (setq kill-ring (cons last-yank (delete last-yank kill-ring)))
        (sa-yank-reset-yank-pointer)))))

(defun sa-yank-pop ()
  "Reset yank pointer after yank."
  (interactive)
  (if (eq last-command 'yank)
      (call-interactively #'yank-pop)
    (rotate-yank-pointer 1)
    (yank)))

;; from the Emacs wiki
(defun unfill-region (beg end)
  "Unfill the region marked by BEG and END."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defvar sa-transpose-lines--last-arg nil
  "Internal variable used by `transpose-lines'.")

(defun sa-transpose-lines (arg)
  "More intuitive `transpose-lines'.

ARG number of lines are \"dragged\" up.  If ARG is -ve, they are
dragged down instead.  The relative cursor position is restored
after the move.

IMO, this is a more natural and graphical way of transposing.
The idea is you want to operate on the current object you are
editing.  The current cursor position is preserved to continue
editing after the operation."
  (interactive "*p")
  ;; Thanks to Jambunathan for the bookmark trick
  (add-text-properties (point) (1+ (point)) '(bookmark t))
  ;; save arg for repeat
  (setq sa-transpose-lines--last-arg arg)
  (if (< arg 0) (forward-line (- 1 arg)))
  (transpose-lines arg)
  (goto-char (1- (previous-single-property-change (point) 'bookmark)))
  (remove-text-properties (point) (1+ (point)) '(bookmark)))

;; just for convenience
(defalias 'sa-transpose-lines-up 'sa-transpose-lines)
(defun sa-transpose-lines-down (&optional arg)
  "Move ARG lines down (including current line)."
  (interactive "*p")
  ;; (message "arg: %s, %s" arg (- (or arg 1)))
  (sa-transpose-lines (- (or arg 1))))

(defun sa-forward-paragraph (&optional arg)
  "If `last-command' was `sa-transpose-lines-down', call it again.
Call `forward-paragraph' otherwise.  ARG is passed on as is."
  (interactive "^p")
  (if (eq last-command 'sa-transpose-lines-down)
      (progn
	(setq this-command 'sa-transpose-lines-down)
	(sa-transpose-lines sa-transpose-lines--last-arg))
    (if (derived-mode-p 'org-mode)
	(org-forward-paragraph)		;ignore arg
      (forward-paragraph arg))))

(defun sa-backward-paragraph (&optional arg)
  "If `last-command' was `sa-transpose-lines-up', call it again.
Call `backward-paragraph' otherwise.  ARG is passed on as is."
  (interactive "^p")
  (if (eq last-command 'sa-transpose-lines-up)
      (progn
	(setq this-command 'sa-transpose-lines-up)
	(sa-transpose-lines sa-transpose-lines--last-arg))	;mandatory arguments
    (if (derived-mode-p 'org-mode)
	(org-backward-paragraph)	;ignore arg
      (backward-paragraph arg))))

(defun sa-search-n-comment (str)
  "Search for string STR and comment the line."
  (interactive "sString: ")
  (let ((repeat t))
    (while repeat
      (search-forward str)
      (comment-region (line-beginning-position) (line-end-position))
      (forward-line)
      (setf repeat (y-or-n-p "Repeat? ")))))


;;;;;;;;;;;;;;;;;;;;;
;; Email utilities ;;
;;;;;;;;;;;;;;;;;;;;;

(defun sa-insert-gmane-link (msgid)
  "Insert gmane http link at point.  Prompts for message id (MSGID)."
  (interactive "sMessage ID: ")
  (insert (format "<http://mid.gmane.org/%s>" msgid)))

(defun sa-insert-gmane-thread-link (msgid)
  "Insert gmane http thread link at point.  Prompts for message id (MSGID)."
  (interactive "sMessage ID: ")
  (insert (format "<http://news.gmane.org/find-root.php?message_id=%%3c%s%%3e>" msgid)))

(defun find-message-at-point (&optional msgid)
  "Find the the message the current buffer is a reply to.

The Message-Id (MSGID) is read from the In-Reply-To or Reply-To
header fields.  It is then displayed by calling `notmuch-show'."
  (interactive "sMessage-Id: ")
  (let ((msgid (if (string= "" msgid)
		   (or (message-fetch-field "in-reply-to")
		       (message-fetch-field "reply-to"))
		 msgid)))
    (notmuch-show (concat "id:" (replace-regexp-in-string "[<>]" "" msgid)))))

(defalias 'fmap 'find-message-at-point)

;;;;;;;;;;;;;;;;;;;
;; Org utilities ;;
;;;;;;;;;;;;;;;;;;;

(defun sa-multi-occur-files (files regexp)
  "Run `multi-occur' on FILES looking for REGEXP."
  (multi-occur
   (mapcar (lambda (x)
	     (with-current-buffer
		 (or (get-file-buffer x) (find-file-noselect x))
	       (widen)
	       (current-buffer)))
	   files)
   regexp))

(defun sa-org-find-notes (&optional regexp)
  "Run occur on ORG files in the present directory, prompt for REGEXP."
  (interactive "MRegexp: ")
  (let* ((files (directory-files default-directory t "^[^.#].*\.org"))
	 (regexp (or (unless (equal regexp "") regexp) "{{{\\(todo\\|note\\|mark\\)(")))
    (print files)
    (sa-multi-occur-files files regexp)))

;;; [[file:~/org/worg/org-hacks.org::#field-same-row-or-column][Table cell functions]]

(defun sa-org-table-cell-to-left ()
  "Move current field in row to the left."
  (interactive)
  (sa-org-table-cell-transpose-horizontal 'left))

(defun sa-org-table-cell-to-right ()
  "Move current field in row to the right."
  (interactive)
  (sa-org-table-cell-transpose-horizontal nil))

(defun sa-org-table-cell-transpose-horizontal (&optional left)
  "Move current field in row to the right.

  With arg LEFT, move to the left.  For repeated invocation the
  point follows the moved field.  Does not fix formulas."
  ;; Derived from `org-table-move-column'
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let* ((col (org-table-current-column))
	 (col1 (if left (1- col) col))
	 ;; Current cursor position
	 (colpos (if left (1- col) (1+ col))))
    (if (and left (= col 1))
	(error "Cannot move column further left"))
    (if (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
	(error "Cannot move column further right"))
    (org-table-goto-column col1 t)
    (and (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
	 (replace-match "|\\2|\\1|"))
    (org-table-goto-column colpos)
    (org-table-align)))

(defun sa-org-table-rotate-rest-of-row-left ()
  "Rotate rest of row to the left."
  (interactive)
  (sa-org-table-rotate-rest-of-row 'left))

(defun sa-org-table-rotate-rest-of-row-right ()
  "Rotate rest of row to the right."
  (interactive)
  (sa-org-table-rotate-rest-of-row nil))

(defun sa-org-table-rotate-rest-of-row (&optional left)
  "Rotate rest of row to the right.

  With arg LEFT, rotate to the left.  For both directions the
  boundaries of the rotation range are the current field and the
  field at the end of the row.  For repeated invocation the point
  stays on the original current field.  Does not fix formulas."
  ;; Derived from `org-table-move-column'
  (interactive "P")
  (if (not (org-at-table-p))
      (error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field)
  (let ((col (org-table-current-column)))
    (org-table-goto-column col t)
    (and (looking-at (if left
			 "|\\([^|\n]+\\)|\\([^\n]+\\)|$"
		       "|\\([^\n]+\\)|\\([^|\n]+\\)|$"))
	 (replace-match "|\\2|\\1|"))
    (org-table-goto-column col)
    (org-table-align)))

(defun sa-org-table-open-cell-horizontal ()
  "Open field in row, move fields to the right by growing table."
  (interactive)
  (insert "|")
  (backward-char)
  (org-table-align))

(defun sa-org-table-open-cell-vertical ()
  "Open field in column, move all fields downwards by growing table."
  (interactive)
  (let ((col (org-table-current-column))
	(p   (point)))
    ;; Cut all fields downwards in same column
    (goto-char (org-table-end))
    (forward-line -1)
    (while (org-at-table-hline-p) (forward-line -1))
    (org-table-goto-column col)
    (org-table-cut-region p (point))
    ;; Paste at one field below
    (goto-char p)
    (forward-line)
    (org-table-goto-column col)
    (org-table-paste-rectangle)
    (goto-char p)
    (org-table-align)))

(defun sa-tbl-export (name)
  "Search for table named `NAME' and export."
  (interactive "sTable name: ")
  (outline-show-all)
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
    (progn
      (forward-line)
      (org-table-export (format "%s.csv" name) "orgtbl-to-csv")))))

;; Original: Liam Healy on the org-mode mailing list; modified by me
;; <http://mid.gmane.org/CADe9tL7xL8Oci9k4BsiOs_sH3b2N4ormAojDwJ1smF8J3yZGLA@mail.gmail.com>
(defun sa-org-datetree-goto-date (arg)
  "Go to and show (or create) the date in the date tree.

Create `ARG' consecutive dates.  `ARG' is the numeric value of
the prefix argument."
  (interactive "p")
  (let* ((date (decode-time (org-read-date nil t)))
	 (date-next (decoded-time-add date (make-decoded-time :day (- arg 1)))))
    (while (or (time-less-p (encode-time date) (encode-time date-next))
	       (time-equal-p (encode-time date) (encode-time date-next)))
      (org-datetree-find-date-create (list (nth 4 date) (nth 3 date)
					   (nth 5 date)))
      ;; FIXME: insert subheading only on creation
      (org-insert-subheading '(4))	;prefix argument to force next heading
      (insert "Hours")
      (org-set-tags "hrs")
      (setf date (decoded-time-add date (make-decoded-time :day 1)))))
  (outline-show-heading)
  (outline-show-subtree)
  (beginning-of-line))

;; Function to add custom based on headline
(defun sa-org-hl-to-custom-id ()
  "Generate and add custom id from headline text.

Ignores special characters except `_' and `-', and converts
spaces to `-'."
  (interactive)
  (let ((head (downcase (nth 4 (org-heading-components)))))
    (save-match-data
      (while (or (string-match " " head)
		 (string-match "[^a-zA-Z0-9_-]\+" head))
	(if (equal " " (match-string 0 head))
	    (set 'head (replace-match "-" nil t head))
	  (set 'head (replace-match "" nil t head)))))
    (org-set-property "CUSTOM_ID" head)))

;; Function to add duplicate org-mode properties
(defun sa-org-entry-put-dupe (pom property value)
  "At point-or-marker POM, set the PROPERTY to VALUE.

Original function: `org-entry-put'.

NB: Allows duplicate properties.  In case of duplicates, they are inserted
before the existing entry.  Use with caution."
  (org-with-point-at pom
    (org-back-to-heading t)
    (let ((beg (point)) (end (save-excursion (outline-next-heading) (point)))
	  range)
      (let ((buffer-invisibility-spec (org-inhibit-invisibility))) ; Emacs 21
	(setq range (org-get-property-block beg end))
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
(defun sa-org-beamer-insert-options-template (&optional kind)
  "Insert a settings template, to make sure users do this right.

KIND is the type of template to insert.

Original function: `org-beamer-insert-options-template'.

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
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\usepackage{libertine}")
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\setbeamertemplate{navigation symbols}{}")
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\setbeamertemplate{footline}[page number]")
	(sa-org-entry-put-dupe nil "EXPORT_LaTeX_HEADER+" "\\institute[Nikhef]{FOM-Nikhef, Amsterdam}")
	(org-entry-put nil "EXPORT_AUTHOR" user-full-name)
	(org-entry-put nil "EXPORT_DATE" (format-time-string "%d %B, %Y"))
	(org-entry-put nil "EXPORT_OPTIONS" "H:1 ^:t")
	(when org-beamer-column-view-format
	  (org-entry-put nil "COLUMNS" org-beamer-column-view-format))
	(org-entry-put nil "BEAMER_col_ALL" org-beamer-column-widths))
    (insert "#+LaTeX_CLASS: beamer\n")
    (insert "#+LaTeX_CLASS_OPTIONS: [presentation,smaller]\n")
    (insert "#+BEAMER_THEME: Montpellier\n")
    (insert "#+BEAMER_COLOR_THEME: orchid\n")
    (insert "#+LaTeX_HEADER: \\usepackage{appendixnumberbeamer}\n")
    (insert "#+LaTeX_HEADER: \\usepackage{libertineotf}\n")
    (insert "#+LaTeX_HEADER: \\setbeamertemplate{navigation symbols}{}\n")
    (insert "#+LaTeX_HEADER: \\setbeamertemplate{footline}[page number]\n")
    (insert "#+LaTeX_HEADER: \\institute[Nikhef]{FOM-Nikhef, Amsterdam}\n")
    (when org-beamer-column-view-format
      (insert "#+COLUMNS: " org-beamer-column-view-format "\n"))
    (insert "#+PROPERTY: BEAMER_col_ALL " org-beamer-column-widths "\n")))

;; recursively find .org files in provided directory
;; modified from an Emacs Lisp Intro example
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
	 (case-fold-search t)	      ; filesystems are case sensitive
	 (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
	 (filext (or filext "org$\\\|org_archive"))
	 (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
	 (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
	(if (string-match fileregex file-or-dir) ; org files
	    (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
	(dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
			  org-file-list) ; add files found to result
	  (add-to-list 'org-file-list org-file)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python tweaks & utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; special beginning/end of statement navigation commands that
;; switches between beginning/end of statement and beginning/end of
;; line.  Inspired by org-special-ctrl-a/e.
(defun sa-python-nav-beginning-of-statement-special ()
  "Move to start of current statement or beginning of line."
  (interactive)
  (let ((bos (save-excursion (python-nav-beginning-of-statement)
			     (point))))
    (if (or (bolp) (> (point) bos))
	(python-nav-beginning-of-statement)
      (beginning-of-line))))

(defun sa-python-nav-end-of-statement-special ()
  "Move to end of current statement or end of line."
  (interactive)
  (let ((pt (point))
	(eos (save-excursion (python-nav-end-of-statement) (point)))
	(mls (> (save-excursion (beginning-of-line) (point))
		(save-excursion (python-nav-beginning-of-statement)
				(point)))))
    (if (< (point) eos)
	(python-nav-end-of-statement)
      (if mls (goto-char pt))
      (end-of-line))))


;;;;;;;;;;;;;;;;
;; GUI tweaks ;;
;;;;;;;;;;;;;;;;

;; Modify emacs frame opacity. Works with compositing capable window
;; managers.
;; Source: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun sa-opacity-modify (&optional dec)
  "Modify the transparency of the Emacs frame.
If DEC is t, decrease the transparency, otherwise increase it in 5% steps."
  (interactive "p")
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil
		     100))
	 (newalpha (if dec (- oldalpha 5)
		     (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit)
	       (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))


;;;;;;;;;;;;;;;;;;;;;
;; Other utilities ;;
;;;;;;;;;;;;;;;;;;;;;

(defun sa-readlink (filename)
  "Run the shell command readlink on FILENAME."
  (string-trim
   (with-temp-buffer
     (shell-command (format "readlink %s" filename) t)
     (buffer-string))))

(defalias 'qc #'quick-calc)

;; calc or count
(defun sa-calc-or-count (&optional arg)
  "Quick calculate by default, count words when mark is active.
When calling `quick-calc', ARG is passed on as is."
  (interactive "P")
  (if mark-active
      (call-interactively 'count-words-region)
    (if arg (quick-calc arg) (quick-calc))))

;; code by David Engster <deng@randomsample.de>
(defun remote-getenv (variable server)
  "Get environment VARIABLE from SERVER via ssh & bash."
  (with-temp-buffer
    ; complex: "env" -> list of tokens to form command.  E.g.: "bash"
    ; "<opt1>" "<optn>" "\"<complex cmd>\"".  Complex command could be
    ; something like: setup_fn;cmd.
    (call-process "/usr/bin/ssh" nil t nil server "env")
    (if (re-search-backward (format "^%s=\\(.*\\)" variable) nil t)
	(match-string 1)
      nil)))


(provide 'nifty)

;;; nifty.el ends here
