;;; org-mode-settings.el --- `org-mode' settings and customisations.

;; since using org-mode in ~/build/org-mode
(require 'org-install)

(load-library "org-inlinetask")
(load-library "org-export")

;; Google weather in agenda
(load-library "google-weather")
(load-library "org-google-weather")

;; read emails with notmuch
(load-library "notmuch")
;; links to notmuch emails in org
(load-library "org-notmuch")

(load-library "org-man")
(load-library "org-occur-goto")
(load-library "org-search-goto")

;; ;; Calendar view for org agenda
;; (require 'calfw)
;; (require 'calfw-org)

;; utilities
(load-library "nifty")

;; temporary solution to latex export issue
(load-library "tex")

;;; Code:
;; `org-mode' variable customisations
;; directory to look for agenda files matching `org-agenda-file-regexp'
(setq org-agenda-files '("~/org")
      ;; List of extra files to be searched by text search commands.
      org-agenda-text-search-extra-files
      (append '(agenda-archives)	; archived agenda files
	      ;; LHCb CKM gamma measurement
	      ;; (find-org-file-recursively "~/org/LHCb-Bs2Dsh")
	      (directory-files "~/org/LHCb-Bs2Dsh" t
			       "^[^.#].*\\.\\(org$\\|org_archive$\\)")
	      ;; other stuff
	      (directory-files "~/org/not-physics" t
			       "^[^.#].*\\.\\(org$\\|org_archive$\\)")
	      )
      ;; open link in same window
      org-link-frame-setup '((gnus . org-gnus-no-new-news)
			     (file . find-file))
      ;; modifying behaviour of C-a/<home> & C-e/<end>
      org-special-ctrl-a/e t
      ;; on links `RET' follows the link
      org-return-follows-link t
      ;; log time for TODO state changes
      org-log-done 'time
      ;; log time on rescheduling and changing deadlines
      org-log-reschedule 'time
      org-log-redeadline 'time
      ;; To put notes inside LOGBOOK drawer
      org-log-into-drawer t
      ;; turn on speed keys for headlines
      org-use-speed-commands t
      ;; fontify code blocks by default
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ;; temporary setting to circumvent bug in texi2dvi
      ;; file a bug report on bugzilla
      ;; debug original value like this
      ;; org-latex-to-pdf-process '("sh -v -x texi2dvi -p -b -c -V %f")
      ;; org-latex-to-pdf-process '("pdflatex -interaction nonstopmode %b"
      ;; 				 "/usr/bin/bibtex %b"
      ;; 				 "pdflatex -interaction nonstopmode %b"
      ;; 				 "pdflatex -interaction nonstopmode %b")
      ;; update TODO cookies recursively
      ;; use property, ":COOKIE_DATA: todo recursive"
      ;; to set this only for a single subtree
      org-hierarchical-todo-statistics nil
      ;; Block parent TODOs if child is not completed
      org-enforce-todo-dependencies t
      org-agenda-current-time-string "- - - NOW! - - -"
      org-agenda-time-grid '((daily today)
			     "----------------"
			     (800 1000 1200 1400 1600 1800 2000))
      org-google-weather-format "%i %c %L, [%l,%h] %s"
      ;; org-goto-interface 'outline-path-completion
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-reverse-note-order t
      ;; Note that using TODO keyword/tags list matches children tasks
      org-stuck-projects '("+LEVEL=2&+SCHEDULED<\"<-1m>\"/!-DONE"
			   nil nil "^\\*\\+ \\+\\(DONE|FIXD|CNCL\\)")
      ;; for utf8 support, commented out because this is deprecated
      ;; recommended solution is to move to luatex or xetex
      ;; org-export-latex-inputenc-alist '(("utf8" . "utf8x"))
      org-beamer-environments-extra
      '(("only"         "O" "\\only%a{%x"            "}")
	("onlyH" 	"H" "\\only%a{%h%x" 	     "}")
	("visible" 	"+" "\\visible%a{%h%x" 	     "}")
	("invisible" 	"-" "\\invisible%a{%h%x"     "}"))
      ;; convert exported odt to pdf with soffice --convert-to pdf
      org-export-odt-preferred-output-format "pdf"
      ;; to circumvent reliance on Apache config, solution by Seb:
      ;; http://thread.gmane.org/gmane.emacs.orgmode/53856/focus=53875
      org-export-html-xml-declaration
      '(("html" . "<!-- <xml version=\"1.0\" encoding=\"utf-8\"> -->"))
      )


;; lowercase easy templates
(setcdr (assoc "s" org-structure-template-alist)
	'("#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))
(setcdr (assoc "e" org-structure-template-alist)
	'("#+begin_example\n?\n#+end_example" "<example>\n?\n</example>"))
(setcdr (assoc "q" org-structure-template-alist)
	'("#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>"))
(setcdr (assoc "v" org-structure-template-alist)
	'("#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>"))
(setcdr (assoc "c" org-structure-template-alist)
	'("#+begin_center\n?\n#+end_center" "<center>\n?\n</center>"))
(setcdr (assoc "l" org-structure-template-alist)
	'("#+begin_latex\n?\n#+end_latex"
         "<literal style=\"latex\">\n?\n</literal>"))
(setcdr (assoc "h" org-structure-template-alist)
	'("#+begin_html\n?\n#+end_html"
         "<literal style=\"html\">\n?\n</literal>"))
(setcdr (assoc "a" org-structure-template-alist)
	'("#+begin_ascii\n?\n#+end_ascii"))


;;; LaTeX export customisations
;; hack for error free latex export with amsmath
;; remove when defaults are changed in the future
(setcar (rassoc '("wasysym" t) org-export-latex-default-packages-alist)
	"nointegrals")
(add-to-list 'org-export-latex-packages-alist '("" "amsmath" t))

;; include todonotes package for latex export of inlinetasks
(add-to-list 'org-export-latex-packages-alist
	     '("backgroundcolor=green!40" "todonotes" nil) t)
(add-to-list 'org-export-latex-packages-alist
	     '("" "makerobust" nil) t)
(add-to-list 'org-export-latex-packages-alist
	     "\\MakeRobustCommand\\begin" t)
(add-to-list 'org-export-latex-packages-alist
	     "\\MakeRobustCommand\\end" t)
(add-to-list 'org-export-latex-packages-alist
	     "\\MakeRobustCommand\\item" t)

;; for code block export with minted.sty and python program pygmentize
(setq org-export-latex-listings 'minted)
(add-to-list 'org-export-latex-packages-alist '("" "minted"))


;;; XeLaTeX customisations
;; remove "inputenc" from default packages as it clashes with xelatex
(setf org-export-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-export-latex-default-packages-alist))
;; the sexp below will also work in this case. But it is not robust as it
;; pops the first element regardless if its a match or not.
;; (pop org-export-latex-default-packages-alist)

(add-to-list 'org-export-latex-packages-alist '("" "xltxtra" t))
;; commented for now as preferable to set per file for now
;; (add-to-list 'org-export-latex-packages-alist '("" "unicode-math" t))
;; (add-to-list 'org-export-latex-packages-alist
;; 	     "\\setmathfont{Linux Libertine}" t) ; needed for unicode-math

;; org to latex customisations, -shell-escape needed for minted
(setq org-latex-to-pdf-process		; for regular export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
      org-e-latex-pdf-process		; for experimental org-export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; FIXME: export templates for inline tasks
;; (defun sa-org-latex-format-inlinetask (heading content
;; 					    &optional todo priority tags)
;;   "Generate format string for inlinetask export templates for latex."
;;   (let ((color (cond ((string-match "QnA" tags)  "color=blue!40")
;; 		     ((string-match "Qn" tags) "color=yellow!40")
;; 		     (t ""))))
;;     (concat (format "\\todo[inline,%s]{" color)
;; 	    (unless (eq todo "")
;; 	      (format "\\textsc{%s%s}" todo priority))
;; 	    (format "\\textbf{%s}\n" heading)
;; 	    content "}")))

(defun sa-org-latex-format-inlinetask (heading content
					    &optional todo priority tags)
  "Generate format string for inlinetask export templates for latex."
  (concat "\\todo[inline,]{"
	  (unless (eq todo "") (format "\\textsc{%s%s}" todo priority))
	  (format "\\textbf{%s}\n" heading) content "}"))

(setcdr (assoc 'latex org-inlinetask-export-templates)
	'("%s" '((sa-org-latex-format-inlinetask
		  heading content todo priority tags))))


;; FIXME: interferes with ASCII export of subtree
;; ;; org export hooks
;; (defun sa-org-export-latex-wrap-todo ()
;;   "Wrap heading with arbitrary latex environment."
;;   (interactive)
;;   (let* ((tags (org-get-tags-string))
;; 	 (heading (org-get-heading t))	; heading with todo
;; 	 (content (org-get-entry))
;; 	 (color (cond ((string-match ":QnA:" tags)  "color=blue!40")
;; 		      ((string-match ":Qn:" tags) "color=yellow!40"))))
;;     (when color
;;       (org-mark-subtree)
;;       (delete-region (region-beginning) (region-end))
;;       (insert (concat
;; 	       (format "\\todo[inline,%s]{\\textbf{%s}\\protect\\linebreak{}%%\n"
;; 		       color heading)
;; 	       (format "%s\n}%%\n" content))))))

;; ;; FIXME: doesn't export markup like /italics/ or *bold* and links properly
;; (add-hook 'org-export-preprocess-hook
;; 	  (lambda ()
;; 	    (let ((match "QnA|Qn"))
;; 	      (org-map-entries (lambda () (sa-org-export-latex-wrap-todo))
;; 			       match))))

;; ;; FIXME: doesn't work with tags:nil
;; (add-hook 'org-export-preprocess-after-blockquote-hook
;; 	  (lambda ()
;; 	    (let ((match "QnA|Qn"))
;; 	      (org-map-entries (lambda () (sa-org-export-latex-wrap-todo))
;; 			       match))))

;; ;; then generalise it
;; (defun sa-org-export-latex-wrap-env () ;envb enve &opt envargs)
;;   "Wrap heading with arbitrary latex environment."
;;   (interactive)
;;   ;; (unless env
;;   ;;   (setq envb (org-entry-get (point) "LATEX_envb")) ; use as (org-entry-get nil "SOME_PROPERTY")
;;   ;;   (setq enve (org-entry-get (point) "LATEX_enve")))
;;   ;; (unless envargs
;;   ;;   (setq envargs (org-entry-get (point) "LATEX_envargs")))
;;   (let ((env (org-entry-get (point) "LATEX_env"))
;; 	(envargs (org-entry-get (point) "LATEX_envargs"))
;; 	(tags (org-get-tags))
;; 	(heading (org-get-heading t))	; heading with todo
;; 	(content (org-get-entry)))
;;     (org-mark-subtree)
;;     (delete-region (region-beginning) (region-end))
;;     (insert (concat env "[inline, color=" envargs "]{%\n"
;; 		    "\\textbf{" heading "}\n"
;; 		    content "\n}%\n"))))

;; backend aware export preprocess hook
(defun sa-org-export-preprocess-hook ()
  "My backend aware export preprocess hook."
  (save-excursion
    (when (eq org-export-current-backend 'latex)
      ;; ignoreheading tag for bibliographies and appendices
      (let* ((tag "ignoreheading"))
	;; (goto-char (point-min))
	;; (while (re-search-forward (concat ":" tag ":") nil t)
	;;   (delete-region (point-at-bol) (point-at-eol)))
	(org-map-entries (lambda ()
			   (delete-region (point-at-bol) (point-at-eol)))
			 (concat ":" tag ":"))))
    (when (eq org-export-current-backend 'html)
      ;; set custom css style class based on matched tag
      (let* ((match "Qn"))
	(org-map-entries
	 (lambda () (org-set-property "HTML_CONTAINER_CLASS" "inlinetask"))
	 match)))))
(add-hook 'org-export-preprocess-hook 'sa-org-export-preprocess-hook)

(setq org-agenda-skip-function-global ; skip END entries in inline tasks
      (lambda ()
	(when (and (featurep 'org-inlinetask)
		   (let ((case-fold-search t))
		     (org-looking-at-p (concat (org-inlinetask-outline-regexp)
					       "end[ \t]*$"))))
	  (or (save-excursion (outline-next-heading)) (point-max)))))

;; Export tags search result to a temporary buffer
(defun sa-org-tags-search-to-buffer(match)
  "Do a tags search and copy the results to the temporary buffer
\"*temp*\"."
  (interactive "sSearch for: " )
  (let* ((agenda-files (org-agenda-files t)))
    (switch-to-buffer "*temp*")
    (org-mode)
    (dolist (buf agenda-files)
      (save-excursion
	(find-file buf)
	(org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match)))
	(beginning-of-buffer)
	(while (condition-case nil (org-occur-next-match 1) (error nil))
	  (if (org-inlinetask-at-task-p) (org-copy-subtree 2)
	    (org-copy-subtree)) (kill-append "\n" nil)
	    (append-next-kill))))
    (switch-to-buffer "*temp*") (goto-char (point-max)) (yank)))

;; (defun sa-org-export-final-hook ()
;;   "My backend specific final export hook."
;;   (save-excursion
;;     (if (eq org-export-current-backend 'latex)
;; 	;; (if (eq buffer-file-coding-system 'utf-8-unix))
;; 	(progn (re-search-forward "inputenc")
;; 	       (message "hook")
;; 	       (beginning-of-line)
;; 	       (insert "% ")))))
;; (add-hook 'org-export-latex-final-hook 'sa-org-export-final-hook t)


;; show links as inline images using `iimage-mode'
(load-library "iimage")
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]")  1))


;; reftex setup
(defun sa-setup-reftex ()
  "Load and setup `reftex'."
  (interactive)
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
  ;; (when (and (not (featurep 'reftex))
  ;; 	     (buffer-file-name)
  ;; 	     (file-exists-p (buffer-file-name)))
  ;;   (reftex-mode)
  ;;   (reftex-parse-all))


;;; TODO keywords
;; @ - time stamp with note
;; ! - only time stamp
(setq org-todo-keywords
      '((sequence "TODO(t)" "WInP(w!)" "DLAY(l@/!)" "|" "DONE(d@)" "CNCL(c@/!)")
	(type "TEST(e!)" "DBUG(b@)" "LEAK(l@)" "SEGF(s@)" "|" "FIXD(f@/!)")
	))


;;; Custom agenda commands
(setq org-agenda-custom-commands
      '(("F" "Future meetings"
	 tags "CATEGORY=\"meetings\"+TIMESTAMP>=\"<today>\"")
	("B" "Search Bs → Dsh files" search ""
	 ((org-agenda-files (list "~/org/analysis.org" "~/org/meetings.org"))
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/LHCb-Bs2Dsh"))))
	("V" "Search Velo files" search ""
	 ((org-agenda-files (list "~/org/analysis.org" "~/org/meetings.org"))
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/LHCb-Velo"))))
	;; ("g" "Search CKM γ project notes" search "")
	("p" . "Pending tasks")
	("pk" "Dated pending tasks"
	 tags-todo "TIMESTAMP<\"<today>\"-TODO={DONE\\|CNCL}"
	 ((org-agenda-overriding-header "Pending tasks")))
	("pl" "Pending entries in thesis timeline"
	 tags "CATEGORY=\"thesis\"+SCHEDULED<=\"<today>\"-TODO={DONE\\|CNCL}"
	 ((org-agenda-overriding-header "Thesis future timeline")
	  (org-agenda-sorting-strategy '(time-up))))
	("pd" "Upcoming thesis deadlines"
	 tags "CATEGORY=\"thesis\"+DEADLINE<\"<+1m>\"-TODO={DONE\\|CNCL}"
	 ((org-agenda-overriding-header "Thesis deadlines")
	  (org-agenda-sorting-strategy '(time-up))))
	("po" "Other pending thesis entries"
	 tags "CATEGORY=\"thesis\"+TIMESTAMP<=\"<today>\""
	 ((org-agenda-overriding-header "Thesis pointers")
	  (org-agenda-sorting-strategy '(time-up))))
	;; ("R" "Search any arbitrary directory" search ""
	;;  ((org-agenda-files nil)
	;;   (org-agenda-text-search-extra-files
	;;    (find-org-file-recursively))))
	("W" . "Search Worg")
	("Wa" "Search all articles" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/Worg"))))
	("Wb" "Search babel articles" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/Worg/org-contrib/babel"))))
	("Wc" "Search orgmode configuration" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/Worg/org-configs"))))
	("Wp" "Search articles on contrib packages" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/Worg/org-contrib"))))
	("Wt" "Search tutorials" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/Worg/org-tutorials"))))
	("A" "Search ATLAS files" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (find-org-file-recursively "~/org/ATLAS-wprime"))))
	;; ("E" . "Search and export to temporary buffer")
	;; ("Et" "Export tags search result to buffer" org-tags-search-to-buffer "Qn")
	))


;;; `org-capture' templates
(setq org-capture-templates
      '(("m" "Select meeting templates")
	("mr" "Regular appointment/meeting" entry (file+headline "~/org/meetings.org" "Appointments and meetings")
	 "*** %? %^t%^{CATEGORY}p\n"
	 :prepend t)
	("mb" "Create Bfys meeting" entry (file+headline "~/org/meetings.org" "Bfys meetings")
	 "*** %? %^t\n"
	 :prepend t)
	("ma" "Create analysis meeting" entry (file+headline "~/org/meetings.org" "Analysis meetings")
	 "*** %? analysis meeting %^t %^G\n"
	 :prepend t)
	("mm" "Meeting minutes w/ clock" entry (file+datetree "~/org/meetings.org")
	 "**** %^{prompt} %U %^G\n\n%?"
	 :clock-in t)
	("mn" "Meeting notes w/o clock" entry (file+datetree "~/org/meetings.org")
	 "**** %^{prompt} %U %^G\n%?")
	("l" "Talks and lectures" entry (file+headline "~/org/meetings.org" "Colloquia")
	 "*** %^{prompt} %^t\n%^L"
	 :prepend t :immediate-finish t)
	("w" "Workshops and conferences" entry (file+headline "~/org/meetings.org" "Workshops - Conferences")
	 "** %^{prompt}%^{CATEGORY}p\n   %^t--%^t\n%?"
	 :prepend t :empty-lines 1)
	("d" "Add TODO with a DEADLINE" entry (file+headline "~/org/notes.org" "Tasks")
	 "** TODO %^{prompt}\n   DEADLINE: %^t\n%?"
	 :prepend t)
	("n" "Notes" entry (file+headline "~/org/notes.org" "Notes")
	 "** %^{prompt}%^{CATEGORY}p\n\n%?"
	 :prepend t)
	("p" "Schedule a trip" entry (file+headline "~/org/notes.org" "Trips")
	 "** %^{prompt}%^{CATEGORY}p\n   %^t--%^t\n%?"
	 :prepend t)
	("r" "Reading material" entry (file+headline "~/org/notes.org" "Reading")
	 "** %?%^{CATEGORY}p %^G\n   %^t"
	 :prepend t) ; :unnarrowed t
	("s" "Schedule a TODO Item" entry (file+headline "~/org/notes.org" "Tasks")
	 "** TODO %^{prompt}\n   SCHEDULED: %^t\n%?"
	 :prepend t)
	("t" "Regular TODO Item" entry (file+headline "~/org/notes.org" "Tasks")
	 "** TODO %^{prompt}\n   %^t\n%?"
	 :prepend t)
	))


;; compatibility with session.el
(add-hook 'session-after-jump-to-last-change-hook
	  (lambda ()
	    (when (and (eq major-mode 'org-mode)
		       (outline-invisible-p))
	      (org-reveal))))


;; org-mode navigation commands
(defun sa-outline-forward-same-level (arg)
  "If its the last outline sibling, move to the next visible outline
heading."
  (interactive "p")
  (if (save-excursion (outline-get-next-sibling))
      (outline-forward-same-level arg)
    (outline-next-visible-heading arg)))

(defun sa-outline-backward-same-level (arg)
  "If its the last outline sibling, move to the previous visible outline
heading."
  (interactive "p")
  (if (save-excursion (outline-get-last-sibling))
      (outline-backward-same-level arg)
    (outline-previous-visible-heading arg)))

(defun sa-org-dwim-next()
  "Move to next item or headline. If at an item move to the next item
otherwise move to next headline."
  (interactive)
  (unless (eq major-mode 'org-mode) (error "Not an `org-mode' buffer"))
  (if (org-in-item-p)
      (if (eq t (condition-case nil (org-next-item)
                  (error t)))
	  (outline-next-visible-heading 1))
    (outline-next-visible-heading 1)))

(defun sa-org-dwim-previous()
  "Move to next item or headline. If at an item move to the next item
otherwise move to next headline."
  (interactive)
  (unless (eq major-mode 'org-mode) (error "Not an `org-mode' buffer"))
  (if (org-in-item-p)
      (if (eq t (condition-case nil (org-previous-item)
	          (error t)))
	  (outline-previous-visible-heading 1))
    (outline-previous-visible-heading 1)))


;;; Keymaps:
;; To specify key combinations use,
;; + Quoted lisp vectors - '[(C-right)]
;; + String translated to keyboard events - (kbd "C-<right>")

;; Testing org-element
(defalias 'org-metaup 'org-element-drag-backward)
(defalias 'org-metadown 'org-element-drag-forward)

;; global keymaps
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link-global)
(global-set-key (kbd "C-c C-o") 'org-open-at-point-global)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; `org-mode' keymaps
(defun sa-org-mode-keymap()
  "My `org-mode' keymap."
  ;; ;; overload default `fill-paragraph' keybind to use org specific command
  ;; (org-defkey org-mode-map (kbd "M-q") 'org-fill-paragraph) ; complains with wrong argument type
  ;; toggle inline images with iimage
  ;; (org-defkey org-mode-map (kbd "C-c i") 'org-toggle-inline-images)
  (org-defkey org-mode-map (kbd "C-c g") 'oog) ; org-occur-goto
  (org-defkey org-mode-map (kbd "C-c s") 'osg) ; org-search-goto
  ;; table copy paste
  (org-defkey org-mode-map (kbd "C-M-w") 'org-table-copy-region)
  (org-defkey org-mode-map (kbd "C-M-y") 'org-table-paste-rectangle)
  ;; navigating list items / headings depending on context
  (org-defkey org-mode-map (kbd "<XF86Forward>") 'sa-org-dwim-next)
  (org-defkey org-mode-map (kbd "<XF86Back>") 'sa-org-dwim-previous)
  ;; navigating links
  (org-defkey org-mode-map (kbd "C-c <XF86Forward>") 'org-next-link)
  (org-defkey org-mode-map (kbd "C-c <XF86Back>") 'org-previous-link)
  ;; navigating headlines
  (org-defkey org-mode-map (kbd "C-c <left>") 'outline-up-heading)
  (org-defkey org-mode-map (kbd "C-c <up>") 'outline-previous-visible-heading)
  (org-defkey org-mode-map (kbd "C-c <down>") 'outline-next-visible-heading)
  ;; super / windows key may not work on laptops
  (org-defkey org-mode-map (kbd "C-<XF86Back>") 'sa-outline-backward-same-level)
  (org-defkey org-mode-map (kbd "C-<XF86Forward>") 'sa-outline-forward-same-level))

;; `org-agenda-mode' keymaps
(defun sa-org-agenda-mode-keymap()
  "My `org-agenda-mode' keymap."
  ;; set property
  (org-defkey org-agenda-mode-map (kbd "C-c p") 'org-agenda-set-property)
  ;; month view
  (org-defkey org-agenda-mode-map (kbd "C-c m") 'org-agenda-month-view))


;; org-agenda config; not needed anymore, but kept as an example
;; This function is used to insert current time in the agenda buffer
;; Thanks to Julien Danjou
;; (defun jd:org-current-time ()
;;   "Return current-time if date is today."
;;   (when (equal date (calendar-current-date))
;;     (propertize (format-time-string "%H:%M Current time") 'face
;; 		'(:weight bold :foreground "DodgerBlue4" :background "snow"))))


;;; Hooks
;; `org-agenda-mode' hook
(defun sa-org-agenda-mode-hook()
  "My `org-agenda-mode' hook."
  (sa-org-agenda-mode-keymap)
  (visual-line-mode t))
(add-hook 'org-agenda-mode-hook 'sa-org-agenda-mode-hook)

;; `org-mode' hook
(defun sa-org-mode-hook()
  "My `org-mode' hook."
  (local-unset-key (kbd "C-c ["))	; add/remove agenda files
  (local-unset-key (kbd "C-c ]"))
  ;; (flyspell-mode t)
  (sa-org-mode-keymap)
  ;; line folding w/o actually folding it, use `M-q' to wrap.
  (visual-line-mode t)
  ;; imenu for org-mode buffers
  (imenu-add-to-menubar "Headlines")
  ;; dynamic abbreviations for org-mode
  (setq local-abbrev-table text-mode-abbrev-table))
(add-hook 'org-mode-hook 'sa-org-mode-hook)

;; Make windmove work in org-mode with 'shift as modifier:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; Setup `org-babel' for emacs-lisp, gnuplot, latex and shell-script.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((calc . t)
   (ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (octave .t)
   (python . t)
   (ruby . t)
   (R . t)
   (ruby . t)
   (sh . t)))


;;; org-mode-settings.el ends here
