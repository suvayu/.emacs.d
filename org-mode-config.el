;;; org-mode-config.el --- `org-mode' settings and customisations.

;; (require 'org-inlinetask)
(require 'org)
(require 'ox)
(require 'ox-ascii)
(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-html)
;; (require 'ox-odt)
;; (require 'ox-groff)
;; (require 'ox-man)   ; NB: customise org-man-pdf-process
;; (require 'ox-texinfo)
;; (require 'ox-publish)

;; ;; Google weather in agenda
;; (load-library "google-weather")
;; (load-library "org-google-weather")

;; links to notmuch emails in org
(require 'org-notmuch)

;; (require 'org-man)
(require 'org-occur-goto)
(require 'org-search-goto)

;; ;; Calendar view for org agenda
;; (require 'calfw)
;; (require 'calfw-org)

;; utilities
(require 'nifty)

;;; Code:
;; `org-mode' variable customisations
;; directory to look for agenda files matching `org-agenda-file-regexp'
(setq org-agenda-files '("~/org")
      ;; List of extra files to be searched by text search commands.
      org-agenda-text-search-extra-files
      (append '(agenda-archives)	; archived agenda files
	      (sa-find-org-file-recursively "~/org/HEP" "org")
	      (sa-find-org-file-recursively "~/org/LHCb-Bs2Dsh" "org")
	      (sa-find-org-file-recursively "~/org/LHCb-Velo" "org")
	      ;; (directory-files "~/org/HEP" t
	      ;; 		       "^[^.#].*\\.\\(org$\\|org_archive$\\)")
	      ;; (directory-files "~/org/LHCb-Bs2Dsh" t
	      ;; 		       "^[^.#].*\\.\\(org$\\|org_archive$\\)")
	      ;; (directory-files "~/org/LHCb-Velo" t
	      ;; 		       "^[^.#].*\\.\\(org$\\|org_archive$\\)")
	      )
      ;; open link in same window
      org-link-frame-setup '((gnus . org-gnus-no-new-news)
			     (file . find-file))
      ;; modifying behaviour of C-a/<home> & C-e/<end>
      org-special-ctrl-a/e t
      ;; on links `RET' follows the link
      org-return-follows-link t
      ;; custom links
      org-link-abbrev-alist
      '(("gmane" . "http://thread.gmane.org/%s")
	("google" . "https://www.google.com/search?q=%s")
	("arxiv" . "http://arxiv.org/abs/%s")
	("cds" . "https://cds.cern.ch/record/%s")
	("doi" . "http://dx.doi.org/%s"))
      ;; log time for TODO state changes
      org-log-done 'time
      ;; log time on rescheduling and changing deadlines
      org-log-reschedule 'time
      org-log-redeadline 'time
      ;; To put notes inside LOGBOOK drawer
      org-log-into-drawer t
      ;; turn on speed keys for headlines
      org-use-speed-commands t
      ;; reveal folded headline when trying to edit
      org-catch-invisible-edits 'show
      ;; fontify code blocks by default
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ;; temporary setting to circumvent bug in texi2dvi
      ;; file a bug report on bugzilla
      ;; debug original value like this
      ;; org-latex-pdf-process '("sh -v -x texi2dvi -p -b -c -V %f")
      ;; org-latex-pdf-process '("pdflatex -interaction nonstopmode %b"
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
      ;; google-weather, no more
      ;; org-google-weather-format "%i %c %L, [%l,%h] %s"
      ;; org-goto-interface 'outline-path-completion
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-reverse-note-order t
      ;; Note that using TODO keyword/tags list matches children tasks
      org-stuck-projects '("+LEVEL=2&+SCHEDULED<\"<-1m>\"/!-DONE"
			   nil nil "^\\*\\+ \\+\\(DONE|FIXD|CNCL\\)")
      ;; using org-export now
      ;; ;; org-export-latex-inputenc-alist '(("utf8" . "utf8x"))
      ;; org-beamer-environments-extra
      ;; '(("only"         "O" "\\only%a{%x"            "}")
      ;; 	("onlyH" 	"H" "\\only%a{%h%x" 	     "}")
      ;; 	("visible" 	"+" "\\visible%a{%h%x" 	     "}")
      ;; 	("invisible" 	"-" "\\invisible%a{%h%x"     "}"))
      ;; convert exported odt to pdf with soffice --convert-to pdf
      org-odt-preferred-output-format "pdf"
      ;; to circumvent reliance on Apache config, solution by Seb:
      ;; http://thread.gmane.org/gmane.emacs.orgmode/53856/focus=53875
      org-html-xml-declaration
      '(("html" . "<!-- <xml version=\"1.0\" encoding=\"utf-8\"> -->"))
      org-entities-user			; "\ " can also be used
      '(("space" "~" nil " " " " " " " "))
      org-latex-remove-logfiles nil
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


;;; ASCII export customisation for the new exporter
;; the markers for Latin is nicer, use for UTF-8 too
(setcdr (assoc 'utf-8 org-ascii-bullets) '(?§ ?¶))

;;; LaTeX export customisations
;; hack for error free latex export with amsmath
;; remove when defaults are changed in the future
(setcar (rassoc '("wasysym" t) org-latex-default-packages-alist)
	"nointegrals")
(add-to-list 'org-latex-packages-alist '("" "amsmath" t))

;; include todonotes package for latex export of inlinetasks
(add-to-list 'org-latex-packages-alist
	     '("backgroundcolor=green!40" "todonotes" nil) t)
(add-to-list 'org-latex-packages-alist
	     '("" "makerobust" nil) t)
(add-to-list 'org-latex-packages-alist
	     "\\MakeRobustCommand\\begin" t)
(add-to-list 'org-latex-packages-alist
	     "\\MakeRobustCommand\\end" t)
(add-to-list 'org-latex-packages-alist
	     "\\MakeRobustCommand\\item" t)

;; FIXME: temporarily commented
;; ;; for code block export with minted.sty and python program pygmentize
;; (setq org-latex-listings 'minted)
;; (add-to-list 'org-latex-packages-alist '("" "minted"))


;;; XeLaTeX customisations
;; remove "inputenc" from default packages as it clashes with xelatex
(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
;; the sexp below will also work in this case. But it is not robust as it
;; pops the first element regardless if its a match or not.
;; (pop org-latex-default-packages-alist)

(add-to-list 'org-latex-packages-alist '("" "xltxtra" t))
;; choose Linux Libertine O as serif and Linux Biolinum O as sans-serif fonts
(add-to-list 'org-latex-packages-alist '("" "libertine" t))
;; commented for now as preferable to set per file for now
;; (add-to-list 'org-latex-packages-alist '("" "unicode-math" t))
;; (add-to-list 'org-latex-packages-alist
;; 	     "\\setmathfont{Linux Libertine}" t) ; needed for unicode-math

;; org to latex customisations, -shell-escape needed for minted
(setq org-export-dispatch-use-expert-ui t ; non-intrusive export dispatch
      org-latex-pdf-process		; for regular export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; export single chapter
(add-to-list 'org-latex-classes
	     '("chapter" "\\documentclass[11pt]{report}"
	       ("\\chapter{%s}" . "\\chapter*{%s}")
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; ;; FIXME: doesn't work because of \hypersetup, \tableofcontents, etc.
;; ;; minimal export with the new exporter (maybe use the standalone class?)
;; (add-to-list 'org-latex-classes
;;              '("minimal"
;;                "\\documentclass\{minimal\}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]"
;;                ("\\section\{%s\}" . "\\section*\{%s\}")
;;                ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
;;                ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; beamer export with the new exporter
(add-to-list 'org-beamer-environments-extra
	     '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))

(add-to-list 'org-beamer-environments-extra
	     '("boldH" "h" "\\textbf{%h}" "%%%%"))

(add-to-list 'org-beamer-environments-extra
	     '("phantom" "P" "\\phantom{%h}" ""))

(add-to-list 'org-export-snippet-translation-alist
	     '("b" . "beamer"))
(add-to-list 'org-export-snippet-translation-alist
	     '("l" . "latex"))
(add-to-list 'org-export-snippet-translation-alist
	     '("h" . "html"))
(add-to-list 'org-export-snippet-translation-alist
	     '("o" . "odt"))

;; filters for markups
(defun sa-beamer-bold (contents backend info)
  (when (org-export-derived-backend-p backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

(add-to-list 'org-export-filter-bold-functions 'sa-beamer-bold)

(defun sa-beamer-structure (contents backend info)
  (when (org-export-derived-backend-p backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\structure" contents)))

(add-to-list 'org-export-filter-strike-through-functions 'sa-beamer-structure)

;; FIXME: using $_{\text{string}}$ looks much better!
;; (defun sa-latex-subscript (contents backend info)
;;   (when (org-export-derived-backend-p backend 'beamer 'latex)
;;     (replace-regexp-in-string "\\$_{\\\\text{\\([^}]+\\)}}\\$"
;; 			      "\\\\textsubscript{\\1}" contents)))

;; (add-to-list 'org-export-filter-subscript-functions 'sa-latex-subscript)

;; (defun sa-latex-superscript (contents backend info)
;;   (when (org-export-derived-backend-p backend 'beamer 'latex)
;;     (replace-regexp-in-string "\\$\\^{\\\\text{\\([^}]+\\)}}\\$"
;; 			      "\\\\textsuperscript{\\1}" contents)))

;; (add-to-list 'org-export-filter-superscript-functions 'sa-latex-superscript)

;; FIXME: implement configurable reference style for latex export
;; (defun sa-latex-reflink (contents backend info)
;;   (when (and (eq (plist-get info :refstyle) t)
;; 	     (org-export-derived-backend-p backend 'latex))
;;     (replace-regexp-in-string "\\`\\\\\\(ref\\){\\([a-zA-Z0-9]+\\):\\([a-zA-Z0-9]+\\)}"
;; 			      "\\\\\\2\\1{\\2:\\3}" contents)))

;; (add-to-list 'org-export-filter-link-functions 'sa-latex-reflink)

;;; not needed any more, here for example purposes
;; ;; smart quotes on only for latex backend (courtesy: Jambunathan)
;; (defun sa-org-latex-options-function (info backend)
;;  (when (eq backend 'latex)
;;   (plist-put info :with-smart-quotes t)))

;; (add-to-list 'org-export-filter-options-functions 'sa-org-latex-options-function)

(defun sa-ignore-headline (contents backend info)
  "Ignore headlines with tag `ignoreheading'."
  (when (and (org-export-derived-backend-p backend 'latex 'html 'ascii)
		  (string-match "\\`.*ignoreheading.*\n"
				(downcase contents)))
    (replace-match "" nil nil contents)))

(add-to-list 'org-export-filter-headline-functions 'sa-ignore-headline)


;; Setup for KOMA script letter with scrlttr2
(add-to-list 'org-latex-classes
               '("scrlttr2"
                 "\\documentclass\[%
DIV=14,
fontsize=12pt,
parskip=half,
subject=titled,
backaddress=false,
fromalign=left,
fromemail=true,
fromphone=true\]\{scrlttr2\}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"
		 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


;; ;; FIXME: just switching binary is not enough,
;; ;; `org-latex-packages-alist' has to be updated
;; (defun sa-switch-latex-binary(binary)
;;   "Switch binary for LaTeX export of org files.  Note this does
;; not correct default package list."
;;   (interactive "sLaTeX binary: ")
;;   (downcase binary)
;;   (let ((newcmd (replace-regexp-in-string
;; 		 ".*latex" binary
;; 		 (car org-latex-pdf-process))))
;;     (setf org-latex-pdf-process `(,newcmd ,newcmd ,newcmd))
;;     (message "Note default packages are unchanged!")))


;; FIXME: export templates for inline tasks
;; This works but disabled
;; (defun sa-org-latex-format-inlinetask (heading content
;; 					    &optional todo priority tags)
;;   "Generate format string for inlinetask export templates for latex."
;;   (concat "\\todo[inline,]{"
;; 	  (unless (eq todo "") (format "\\textsc{%s%s}" todo priority))
;; 	  (format "\\textbf{%s}\n" heading) content "}"))
;;
;; (setcdr (assoc 'latex org-inlinetask-export-templates)
;; 	'("%s" '((sa-org-latex-format-inlinetask
;; 		  heading content todo priority tags))))
;;
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


;; FIXME: Migrate to filters for experimental export
;; ;; FIXME: interferes with ASCII export of subtree
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


;; FIXME: instead of preprocess hook, use filters for new exporter
;; ;; backend aware export preprocess hook
;; (defun sa-org-export-preprocess-hook ()
;;   "My backend aware export preprocess hook."
;;   (save-excursion
;;     (when (eq org-export-current-backend 'latex)
;;       ;; ignoreheading tag for bibliographies and appendices
;;       (let* ((tag "ignoreheading"))
;; 	;; (goto-char (point-min))
;; 	;; (while (re-search-forward (concat ":" tag ":") nil t)
;; 	;;   (delete-region (point-at-bol) (point-at-eol)))
;; 	(org-map-entries (lambda ()
;; 			   (delete-region (point-at-bol) (point-at-eol)))
;; 			 (concat ":" tag ":"))))
;;     (when (eq org-export-current-backend 'html)
;;       ;; set custom css style class based on matched tag
;;       (let* ((match "Qn"))
;; 	(org-map-entries
;; 	 (lambda () (org-set-property "HTML_CONTAINER_CLASS" "inlinetask"))
;; 	 match)))))
;; (add-hook 'org-export-preprocess-hook 'sa-org-export-preprocess-hook)


;; Fix for inlinetasks in agenda
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


;; disable inline images
;; ;; show links as inline images using `iimage-mode'
;; (load-library "iimage")
;; (add-to-list 'iimage-mode-image-regex-alist
;;              (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
;;                            "\\)\\]")  1))


;; reftex setup
(defun sa-setup-reftex ()
  "Load and setup `reftex'."
  (interactive)
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
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
	("H" "Search HEP notes" search ""
	 ((org-agenda-files (list "~/org/research.org" "~/org/meetings.org"))
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/HEP"))))
	("B" "Search Bs → Dsh files" search ""
	 ((org-agenda-files (list "~/org/research.org" "~/org/meetings.org"))
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/LHCb-Bs2Dsh"))))
	("V" "Search Velo files" search ""
	 ((org-agenda-files (list "~/org/research.org" "~/org/meetings.org"))
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/LHCb-Velo"))))
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
	;;  ((org-agenda-files nil)  ; FIXME: depends on interactive fix
	;;   (org-agenda-text-search-extra-files
	;;    (sa-find-org-file-recursively))))
	("W" . "Search Worg")
	("Wa" "Search all articles" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/Worg"))))
	("Wt" "Search exporter documentation" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/Worg/org-tutorials"))))
	("Wc" "Search orgmode configuration" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/Worg/org-configs"))))
	("Wt" "Search tutorials" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/Worg/org-tutorials"))))
	("Wp" "Search articles on contrib packages" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/Worg/org-contrib"))))
	("Wb" "Search babel articles" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/Worg/org-contrib/babel"))))
	("A" "Search ATLAS files" search ""
	 ((org-agenda-files nil)
	  (org-agenda-text-search-extra-files
	   (sa-find-org-file-recursively "~/org/ATLAS-wprime"))))
	;; ("E" . "Search and export to temporary buffer")
	;; ("Et" "Export tags search result to buffer" org-tags-search-to-buffer "Qn")
	))


;;; `org-capture' templates
(setq org-capture-templates
      '(("m" "Select meeting templates")
	("mb" "Create Bfys meeting" entry (file+headline "~/org/meetings.org" "Meetings")
	 "*** %? %^t\n"
	 :prepend t)
	("mm" "Meeting minutes w/ clock" entry (file+datetree "~/org/meetings.org")
	 "**** %^{prompt} %U %^G\n\n%?"
	 :clock-in t)
	("mn" "Meeting notes w/o clock" entry (file+datetree "~/org/meetings.org")
	 "**** %^{prompt} %U %^G\n%?"
	 :immediate-finish t :jump-to-captured t) ; :empty-lines-before 1
	("d" "Add task with a DEADLINE" entry (file+headline "~/org/tasks.org" "Tasks")
	 "** TODO %^{prompt}\n   DEADLINE: %^t\n%?"
	 :prepend t)
	("n" "Notes" entry (file+headline "~/org/tasks.org" "Notes")
	 "** %^{prompt}%^{CATEGORY}p\n\n%?"
	 :prepend t)
	("r" "Reading material" entry (file+headline "~/org/biblio.org" "Reading")
	 "** %?%^{CATEGORY}p %^G\n   %^t"
	 :prepend t) ; :unnarrowed t
	("s" "Schedule a task" entry (file+headline "~/org/tasks.org" "Tasks")
	 "** TODO %^{prompt}\n   SCHEDULED: %^t\n%?"
	 :prepend t)
	("t" "Regular task" entry (file+headline "~/org/tasks.org" "Tasks")
	 "** TODO %^{prompt}\n   %^t\n%?"
	 :prepend t)
	))

;; source: (notmuch-show "id:CAGOCFPUW-wQqU0OdaW5OeM60ZOosCgBO9_mdhyQ77aiH_0UtZQ@mail.gmail.com")
(defun make-org-capture-frame ()
  "Run org-capture with slightly modified behaviour."
  (interactive)
  (cl-flet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)
    (delete-other-windows)))


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

;; global keymaps
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link-global)
(global-set-key (kbd "C-c C-o") 'org-open-at-point-global)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-h o") 'org-info)

;; Make windmove work in org-mode with 'shift as modifier:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; `org-mode' keymaps
;; ;; overload default `fill-paragraph' keybind to use org specific command
;; (org-defkey org-mode-map (kbd "M-q") 'org-fill-paragraph) ; complains with wrong argument type
;; toggle inline images with iimage
;; (org-defkey org-mode-map (kbd "C-c i") 'org-toggle-inline-images)
(org-defkey org-mode-map (kbd "C-c d") 'org-display-outline-path)
(org-defkey org-mode-map (kbd "C-c g") 'oog) ; org-occur-goto
(org-defkey org-mode-map (kbd "C-c s") 'osg) ; org-search-goto
;; table copy paste (originally C-c M-w was bound to `org-copy')
(org-defkey org-mode-map (kbd "C-c M-w") 'org-table-copy-region)
(org-defkey org-mode-map (kbd "C-c M-y") 'org-table-paste-rectangle)
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
(org-defkey org-mode-map (kbd "C-<XF86Forward>") 'sa-outline-forward-same-level)
(org-defkey org-mode-map (kbd "C-M-@") 'mark-end-of-sentence)

;; `org-agenda-mode' keymaps
;; (eval-after-load 'org-agenda
;;   ;; set property
;;   (org-defkey org-agenda-mode-map (kbd "C-c p") 'org-agenda-set-property)
;;   ;; month view
;;   (org-defkey org-agenda-mode-map (kbd "C-c m") 'org-agenda-month-view))


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
  (visual-line-mode t))
(add-hook 'org-agenda-mode-hook 'sa-org-agenda-mode-hook)

;; `org-mode' hook
(defun sa-org-mode-hook()
  "My `org-mode' hook."
  (local-unset-key (kbd "C-c ["))	; add/remove agenda files
  (local-unset-key (kbd "C-c ]"))
  ;; (flyspell-mode t)
  ;; line folding w/o actually folding it, use `M-q' to wrap.
  (visual-line-mode t)
  ;; imenu for org-mode buffers
  ;; Submenu with TODOs
  ;; (add-to-list 'imenu-generic-expression
  ;; 	       `("TODOs"
  ;; 		 ,(concat "^\\(?:[*]+\\) +"
  ;; 			  (make-regexp org-todo-keywords-1)
  ;; 			  " +.*$")
  ;; 		 0))
  (imenu-add-to-menubar "Headlines")
  ;; dynamic abbreviations for org-mode
  (setq local-abbrev-table org-mode-abbrev-table))
(add-hook 'org-mode-hook 'sa-org-mode-hook)


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
   (R . t)
   (ruby . t)
   (shell . t)))


;;; org-mode-settings.el ends here
