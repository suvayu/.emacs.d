;;; org-mode-settings.el --- `org-mode' settings and customisations.

;; since using org-mode in ~/build/org-mode
(require 'org-install)
(require 'google-weather)
(require 'org-google-weather)
(require 'org-inlinetask)
;; (require 'org2blog)

;;; Code:

;; autoload, useful to check if `org-mode' is loaded
(autoload 'org-mode-p "org-macs"
  "Check if the current buffer is in Org-mode." t)


;; `org-mode' variable customisations
;; directory to look for agenda files matching `org-agenda-file-regexp'
(setq org-agenda-files '("~/org")
      ;; List of extra files to be searched by text search commands.
      org-agenda-text-search-extra-files
      (append '(agenda-archives)	; archived agenda files
	      (directory-files "~/org/Wprime/kfactor" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; notes/results
	      (directory-files "~/org/Wprime/note" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; notes/results
	      (directory-files "~/org/Wprime/qcdfit_plots" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; notes/results
	      (directory-files "~/org/Wprime/recoil_smearing" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; notes/results
	      (directory-files "~/org/Wprime/talks" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; notes/results
	      (directory-files "~/org/masters-thesis" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; thesis org files
	      (directory-files "~/org/not-physics" t "^[^.#].*\\.\\(org$\\|org_archive\\)") ; other stuff
	      ;; (directory-files "~/org/Worg" t "^[^.#].*\\.org$") ; org files from Worg
	      ;; (directory-files "~/org" t "^[^.#].*\\.txt$") ; text files in org directory
	      )
      ;; modifying behaviour of C-a/<home> & C-e/<end>
      org-special-ctrl-a/e t
      ;; on links `RET' follows the link
      org-return-follows-link t
      ;; log time for TODO state changes
      org-log-done 'time
      ;; log time on rescheduling
      org-log-reschedule 'time
      ;; To put notes inside LOGBOOK drawer
      org-log-into-drawer t
      ;; turn on speed keys for headlines
      org-use-speed-commands t
      ;; fontify code blocks by default
      org-src-fontify-natively t
      ;; temporary setting to circumvent bug in texi2dvi
      ;; file a bug report on bugzilla
      ;; debug original value like this
      ;; org-latex-to-pdf-process '("sh -v -x texi2dvi -p -b -c -V %f")
      ;; org-latex-to-pdf-process '("pdflatex %f" "bibtex %f" "pdflatex %f" "pdflatex %f")
      ;; update TODO cookies recursively
      ;; use property, ":COOKIE_DATA: todo recursive"
      ;; to set this only for a single subtree
      org-hierarchical-todo-statistics nil
      org-agenda-current-time-string "- - - NOW! - - -"
      org-agenda-time-grid '((daily today)
			     "----------------"
			     (800 1000 1200 1400 1600 1800 2000))
      org-google-weather-format "%i %c %L, [%l,%h] %s"
      org-refile-targets '((org-agenda-files :maxlevel . 5))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-reverse-note-order t
      org-stuck-projects '("+LEVEL<=4&+TIMESTAMP<\"<today>\"/-DONE" ("DONE" "FIXD" "CNCL") nil "")
      org-beamer-environments-extra '("only" "o" "\\only%a{%h%x" "}")
      ;; not sure about %x above, double check
      )


;; org to latex customisations
;; hack for error free latex export with amsmath
;; remove when defaults are changed in the future
;; (add-to-list 'org-export-latex-packages-alist '("" "amsmath" t))
(setcar
 (rassoc '("wasysym" t)
	 org-export-latex-default-packages-alist) "nointegrals")

;; (add-to-list 'org-beamer-environments-extra
;; 	     '("only" "o" "\\only%a{%h%x" "}"))


;; show links as inline images using `iimage-mode'
(load-library "iimage")
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]")  1))


;; TODO keywords
;; @ - time stamp with note
;; ! - only time stamp
(setq org-todo-keywords
      '((sequence "TODO(t)" "DLAY(l@/!)" "CONT(c!)" "|" "DONE(d@)" "CNCL(n@/!)")
	(sequence "WInP(w!)" "DBUG(b!)" "|" "CMIT(m@)")
	(type "PBUG(p@)" "CBUG(r@)" "SEGF(s@/@)" "|" "FIXD(f@/!)")
	))

;; TODO keyword faces
(setq org-todo-keyword-faces
      '(("PBUG" . (:background "gold" :foreground "indianred3" :weight bold))
	("CBUG" . (:background "gold" :foreground "indianred3" :weight bold))
	("SEGF" . (:background "gold" :foreground "indianred3" :weight bold))
	("CNCL" . (:background "snow3" :foreground "black" :weight bold))
	))


;; TAG faces
(setq org-tag-faces
      '(("PROJ" :background "indianred3" :foreground "cornsilk2" :weight bold)
	))
;; (setq org-tag-persistent-alist
;;       '(("physics-bug" . 112)
;; 	("code-bug" . 99)
;; 	("segfault" . 115)
;; 	("Project" . 80)))


;; templates for `org-capture'
(setq org-capture-templates
      '(("m" "Select meeting templates")
	("ms" "Schedule a meeting" entry (file+headline "~/org/meetings.org" "Meetings")
	 "** %? %^t%^{CATEGORY}p\n"
	 :prepend t :empty-lines 1)
	("mm" "Meeting minutes w/ clock" entry (file+datetree "~/org/meetings.org")
	 "**** %^{prompt} %U%^{CATEGORY}p\n\n     %?"
	 :clock-in t :empty-lines 1)
	("mt" "Add to clocked meeting minutes" item (clock)
	 "" :unnarrowed t)
	("mn" "Meeting notes" entry (file+datetree "~/org/meetings.org")
	 "**** %^{prompt} %U%^{CATEGORY}p\n\n     %?"
	 :prepend t :empty-lines 1)
	("n" "Notes" entry (file+headline "~/org/notes.org" "Notes")
	 "** %^{prompt}%^{CATEGORY}p\n\n   %?"
	 :prepend t :empty-lines 1)
	("r" "Reading material" entry (file+headline "~/org/notes.org" "Reading")
	 "** %?%^{CATEGORY}p %^G\n   %^t"
	 :prepend t :empty-lines 1) ; :unnarrowed t
	("t" "TODO Item" entry (file+headline "~/org/notes.org" "TODOs")
	 "** %^{prompt|TODO|WInP} %?\n   %^t"
	 :prepend t :empty-lines 1)
	))


;; changing org-mode behaviour
;; defadvising org-mode commands
(defadvice outline-forward-same-level
  (around outline-forward-same-level-or-next-visible (arg))
  "If its the last outline sibling, move to the next visible outline heading."
  (if (save-excursion
	 (outline-get-next-sibling))
      ad-do-it
    (outline-next-visible-heading arg)))
(ad-activate 'outline-forward-same-level)

(defadvice outline-backward-same-level
  (around outline-backward-same-level-or-previous-visible (arg))
  "If its the last outline sibling, move to the previous visible outline heading."
  (if (save-excursion
	 (outline-get-last-sibling))
      ad-do-it
    (outline-previous-visible-heading arg)))
(ad-activate 'outline-backward-same-level)


;; Keymaps:
;; global keymaps
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c f") 'org-footnote-action)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; `org-mode' keymaps
(defun my-org-mode-keymap()
  "My `org-mode' keymap."
  ;; ;; overload default `fill-paragraph' keybind to use org specific command
  ;; (org-defkey org-mode-map (kbd "M-q") 'org-fill-paragraph) ; complains with wrong argument type
  ;; toggle inline images with iimage
  ;; (org-defkey org-mode-map (kbd "") 'org-toggle-inline-images)
  ;; navigating list items
  (org-defkey org-mode-map (kbd "C-c C-x N") 'org-next-item)
  (org-defkey org-mode-map (kbd "C-c C-x P") 'org-previous-item)
  ;; navigating headlines
  (org-defkey org-mode-map (kbd "C-<left>") 'outline-up-heading)
  ;; (local-set-key '[(C-right)] 'outline-back-to-heading) ; this one is not interactive
  (org-defkey org-mode-map (kbd "C-<up>") 'outline-previous-visible-heading)
  (org-defkey org-mode-map (kbd "C-<down>") 'outline-next-visible-heading)
  (org-defkey org-mode-map (kbd "s-<up>") 'outline-backward-same-level)
  (org-defkey org-mode-map (kbd "s-<down>") 'outline-forward-same-level))

;; `org-agenda-mode' keymaps
(defun my-org-agenda-mode-keymap()
  "My `org-agenda-mode' keymap."
  ;; set property
  (org-defkey org-agenda-mode-map (kbd "C-p") 'org-agenda-set-property)
  ;; month view
  (org-defkey org-agenda-mode-map (kbd "C-c m") 'org-agenda-month-view))


;; org-agenda config
;; This function is used to insert current time in the agenda buffer
;; Thanks to Julien Danjou
(defun jd:org-current-time ()
  "Return current-time if date is today."
  (when (equal date (calendar-current-date))
    (propertize (format-time-string "%H:%M Current time") 'face
		'(:weight bold :foreground "DodgerBlue4" :background "snow"))))


;; hooks
;; `org-agenda-mode' hook
(defun my-org-agenda-mode-hook()
  "My `org-agenda-mode' hook."
  (my-org-agenda-mode-keymap)
  (visual-line-mode t))
(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

;; `org-mode' hook
(defun my-org-mode-hook()
  "My `org-mode' hook."
   ; (flyspell-mode t)
   (my-org-mode-keymap)
   ;; line folding w/o actually folding it, use `M-q' to wrap.
   (visual-line-mode t)
   ;; dynamic abbreviations for org-mode
   (setq local-abbrev-table text-mode-abbrev-table))

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

;; (defun setup-org-babel()
;;   (interactive)
;;   (org-babel-load-library-of-babel))

;; `org-mode' hook
(add-hook 'org-mode-hook 'my-org-mode-hook)


;;; org-mode-settings.el ends here
