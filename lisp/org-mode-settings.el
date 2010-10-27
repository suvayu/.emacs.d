;;; org-mode-settings.el --- `org-mode' settings and customisations.

;; since using org-mode in ~/build/org-mode
(require 'org-install)
(require 'google-weather)
(require 'org-google-weather)

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
	      (directory-files "~/org/projects/Wprime" t "^[^.#].*\\.org$") ; org files from talks
	      ;; (directory-files "~/org/Worg" t "^[^.#].*\\.org$") ; org files from Worg
	      ;; (directory-files "~/org" t "^[^.#].*\\.txt$") ; text files in org directory
	      )
      ;; modifying behaviour of C-a/<home> & C-e/<end>
      org-special-ctrl-a/e t
      ;; log time for TODO state changes
      org-log-done 'time
      ;; log time on rescheduling
      org-log-reschedule 'time
      ;; To put notes inside LOGBOOK drawer
      org-log-into-drawer t
      ;; turn on speed keys for headlines
      org-use-speed-commands t
      ;; temporary setting to circumvent bug in texi2dvi
      ;; file a bug report on bugzilla
      ;; debug original value like this
      ;; org-latex-to-pdf-process '("sh -v -x texi2dvi -p -b -c -V %f")
      ;; org-latex-to-pdf-process '("pdflatex %f" "bibtex %f" "pdflatex %f" "pdflatex %f")
      ;; update TODO cookies recursively
      ;; use property, ":COOKIE_DATA: todo recursive"
      ;; to set this only for a single subtree
      org-hierarchical-todo-statistics nil)

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


;; global keymaps
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c f") 'org-footnote-action)
(global-set-key (kbd "C-c b") 'org-switchb)

;; `org-mode' keymaps
(defun my-org-mode-keymap()
  "My `org-mode' keymap."
  ;; ;; overload default `fill-paragraph' keybind to use org specific command
  ;; (local-set-key (kbd "M-q") 'org-fill-paragraph) ; complains with wrong argument type
  ;; toggle inline images with iimage
  ;; (local-set-key (kbd "") 'org-toggle-inline-images)
  (local-set-key (kbd "C-<left>") 'outline-up-heading)
  ;; (local-set-key '[(C-right)] 'outline-back-to-heading) ; this one is not interactive
  (local-set-key (kbd "C-<up>") 'outline-previous-visible-heading)
  (local-set-key (kbd "C-<down>") 'outline-next-visible-heading)
  (local-set-key (kbd "s-<up>") 'outline-backward-same-level)
  (local-set-key (kbd "s-<down>") 'outline-forward-same-level))


;; hooks
;; ;; `org-agenda-mode' hook
;; (defun my-org-agenda-mode-hook()
;;   "My `org-agenda-mode' hook."
;;   (visual-line-mode t))
;; (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

;; FIXME
(defun yas-org-setup
  "Activate yasnippet keybinds."
  (make-variable-buffer-local 'yas/trigger-key)
  (org-set-local yas/trigger-key [tab])
  (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand))

;; `org-mode' hook
(defun my-org-mode-hook()
  "My `org-mode' hook."
  ;; (flyspell-mode t)
  (my-org-mode-keymap)
  ;; line folding w/o actually folding it, use `M-q' to wrap.
  (visual-line-mode t)
  ;; dynamic abbreviations for org-mode
  (setq local-abbrev-table text-mode-abbrev-table)
  ;; FIXME
  (make-variable-buffer-local 'yas/trigger-key)
  (org-set-local 'yas/trigger-key [tab])
  (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand))
;  (yas-org-setup))


;; Make windmove work in org-mode:
;; (add-hook 'org-metaup-hook 'windmove-up)
;; (add-hook 'org-metaleft-hook 'windmove-left)
;; (add-hook 'org-metadown-hook 'windmove-down)
;; (add-hook 'org-metaright-hook 'windmove-right)


;; Setup `org-babel' for emacs-lisp, gnuplot, latex and shell-script.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (python . t)
   (R . t)
   (sh . t)))

;; (defun setup-org-babel()
;;   (interactive)
;;   (org-babel-load-library-of-babel))


;;; org-mode-settings.el ends here
