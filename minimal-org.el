;;; Minimal setup to load latest `org-mode'

;;; Code
(load-file "~/.emacs.d/minimal-init.el")

(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/contrib/lisp"))


;; activate org
(require 'org-install)
;; (require 'google-weather)
;; (require 'org-google-weather)
;; (require 'org-inlinetask)

;; autoload, useful to check if `org-mode' is loaded
(autoload 'org-mode-p "org-macs"
  "Check if the current buffer is in Org-mode." t)

(setq org-latex-to-pdf-process
      '("pdflatex -interaction nonstopmode %b"
	"/usr/bin/bibtex %b"
	"pdflatex -interaction nonstopmode %b"
	"pdflatex -interaction nonstopmode %b"))

;;; org-mode-settings.el --- complete `org-mode' settings and customisations.
;; (load-file "~/.emacs.d/lisp/org-mode-settings.el")

;; ;; `org-mode' variable customisations
;; ;; directory to look for agenda files matching `org-agenda-file-regexp'
;; (setq org-agenda-files '("~/org")
;;       ;; log time for TODO state changes
;;       org-log-done 'time
;;       ;; log time on rescheduling
;;       org-log-reschedule 'time
;;       ;; log time on changing deadline
;;       org-log-redeadline 'time
;;       ;; To put notes inside LOGBOOK drawer
;;       org-log-into-drawer t
;;       )


;; ;; TODO keyword faces
;; (setq org-todo-keyword-faces
;;       '(("PBUG" . (:background "gold" :foreground "indianred3" :weight bold))
;; 	("CBUG" . (:background "gold" :foreground "indianred3" :weight bold))
;; 	("SEGF" . (:background "gold" :foreground "indianred3" :weight bold))
;; 	("CNCL" . (:background "snow3" :foreground "black" :weight bold))
;; 	))


;; ;; Keymaps:
;; ;; global keymaps
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c C-l") 'org-insert-link-global)
;; (global-set-key (kbd "C-c f") 'org-footnote-action)
;; (global-set-key (kbd "C-c b") 'org-switchb)
;; (global-set-key (kbd "C-c c") 'org-capture)


;; ;; hooks
;; ;; `org-agenda-mode' hook
;; (defun my-org-agenda-mode-hook()
;;   "My `org-agenda-mode' hook."
;;   (my-org-agenda-mode-keymap)
;;   (visual-line-mode t))
;; (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

;; ;; `org-mode' hook
;; (defun my-org-mode-hook()
;;   "My `org-mode' hook."
;;    ; (flyspell-mode t)
;;    (my-org-mode-keymap)
;;    ;; line folding w/o actually folding it, use `M-q' to wrap.
;;    (visual-line-mode t)
;;    ;; dynamic abbreviations for org-mode
;;    (setq local-abbrev-table text-mode-abbrev-table))

;; ;; Make windmove work in org-mode with 'shift as modifier:
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)


;; ;; Setup `org-babel' for emacs-lisp, gnuplot, latex and shell-script.
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((calc . t)
;;    (ditaa . t)
;;    (emacs-lisp . t)
;;    (gnuplot . t)
;;    (latex . t)
;;    (octave .t)
;;    (python . t)
;;    (ruby . t)
;;    (R . t)
;;    (ruby . t)
;;    (sh . t)))

;; ;; (defun setup-org-babel()
;; ;;   (interactive)
;; ;;   (org-babel-load-library-of-babel))

;; ;; `org-mode' hook
;; (add-hook 'org-mode-hook 'my-org-mode-hook)
