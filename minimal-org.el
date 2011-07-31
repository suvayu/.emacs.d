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

;; ;; Default: w/o bibtex
;; (setq org-latex-to-pdf-process
;;       '("pdflatex -interaction nonstopmode %b"
;; 	"pdflatex -interaction nonstopmode %b"
;; 	"pdflatex -interaction nonstopmode %b"))

;; ;; w/ bibtex
;; (setq org-latex-to-pdf-process
;;       '("pdflatex -interaction nonstopmode %b"
;; 	"/usr/bin/bibtex %b"
;; 	"pdflatex -interaction nonstopmode %b"
;; 	"pdflatex -interaction nonstopmode %b"))

;;; org-mode-settings.el --- complete `org-mode' settings and customisations.
;; (load-file "~/.emacs.d/lisp/org-mode-settings.el")
