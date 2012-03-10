;;; Minimal setup to load latest `org-mode'

;;; Code
(load-file "~/.emacs.d/minimal-init.el")

(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/build/org-mode/contrib/lisp"))

;; activate org
(require 'org-install)

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
