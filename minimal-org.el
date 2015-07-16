;;; Minimal setup to load latest `org-mode'

;;; Code
(load-file "~/.emacs.d/minimal-init.el")

;; TeXLive setup
(setenv "PATH" "/opt/texlive/2012/bin/x86_64-linux:$PATH" t)

(add-to-list 'load-path (expand-file-name "~/build/org-mode/lisp"))

(require 'ox-beamer)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(require 'ox-koma-letter)
(add-to-list 'org-latex-classes
	     `("scrlttr2"
	       ,(concat "\\documentclass\[a4paper\]\{scrlttr2\}\n"
			"\[NO-DEFAULT-PACKAGES]\n"
			"\[NO-PACKAGES]\n"
			"\\usepackage\{fixltx2e\}\n"
			"\\usepackage\{fontspec\}\n"
			"\\usepackage\{microtype\}\n"
			"\\usepackage\{polyglossia\}\n"
			"\\setdefaultlanguage[variant=british]\{english\}\n"
			"\\usepackage\{libertine\}\n"
			"\\usepackage\[normalem\]\{ulem\}\n"
			"\\usepackage\{amsmath\}\n"
			"\\usepackage\{hyperref\}\n")
	       ("\\section\{%s\}" . "\\section*\{%s\}")
	       ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
	       ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
