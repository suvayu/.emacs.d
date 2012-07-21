;;-*-coding: utf-8; mode: emacs-lisp -*-
;; TODO: switch to templates for the programming modes

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ;; ("for" "" c++-for-skel)
    ;; ("if" "" c++-if-skel)
    ))

;; ("if" "if (  ) ;" (lambda()
;; 			(backward-char 5)) 0)

;; ("for" "for ( = ; ; )
;;   {

;;   }" (lambda()
;;        (previous-line)) 0)

;;   ("ifl" "if (  )
;;   ;
;; else
;;   ;" (lambda()
;; 	 (previous-line 3)
;; 	 (backward-char)) 0)
;;   )

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ;; ("for" "" c++-for-skel)
    ;; ("if" "" c++-if-skel)
    ))

(define-abbrev-table 'global-abbrev-table '(    ))

(define-abbrev-table 'lisp-mode-abbrev-table '(    ))

(define-abbrev-table 'shell-mode-abbrev-table '(    ))

(define-abbrev-table 'unicode-abbrev-table
  '(("K+" "K⁺" nil 0)
    ("K-" "K⁻" nil 0)
    ("pi+" "π⁺" nil 0)
    ("pi-" "π⁻" nil 0)
    ("pi0" "π⁰" nil 0)
    ))

(define-abbrev-table 'latex-mode-abbrev-table
  '(("SU3" "\\(SU(3)\\)" nil 0)
    ("SU2" "\\(SU(2)\\)" nil 0)
    ("U1" "\\(U(1)\\)" nil 0)
    ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    )
  "Abbreviation table for `org-mode'. Inherits from
  `text-mode-abbrev-table'."
  :parents (list text-mode-abbrev-table))
    ;; ("\(" "\(\)" (lambda() (backward-char 2)) 0)
    ;; ("ilatex" "src_latex{}" (lambda() (backward-char 1)) 0)

(define-abbrev-table 'basic-text-mode-abbrev-table
  '(("bkg" "background" nil 0)
    ("bkgs" "backgrounds" nil 0)
    ("dist" "distribution" nil 0)
    ("sig" "signal" nil 0))
  "Basic text abbreviation table for all modes.")

(define-abbrev-table 'text-mode-abbrev-table
  '(("Bs" "Bs" nil 0)
    ("aBs" "B̄s" nil 0)
    ("ewk" "electroweak" nil 0)
    ("MC" "Monte Carlo" nil 0)
    ("SU3" "SU(3)" nil 0)
    ("SU2" "SU(2)" nil 0)
    ("U1" "U(1)" nil 0)
    ("tex" "TeX" nil 0)
    ("latex" "LaTeX" nil 0)
    ("e-tex" "e-TeX" nil 0)
    ("xetex" "XeTeX" nil 0)
    ("xelatex" "XeLaTeX" nil 0)
    ("luatex" "LuaTeX" nil 0)
    )
  "Abbreviation table for text-mode. Inherits from
  `basic-text-mode-abbrev-table'."
  :parents (list basic-text-mode-abbrev-table))
