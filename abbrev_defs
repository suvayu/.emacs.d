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

(define-abbrev-table 'basic-text-mode-abbrev-table
  '(("bkg" "background" nil 0)
    ("bkgs" "backgrounds" nil 0)
    ("dist" "distribution" nil 0)
    ("cfit" "cFit" nil 0)
    ("sfit" "sFit" nil 0)
    ("sig" "signal" nil 0))
  "Basic text abbreviation table for all modes.")

;; abbrevs with +/- do not work, probably b/c they do not count as
;; part of the word
(define-abbrev-table 'unicode-abbrev-table
  '(;; ("K+"  "K⁺"  nil 0)
    ;; ("K-"  "K⁻"  nil 0)
    ("K0"  "K⁰"  nil 0)
    ;; ("pi+" "π⁺"  nil 0)
    ;; ("pi-" "π⁻"  nil 0)
    ("pi0" "π⁰"  nil 0)
    ;; ("D+"  "D⁺"  nil 0)
    ;; ("D-"  "D⁻"  nil 0)
    ;; ("Ds-" "Ds⁻" nil 0)
    ;; ("Ds+" "Ds⁺" nil 0)
    ("B0"  "B⁰"  nil 0)
    ;; ("Bs0" "Bs⁰" nil 0)
    ("Bs2Dspi" "Bs → Dsπ" nil 0)
    ("Bs2DsK" "Bs → DsK" nil 0)
    ("Dspi" "Dsπ" nil 0)
    )
  "Abbreviation table with Unicode characters (use in org buffers
   or emails).")

(define-abbrev-table 'text-mode-abbrev-table
  '(("ewk" "electroweak" nil 0)
    ("llh" "likelihood" nil 0)
    ("kfactor" "k-factor" nil 0)
    ("MC" "Monte Carlo" nil 0)
    ("SU3" "SU(3)" nil 0)
    ("SU2" "SU(2)" nil 0)
    ("U1" "U(1)" nil 0)
    ("tex" "TeX" nil 0)
    ("latex" "LaTeX" nil 0)
    ("etex" "e-TeX" nil 0)
    ("xetex" "XeTeX" nil 0)
    ("xelatex" "XeLaTeX" nil 0)
    ("luatex" "LuaTeX" nil 0)
    )
  "Abbreviation table for text-mode. Inherits from
  `basic-text-mode-abbrev-table'."
  :parents (list basic-text-mode-abbrev-table))

(define-abbrev-table 'latex-mode-abbrev-table
  '(("SU3" "\\(SU(3)\\)" nil 0)
    ("SU2" "\\(SU(2)\\)" nil 0)
    ("U1" "\\(U(1)\\)" nil 0)
    ("Bs" "B_{s}" nil 0)
    ("Ds" "D_{s}" nil 0)
    ("Bs2Dspi" "B_{s} → D_{s}π" nil 0)
    ("Bs2DsK" "B_{s} → D_{s}K" nil 0)
    ("Dspi" "D_{s}π" nil 0)
    ("DsKmp" "D_{s}^{-}K^{+}" nil 0)
    ("DsKpm" "D_{s}^{+}K^{-}" nil 0)
    )
  "Abbreviation table for `org-mode'. Inherits from
  `text-mode-abbrev-table'."
  :parents (list text-mode-abbrev-table))

(define-abbrev-table 'org-mode-abbrev-table
  '()
  "Abbreviation table for `org-mode'. Inherits from
  `text-mode-abbrev-table'."
  :parents (list text-mode-abbrev-table unicode-abbrev-table))
    ;; ("\(" "\(\)" (lambda() (backward-char 2)) 0)
    ;; ("ilatex" "src_latex{}" (lambda() (backward-char 1)) 0)

(define-abbrev-table 'message-mode-abbrev-table
  '(("hth" "Hope this helps" nil 0)
    ("HTH" "Hope this helps" nil 0)
    ("cheers" "Cheers" nil 0)
    )
  "Abbreviation table for `message-mode'. Inherits from
  `text-mode-abbrev-table' and `unicode-abbrev-table'."
  :parents (list text-mode-abbrev-table unicode-abbrev-table))
