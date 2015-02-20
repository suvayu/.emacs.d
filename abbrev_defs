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

(define-abbrev-table 'basic-text-mode-abbrev-table '(    ))

;; abbrevs with +/- do not work, probably b/c they do not count as
;; part of the word
(define-abbrev-table 'unicode-abbrev-table
  '(("Kp"  "K⁺"  nil 0)
    ("Km"  "K⁻"  nil 0)
    ("K0"  "K⁰"  nil 0)
    ("pi"  "π"   nil 0)
    ("pip" "π⁺"  nil 0)
    ("pim" "π⁻"  nil 0)
    ("pi0" "π⁰"  nil 0)
    ("Dp"  "D⁺"  nil 0)
    ("Dm"  "D⁻"  nil 0)
    ("D0"  "D⁰"  nil 0)
    ("B0"  "B⁰"  nil 0)
    ;; ("Bs0" "Bs⁰" nil 0)
    ("Bs2Dspi" "Bs → Dsπ" nil 0)
    ("Bs2DsK" "Bs → DsK" nil 0)
    ("Dspi" "Dsπ" nil 0)
    )
  "Abbreviation table with unicode characters (use in org buffers
   or emails).")

(define-abbrev-table 'text-mode-abbrev-table
  '(("ewk" "electroweak" nil 0)
    ;; ("kfactor" "k-factor" nil 0)
    ("llh" "log-likelihood" nil 0)
    ;; ("MC" "Monte Carlo" nil 0)
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
  "Abbreviation table for text-mode.")

(define-abbrev-table 'latex-mode-abbrev-table
  '(("kfactor" "\\emph{k}-factor" nil 0)
    ("SU3" "\\(SU(3)\\)" nil 0)
    ("SU2" "\\(SU(2)\\)" nil 0)
    ("U1" "\\(U(1)\\)" nil 0)
    ("Bs" "B\\(_\\text{s}\\)" nil 0)
    ("Ds" "D\\(_\\text{s}\\)" nil 0)
    ("Dsst" "D\\(_\\text{s}^{*}\\)" nil 0)
    ("Lb" "Λ\\(_\\text{b}\\)" nil 0)
    ("Lc" "Λ\\(_\\text{c}\\)" nil 0)
    ("pis" "π\\(_\\text{s}\\)" nil 0)
    ("Bs2Dspi" "B\\(_\\text{s}\\) → D\\(_\\text{s}\\)π" nil 0)
    ("Bs2DsK"  "B\\(_\\text{s}\\) → D\\(_\\text{s}\\)K" nil 0)
    ("Dspi"    "D\\(_\\text{s}\\)π" nil 0)
    ("DsK"     "D\\(_\\text{s}\\)K" nil 0)
    ("Bs2"     "B\\(_\\text{s}\\) →" nil 0)
    ("Bs2Ds"   "B\\(_\\text{s}\\) → D\\(_\\text{s}\\)" nil 0)
    )
  "Abbreviation table for `org-mode'. Inherits from
  `text-mode-abbrev-table'."
  :parents (list text-mode-abbrev-table))

(define-abbrev-table 'org-mode-abbrev-table
  '(("kfactor" "/k/-factor" nil 0)
    ("Bs" "B_{s}" nil 0)
    ("Ds" "D_{s}" nil 0)
    ("Dsst" "D\\(_\\text{s}^{*}\\)" nil 0)
    ("Lb" "Λ_{b}" nil 0)
    ("Lc" "Λ_{c}" nil 0)
    ("pis" "π_{s}" nil 0)
    ("Bs2Dspi" "B_{s} → D_{s}π" nil 0)
    ("Bs2DsK" "B_{s} → D_{s}K" nil 0)
    ("Dspi" "D_{s}π" nil 0)
    ("DsK" "D_{s}K" nil 0)
    ("Bs2" "B_{s} →" nil 0)
    ("Bs2Ds" "B_{s} → D_{s}" nil 0)
    )
  "Abbreviation table for `org-mode'. Inherits from
  `text-mode-abbrev-table'."
  :parents (list text-mode-abbrev-table unicode-abbrev-table))
    ;; ("\(" "\(\)" (lambda() (backward-char 2)) 0)
    ;; ("ilatex" "src_latex{}" (lambda() (backward-char 1)) 0)

(define-abbrev-table 'message-mode-abbrev-table
  '(("hth" "Hope this helps" nil 0)
    ("HTH" "Hope this helps" nil 0)
    ("cheers" "Cheers" nil 0)
    ("thanks" "Thanks" nil 0)
    )
  "Abbreviation table for `message-mode'. Inherits from
  `text-mode-abbrev-table' and `unicode-abbrev-table'."
  :parents (list text-mode-abbrev-table unicode-abbrev-table))
