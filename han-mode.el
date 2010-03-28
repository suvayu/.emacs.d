;;; han-mode.el --- This is a major mode for editing HAN configuration files
;;;		    for ATLAS Data Quality Monitoring

;;; Commentary:
;; This mode is a very basic major mode for editing HAN configuration files.
;; It does syntax highlighting, and recognises comments.  I wrote this in one
;; afternoon and it is my first major mode.  So its a little buggy.  :-p

;;; History:
;; 
;;  26/03/10 - font-lock-mode cannot distinguish between keywords and attributes.
;;

;;; Code:

;; requires
(require 'font-lock)

;; mode hook
(defvar han-mode-hook nil)

;; mode map
(defvar han-mode-map
  (let ((han-mode-map (make-sparse-keymap)))
    (define-key han-mode-map "\C-j" 'newline-and-indent) ;; example
    han-mode-map)
  "Keymap for HAN configuration major mode.")

(add-to-list 'auto-mode-alist
	     (cons "\\(run\\|minutes[0-9]\\{1,3\\}\\).config\\'" 'han-mode))

(defconst han-font-lock-keywords
  (list
   '("\\<\\(algorithm\\|composite\\(?:[Aa]lgorithm\\)\\|dir\\|hist\\|limits\\|metadata\\|output\\|reference\\|thresholds\\)\\>"
     . font-lock-keyword-face) ; keywords
   '("\\<\\(algorithm\\|display\\|error\\|file\\|libnames?\\|name\\|output\\|path\\|re\\(?:ference\\|gex\\)\\|subalgs\\|thresholds\\|w\\(?:arning\\|eight\\)\\)\\> *="
     . font-lock-builtin-face) ; attributes
   '("\\<\\(all_in_dir\\|false\\|no\\|same_name\\|t\\(?:op_level\\|rue\\)\\|yes\\)\\>"
     . font-lock-string-face) ; special tokes
   '("\\([,@|]\\)" . font-lock-warning-face)) ; operators
  "Keyword highlighting for HAN configuration files.")

;; syntax table
(defvar han-mode-syntax-table
  (let ((han-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" han-mode-syntax-table)
    (modify-syntax-entry ?# "<" han-mode-syntax-table)
    (modify-syntax-entry ?\n ">" han-mode-syntax-table)
    (modify-syntax-entry ?{ "(" han-mode-syntax-table)
    (modify-syntax-entry ?} ")" han-mode-syntax-table)
    han-mode-syntax-table)
  "Syntax table for `han-mode'.")

(define-derived-mode han-mode conf-mode "HAN"
;; (defun han-mode ()
  "Major mode for editing HAN configuration files."

  ;; ;; not required when derived from another mode
  ;; (interactive)
  ;; (kill-all-local-variables)
  ;; (set-syntax-table han-mode-syntax-table)

  (use-local-map han-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(han-font-lock-keywords))

  ;; Commenting
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)

  (setq comment-start "# "
	comment-end ""
	comment-column 48))

  ;; ;; not required when derived from another mode
  ;; (setq major-mode 'han-mode)
  ;; (setq mode-name "HAN")
  ;; (run-hooks 'han-mode-hook))

(provide 'han-mode)

;;; han-mode.el ends here
