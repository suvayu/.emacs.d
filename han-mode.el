;;; han-mode.el --- This is a major mode for editing HAN configuration files
;;;		    for ATLAS Data Quality Monitoring

;; Copyright (C) 2010 by Suvayu Ali
;; Author: Suvayu Ali <fatkasuvayuNOSPAM(at)gmail(dot)com>
;; url: <http://github.com/suvayu/elisp/blob/master/han-mode.el>

;; This file is not part of any emacsen.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This mode is a very basic major mode for editing HAN configuration files
;; for ATLAS Data Quality Monitoring. It does syntax highlighting, and
;; allows commenting. I wrote this in one afternoon and it is my first
;; major mode. So its a little buggy. :-p

;;; History:
;; 
;;  26/03/10 - font-lock-mode cannot distinguish between keywords and attributes.
;;  28/03/10 - fixed commenting bug and included more distinctive faces.
;;  29/03/10 - made keywords more modular.

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

(defconst han-keywords
  "\\<\\(algorithm\\|composite\\(?:[Aa]lgorithm\\)\\|dir\\|hist\\|limits\\|metadata\\|output\\|reference\\|thresholds\\)\\>"
  "Keywords for `han-mode'")

(defconst han-attributes
  "\\<\\(algorithm\\|display\\|error\\|file\\|libnames?\\|name\\|output\\|path\\|re\\(?:ference\\|gex\\)\\|subalgs\\|thresholds\\|w\\(?:arning\\|eight\\)\\)\\> *="
  "Attributes for `han-mode'")

(defconst han-spl-tokens
  "\\<\\(all_in_dir\\|false\\|no\\|same_name\\|t\\(?:op_level\\|rue\\)\\|yes\\)\\>"
  "Special tokens for `han-mode'")

(defconst han-operators
  "\\([,@|]\\)"
  "Operators for `han-mode'")

(defvar han-font-lock-keywords
  (list
   `(,han-keywords	. font-lock-keyword-face)
   `(,han-attributes	. font-lock-builtin-face)
   `(,han-spl-tokens	. font-lock-string-face)
   `(,han-operators	. font-lock-warning-face))
  "Keyword, attribute, special token and operator highlighting
   for HAN configuration files in `han-mode'.")

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

;; (defun han-indent-line ()
;;   "Indent current like for HAN configuration files."
;;   (interactive)
;;   (beginning-of-line)
;;   (if (bobp) ; check if beginning of buffer
;;       (indent-line-to 0)
;;     (if (looking-at "^\\s-*}") ; check whether closing `}'
;; 	(progn 
;; 	  (save-excursion 
;; 	    ;; (forward-line -1)
;; 	    ;; (setq cur-indent (- (current-indentation) default-tab-width))
;; 	    (search-backward-regexp 
;; 	     (concat "^\\s-*" han-keywords " +\\w+ *{") nil t 1)
;; 	    )))))

;; regex to match earlier `{' "^\\s-*\\<\\(algorithm\\|composite\\(?:[Aa]lgorithm\\)\\|dir\\|hist\\|limits\\|metadata\\|output\\|reference\\|thresholds\\)\\> +\\w+ *{"
;; (concat "^\\s-*" han-keywords " +\\w+ *{")

(define-derived-mode han-mode c++-mode "HAN"
  "Major mode for editing HAN configuration files."
  (use-local-map han-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(han-font-lock-keywords))

  ;; Commenting
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  ;; (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)

  (setq comment-start "# "
	comment-end ""
	;; comment-start-skip "# \\w"
	comment-column 48))

;; ;; not required when derived from another mode
;; (defun han-mode () 
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table han-mode-syntax-table)
;;   (setq major-mode 'han-mode)
;;   (setq mode-name "HAN")
;;   (run-hooks 'han-mode-hook))

(provide 'han-mode)

;;; han-mode.el ends here
