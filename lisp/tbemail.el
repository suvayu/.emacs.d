;;; tbemail.el --- Provide syntax highlighting for email editing via
;;; Thunderbird's "External Editor" extension.
;;;   see: http://globs.org/articles.php?lng=en&pg=2&id=2

;; Copyright (C) 2007 by Martin Pohlack

;; Author: Martin Pohlack martinp (at) gmx.de
;; Version: 0.2
;; Time-stamp: <2007-11-09 martinp>

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Usage:

;; Load the mode via 
;;
;;   (require 'tbemail)
;;
;; in your init.el.  Be sure to have it in a known path, e.g., like so:
;;
;; (setq load-path (append load-path '("~/.emacs.d/lisp/")))

;;; Code:

(defvar tbemail-mode-hook nil)

(setq auto-mode-alist
      (append '(("\.eml$" . tbemail-mode))
              auto-mode-alist))

(defface tbemail-email-quote1
  '((t (:foreground "#000080" :background "#d7d7d7")))
  "Face for email quote-level 1" )

(defface tbemail-email-quote2
  '((t (:foreground "#600000" :background "#cdcdcd")))
  "Face for email quote-level 2" )

(defface tbemail-email-quote3
  '((t (:foreground "#006000" :background "#c3c3c3")))
  "Face for email quote-level 3" )

(defface tbemail-email-quote4
  '((t (:foreground "#600080" :background "#b9b9b9")))
  "Face for email quote-level 4" )

(defface tbemail-email-quote5
  '((t (:foreground "#600080" :background "#ffdab9")))
  "Face for email quote-level 5" )

(defface tbemail-email-quote6
  '((t (:foreground "#600080" :background "#ffe4b5")))
  "Face for email quote-level 6" )

(defface tbemail-email-quote7
  '((t (:foreground "#600080" :background "#778899")))
  "Face for email quote-level 7" )

(defface tbemail-email-quote8
  '((t (:foreground "#600080" :background "#5f9ea0")))
  "Face for email quote-level 8" )

(defface tbemail-email-quote9
  '((t (:foreground "#600080" :background "#00ff7f")))
  "Face for email quote-level 9" )

(defface tbemail-email-bold
  '((t (:bold t)))
  "Face for email bold text" )

(defface tbemail-email-italic
  '((t (:italic t)))
  "Face for email italic text" )

(defface tbemail-email-underline
  '((t (:underline t)))
  "Face for email underlined text" )

(defvar tbemail-font-lock-keywords
      (list '("^> +\\(.*\\)$"           1 'tbemail-email-quote1)
            '("^>> +\\(.*\\)$"          1 'tbemail-email-quote2)
            '("^>>> +\\(.*\\)$"         1 'tbemail-email-quote3)
            '("^>>>> +\\(.*\\)$"        1 'tbemail-email-quote4)
            '("^>>>>> +\\(.*\\)$"       1 'tbemail-email-quote5)
            '("^>>>>>> +\\(.*\\)$"      1 'tbemail-email-quote6)
            '("^>>>>>>> +\\(.*\\)$"     1 'tbemail-email-quote7)
            '("^>>>>>>>> +\\(.*\\)$"    1 'tbemail-email-quote8)
            '("^>>>>>>>>> +\\(.*\\)$"   1 'tbemail-email-quote9)
            '("/\\(\\b\\w*\\b\\)/"     1 'tbemail-email-italic)
            '("\\*\\(\\b\\w*\\b\\)\\*" 1 'tbemail-email-bold)
            '("[_]\\(\\b\\w*\\b\\)[_]" 1 'tbemail-email-underline)
	    '("\"\\([^\"]\\)+\"" . 'font-lock-string-face) ; quoting with newlines
	    '("`\\([^`'\n]\\)+'" . 'font-lock-string-face) ; quoting excluding newlines
            ))

(defun tbemail-mode ()
  "Major mode for highlighting Thunderbird-External-Edit emails"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(tbemail-font-lock-keywords nil t nil nil))

  ;; Comment stuff.
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  ;; (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)

  (setq comment-start "> "
	comment-end ""
	;; comment-start-skip "\\W*"
	comment-empty-lines "t"
	comment-column 48)

  (setq mode-name "TBE" major-mode 'tbemail)
  (run-hooks 'tbemail-mode-hook))

(provide 'tbemail)
