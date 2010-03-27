;;; @(#) cmt-mode.el --- a major mode for editing CMT configuration files

;; $Id: cmt-mode.el,v 1.2 2004/03/23 13:36:34 arnault Exp $

;; Copyright (C) 2004 by Antoine Pérus

;; Author: Antoine Pérus <perus@lal.in2p3.fr>
;; Created: 29 Feb 2004
;; Keywords: local, languages, tools

;; This file is not part of XEmacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This mode adds some services editing the CMT requirements file.
;; See <http://www.cmtsite.org> for a CMT description.
;; This is a hack initialy started from apache.el and css-mode.el
;; The list of keywords was derived from the documentation for CMT;
;; there is lot of omissions.
;;
;; To enable automatic selection of this mode :
;;
;;   (autoload 'cmt-mode "cmt-mode" "autoloaded" t)
;;   (add-to-list 'auto-mode-alist '("requirements"   . cmt-mode))
;;

;;; Change Log:
;;
;;   13/03/04 - New way in definition of cmt-font-lock-keywords with a more
;;   modular definition of keyword groups. Should help too for the next step :
;;   a completion mechanism.

;;; Code:

;; Requires modules
(eval-when-compile
  (defvar byte-compile-dynamic nil)	;silence old Emacs's ByteCompiler
  (set (make-local-variable 'byte-compile-dynamic) t))

(require 'font-lock)
(require 'custom)

;; Variables

;;;###autoload
(defgroup cmt nil
  "Major mode for editing CMT configuration files."
  :prefix "cmt-"
  :group 'languages)

(defcustom cmt-manual-url "http://www.cmtsite.org/CMTDoc.html"
  "*URL at which to find the CMT manual."
  :type 'string
  :group 'cmt)

;;;###autoload
(defcustom cmt-file-patterns
  (list "requirements")
  "*List of file patterns for which to automatically invoke `cmt-mode'."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'cmt)

(defcustom cmt-mode-hook nil
  "*List of hook functions run by `cmt-mode' (see `run-hooks')."
  :type 'hook
  :group 'cmt)

(defvar cmt-mode-version-id
  "$Id: cmt-mode.el,v 1.2 2004/03/23 13:36:34 arnault Exp $"
  "Latest modification time and version number.")

(defconst cmt-mode-version "0.4"
  "Cmt-mode official version.")

;;; --- end of up-to-date-overview configuration ------------------

(defvar cmt-mode-syntax-table nil
  "Syntax table for `cmt-mode'.")

;; Make cmt-mode the default mode for cmt config buffers.
;;;###autoload
(let ((cmt-file-patterns-temp cmt-file-patterns))
  (while cmt-file-patterns-temp
    (add-to-list 'auto-mode-alist
		  (cons (car cmt-file-patterns-temp) 'cmt-mode))
    (setq cmt-file-patterns-temp (cdr cmt-file-patterns-temp))))



(defconst cmt-symbols
  '(
    "alias" "action"
    ;; environment directives
    "set" "set_append" "set_prepend" "set_remove"
    ;; path directives
    "path" "path_append" "path_prepend" "path_remove"
    ;; macro directives
    "macro" "macro_append" "macro_prepend" "macro_remove"
    ;; setup directives
    "setup_script" "cleanup_script"
    "setup_strategy"
    )
  "A list of CMT symbol keywords.")

(defconst cmt-patterns
  '(
    ;; Pattern directives
    "pattern" "apply_pattern" "ignore_pattern" "cmtpath_pattern"
    )
  "A list of CMT pattern keywords.")

(defconst cmt-tags
  '(
    "tag" "apply_tag" "ignore_tag" "tag_exclude"
    )
  "A list of CMT tags relative keywords.")

(defconst cmt-features
  '(
    ;; core directives
    "package" "version" "project"
    "author" "manager"
    ;;
    "use"
    ;;
    "branches"
    ;;
    "public" "end_public" "private" "end_private"
    ;;
    "application" "library" "document"
    "make_fragment"
    ;;
    "include_dirs" "include_path"
    )
  "A list of CMT feature keywords.")

(defconst cmt-options
  '(
    "-no_share" "-no_static"
    "-prototypes" "-no_prototypes" "-check"
    "-group" "-suffix"
    "-import" "-no_auto_imports"
    "-header" "-global"
    )
  "A list of CMT options.")


(defconst cmt-keywords-alist
  (append
   (mapcar (lambda(prop)
	     (cons (concat prop " ") nil)) cmt-symbols)
   (mapcar (lambda(prop)
	     (cons (concat prop " ") nil)) cmt-patterns)
   (mapcar (lambda(prop)
	     (cons (concat prop " ") nil)) cmt-tags)
   (mapcar (lambda(prop)
	     (cons (concat prop " ") nil)) cmt-features)
   (mapcar (lambda(prop)
	     (cons (concat prop "=") nil)) cmt-options)
   )
  "An association list of the CMT keywords for completion use.")


(defun cmt-list-2-regexp-at-beginning-of-line(altlist)
  "Takes a list and returns the regexp ^\\s-*\\(elem1\\|elem2\\|...\\)\\s-"
  (let ((regexp "^\\s-*\\("))
    (mapcar (lambda(elem)
              (setq regexp (concat regexp elem "\\|")))
            altlist)
    (concat (substring regexp 0 -2) ; cutting the last "\\|"
            "\\)\\s-")
    ))

(defun cmt-list-2-regexp(altlist)
  "Takes a list and returns the regexp \\(elem1\\|elem2\\|...\\)"
  (let ((regexp "\\("))
    (mapcar (lambda(elem)
              (setq regexp (concat regexp elem "\\|")))
            altlist)
    (concat (substring regexp 0 -2) ; cutting the last "\\|"
            "\\)")
    ))

;; Font lock
(defconst cmt-font-lock-keywords
  (purecopy
   (list
    (list "^\\s-*#.*$" 0 'font-lock-comment-face t)

    (cons (cmt-list-2-regexp-at-beginning-of-line cmt-symbols)
	  font-lock-function-name-face)

    (cons (cmt-list-2-regexp-at-beginning-of-line cmt-patterns)
	  font-lock-function-name-face)

    (cons (cmt-list-2-regexp-at-beginning-of-line cmt-tags)
	  font-lock-type-face)

    (cons (cmt-list-2-regexp-at-beginning-of-line cmt-features)
	  font-lock-keyword-face)

    (cons (cmt-list-2-regexp cmt-options) font-lock-type-face)

    ))
  "Expressions to highlight in `cmt-mode' buffers.")



;; Motion code.

(defconst cmt-pattern-regex
  (eval-when-compile
   (concat
    "^"
    "\\("
    "pattern"
   "\\)"))
  "Regex used to find patterns.")

(defun cmt-next-pattern ()
  "Move point to the beginning of the next pattern definition."
  (interactive)
  (let ((here (point)))
    (end-of-line)
    (if (re-search-forward cmt-pattern-regex (point-max) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))

(defun cmt-previous-pattern ()
  "Move point to the beginning of the previous pattern definition."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (if (re-search-backward cmt-pattern-regex (point-min) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))


(defun cmt-next-feature ()
  "Move point to the beginning of the next feature."
  (interactive)
  (let ((here (point)))
    (end-of-line)
    (if (re-search-forward
	 (cmt-list-2-regexp-at-beginning-of-line cmt-features)
	 (point-max) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))

(defun cmt-previous-feature ()
  "Move point to the beginning of the previous feature."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (if (re-search-backward
	 (cmt-list-2-regexp-at-beginning-of-line cmt-features)
	 (point-min) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))


(defconst cmt-menubar-menu
    '("Cmt"
      ["Move to Next Pattern" cmt-next-pattern t]
      ["Move to Previous Pattern" cmt-previous-pattern t]
      "---"
      ["Move to Next Feature" cmt-next-feature t]
      ["Move to Previous Feature" cmt-previous-feature t]
    ))

(defconst cmt-popup-menu
    (cons "CMT Mode Commands"
	  (cdr cmt-menubar-menu)))




;;===============

;;; Property completion
; This code is stolen from ans1-mode.el.

(defun cmt-completion ()
  "Do a completion with `cmt-keywords-alist'."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (set-syntax-table cmt-mode-syntax-table)
		    (skip-syntax-backward "\\w")
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (completion (try-completion pattern cmt-keywords-alist)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for %s " pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (sort (all-completions pattern
				     cmt-keywords-alist)
		    'string<))))
	  )
    )
  )




;;===============


;; Syntax table
(if cmt-mode-syntax-table
    nil
  (setq cmt-mode-syntax-table (copy-syntax-table nil))
  ;; underscore considered part of word
  (modify-syntax-entry ?_    "w"    cmt-mode-syntax-table)
  (modify-syntax-entry ?\-   "w"    cmt-mode-syntax-table)
  (modify-syntax-entry ?\"   "\""   cmt-mode-syntax-table))


;; Keymap for cmt-mode.
(defvar cmt-mode-map nil
  "Keymap used in `cmt-mode' buffers.")

(if cmt-mode-map
    nil
  (setq cmt-mode-map (make-sparse-keymap))
  ;; Set up the keymap
  (define-key cmt-mode-map "\M-\t" 'cmt-completion)
  (define-key cmt-mode-map "\M-p"  'cmt-previous-pattern)
  (define-key cmt-mode-map "\M-n"  'cmt-next-pattern)
  (define-key cmt-mode-map "\M-f"  'cmt-next-feature)
  (define-key cmt-mode-map "\M-b"  'cmt-previous-feature)
)


;;;###autoload
(defun cmt-mode ()
  "Major mode for editing CMT requirements files.
See <http://www.cmtsite.org/CMTDoc.html>.

\\{cmt-mode-map}"

  (interactive)

  ; Initializing
  (kill-all-local-variables)

  (use-local-map cmt-mode-map)
  (set-syntax-table cmt-mode-syntax-table)

  ; Setting up font-locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cmt-font-lock-keywords nil t nil nil))

  ; Comment stuff.
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)

  (setq comment-start "#"
	comment-end ""
	comment-start-skip "#\\W*"
	comment-column 48)

  ;; Set menu
  ;; XEmacs addition
  (setq mode-popup-menu cmt-popup-menu)
  (if (featurep 'menubar)
      (progn
	;; make a local copy of the menubar, so our mode doesn't
	;; change the global menubar
	(set-buffer-menubar current-menubar)
	(add-submenu nil cmt-menubar-menu)))
  
  (setq mode-name "CMT"
	major-mode 'cmt-mode)
  (run-hooks 'cmt-mode-hook))


;; Provides
(provide 'cmt-mode)

;;; cmt-mode.el ends here
