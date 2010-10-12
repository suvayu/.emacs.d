;; -*- mode: emacs-lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains some nifty lisp   ;;
;; functions I wrote for my convenience ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert Emacs version
(defun which-emacs ()
  "Inserts a shortened output of the `emacs-version' at point."
  (interactive)
  (setq str (emacs-version))
  (insert (substring
	   str 0 (string-match "(.+)" str))
	  (format
	   "(%s)" (progn
		    (string-match "[0-9]+-[0-9]+-[0-9]+" str)
		    (match-string 0 str)
		    ))))


;; make ATLAS GoodRunList
(defun make-GRL (beg end)
  "Convert the marked region of a ATLAS GoodRunList XML to C
if..else source blocks."
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
	 "\\(<LumiBlockCollection>\n.+<Run>\\)\\([0-9]+\\)\\(</Run>\\)" nil t)
      (replace-match "if ( Run == \\2 ) {" nil nil))
    (goto-char beg)
    (while (re-search-forward
	    "\\(^ +\\)</LumiBlockCollection>"
	    nil t)
      (replace-match "\\1  return false;\n\\1}" nil nil))
    (goto-char beg)
    (while (re-search-forward
	    "\\(<LBRange Start=\"\\)\\([0-9]+\\)\\(\" End=\"\\)\\([0-9]+\\)\\(\"/>\\)"
	    nil t)
      (replace-match "if ( LumiBlock >= \\2 && LumiBlock <= \\4) return true;" nil nil))
    (remove-overlays beg end)
    (message "Coverted GRL XML to C if..else blocks.")
    ))

(provide 'nifty)

;;; end of nifty.el