;; -*- mode: emacs-lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains some nifty lisp    ;;
;; functions I wrote for my convenience  ;;
;; or I got it from somewhere (credited) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;; modify opacity of emacs frame
;; works with compositing capable window managers
;; Source: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the emacs frame; if DEC is t,
decrease the transparency, otherwise increase it in 5% steps."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	 (oldalpha (if alpha-or-nil alpha-or-nil
		     100))
	 (newalpha (if dec (- oldalpha 5)
		     (+ oldalpha 5))))
    (when (and (>= newalpha frame-alpha-lower-limit)
	       (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-+ will increase opacity (== decrease transparency)
(global-set-key (kbd "C-=")
		'(lambda()
		   (interactive)
		   (djcb-opacity-modify)))

 ;; C-- will decrease opacity (== increase transparency
(global-set-key (kbd "C--")
		'(lambda()
		   (interactive)
		   (djcb-opacity-modify t)))

 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-0")
		'(lambda()
		   (interactive)
		   (modify-frame-parameters nil `((alpha . 100)))))


(provide 'nifty)

;;; end of nifty.el
