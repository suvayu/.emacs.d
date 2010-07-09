;;; org-mode-settings.el --- `org-mode' settings and customisations.

;; since using org-mode in /opt/emacs-lisp
(require 'org-install)

;;; Code:

;; autoload, useful to check if `org-mode' is loaded
(autoload 'org-mode-p "org-macs"
  "Check if the current buffer is in Org-mode." t)


;; `org-mode' variable customisations
;; directory to look for agenda files matching `org-agenda-file-regexp'
(setq org-agenda-files '("~/org")
      ;; modifying behaviour of C-a/<home> & C-e/<end>
      org-special-ctrl-a/e t
      ;; log time for TODO state changes
      org-log-done 'time
      ;; update TODO cookies recursively
      ;; use property, ":COOKIE_DATA: todo recursive"
      ;; to set this only for a single subtree
      org-hierarchical-todo-statistics nil)


;; TODO keywords
;; @ - time stamp with note
;; ! - only time stamp
(setq org-todo-keywords
      '((sequence "TODO(t)" "DLAY(l@/!)" "CONT(c)" "|" "DONE(d!)" "CNCL(n@/!)")
	(sequence "WInP(w)" "DBUG(b)" "|" "CMIT(m@)")
	(type "PBUG(p@)" "CBUG(c@)" "SEGF(s@/@)" "|" "FIXD(f@/!)")))

;; TODO keyword faces
(setq org-todo-keyword-faces
      '(("PBUG" . (:background "gold" :foreground "IndianRed3" :weight bold))
	("CBUG" . (:background "gold" :foreground "IndianRed3" :weight bold))
	("SEGF" . (:background "gold" :foreground "IndianRed3" :weight bold))
	("CNCL" . (:background "snow3" :foreground "black" :weight bold))))


;; TAG faces
(setq org-tag-faces
      '(("PROJ" :background "indianred3" :foreground "cornsilk2" :weight bold)
	))
;; (setq org-tag-persistent-alist
;;       '(("physics-bug" . 112)
;; 	("code-bug" . 99)
;; 	("segfault" . 115)
;; 	("Project" . 80)))


;; custom keymap
(defun my-org-mode-keymap()
  "My `org-mode' keymap."
  (local-set-key (kbd "C-<left>") 'outline-up-heading)
  ;; (local-set-key '[(C-right)] 'outline-back-to-heading) ; this one is not interactive
  (local-set-key (kbd "C-<up>") 'outline-previous-visible-heading)
  (local-set-key (kbd "C-<down>") 'outline-next-visible-heading)
  (local-set-key (kbd "s-<up>") 'outline-backward-same-level)
  (local-set-key (kbd "s-<down>") 'outline-forward-same-level))


;; hooks
;; `org-agenda-mode' hook
(defun my-org-agenda-mode-hook()
  "My `org-agenda-mode' hook."
  (visual-line-mode t))
(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

;; `org-mode' hook
(defun my-org-mode-hook()
  "My `org-mode' hook."
  ;; turn on auto-fill
  (turn-on-auto-fill)
  (my-org-mode-keymap))

;;; org-mode-settings.el ends here
