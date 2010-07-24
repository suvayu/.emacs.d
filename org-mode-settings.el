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
      ;; log time on rescheduling
      org-log-reschedule 'time
      ;; update TODO cookies recursively
      ;; use property, ":COOKIE_DATA: todo recursive"
      ;; to set this only for a single subtree
      org-hierarchical-todo-statistics nil)

;; show links as inline images using `iimage-mode'
(load-library "iimage")
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]")  1))

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
      (set-face-underline-p 'org-link t))
  (iimage-mode))


;; TODO keywords
;; @ - time stamp with note
;; ! - only time stamp
(setq org-todo-keywords
      '((sequence "TODO(t)" "DLAY(l@/!)" "CONT(c!)" "|" "DONE(d@)" "CNCL(n@/!)")
	(sequence "WInP(w!)" "DBUG(b!)" "|" "CMIT(m@)")
	(type "PBUG(p@)" "CBUG(c@)" "SEGF(s@/@)" "|" "FIXD(f@/!)")))

;; TODO keyword faces
(setq org-todo-keyword-faces
      '(("PBUG" . (:background "gold" :foreground "indianred3" :weight bold))
	("CBUG" . (:background "gold" :foreground "indianred3" :weight bold))
	("SEGF" . (:background "gold" :foreground "indianred3" :weight bold))
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


;; changing org-mode behaviour
;; defadvising org-mode commands
(defadvice outline-forward-same-level
  (around outline-forward-same-level-or-next-visible (arg))
  "If its the last outline sibling, move to the next visible outline heading."
  (if (save-excursion
	 (outline-get-next-sibling))
      ad-do-it
    (outline-next-visible-heading arg)))
(ad-activate 'outline-forward-same-level)

(defadvice outline-backward-same-level
  (around outline-backward-same-level-or-previous-visible (arg))
  "If its the last outline sibling, move to the previous visible outline heading."
  (if (save-excursion
	 (outline-get-last-sibling))
      ad-do-it
    (outline-previous-visible-heading arg)))
(ad-activate 'outline-backward-same-level)


;; global keymaps
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; `org-mode' keymaps
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

;; FIXME
(defun yas-org-setup
  "Activate yasnippet keybinds."
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (define-key yas/keymap [tab] 'yas/next-field-group))

;; `org-mode' hook
(defun my-org-mode-hook()
  "My `org-mode' hook."
  ;; turn on auto-fill
  (turn-on-auto-fill)
  (flyspell-mode t)
  (my-org-mode-keymap)
  ;; FIXME
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key [tab])
  (define-key yas/keymap [tab] 'yas/next-field-group))
;  (yas-org-setup))


;; Setup `org-babel' for emacs-lisp, gnuplot, latex and shell-script.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (sh . t)))

;; (defun setup-org-babel()
;;   (interactive)
;;   (org-babel-load-library-of-babel))


;;; org-mode-settings.el ends here
