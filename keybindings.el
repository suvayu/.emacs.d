;; -*- mode: emacs-lisp -*-
;;; keybindings.el --- on bhishma

;;; Code:

;; Navigation
;; side scrolling on
(put 'scroll-left 'disabled nil)
;; narrow-to-region enabled
(put 'narrow-to-region 'disabled nil)
;; navigate thru windows using M-<arrow>
(windmove-default-keybindings 'meta)

;; minibuffer history completion
(mapc
    #'(lambda (map)
	(define-key map [(meta p)] 'previous-complete-history-element)
	(define-key map [(meta n)] 'next-complete-history-element))
    (nconc (list minibuffer-local-completion-map
                 minibuffer-local-isearch-map
                 minibuffer-local-map
                 minibuffer-local-must-match-map
		 minibuffer-local-ns-map)))

;; `occur-mode' customisations
(define-key occur-mode-map (kbd "TAB") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "f") 'next-error-follow-minor-mode)

;; Isearch in other-window
(global-set-key (kbd "M-s C-s") 'sa-isearch-forward-other-window)
(global-set-key (kbd "M-s C-r") 'sa-isearch-backward-other-window)
(global-set-key (kbd "M-s C-M-s") 'sa-isearch-forward-regexp-other-window)
(global-set-key (kbd "M-s C-M-r") 'sa-isearch-backward-regexp-other-window)
;; NB: C-c C-s was bound to (c-show-syntactic-information ARG) in c-mode

(global-set-key '[(C-mouse-4)] 'text-scale-increase) ; scroll up
(global-set-key '[(C-mouse-5)] 'text-scale-decrease) ; scroll down


;; transpose-* keybindings
;; (global-set-key (kbd "C-<up>") 'transpose-lines)
;; (global-set-key (kbd "C-<down>") '(lambda()
;; 				  (interactive)
;; 				  (transpose-lines -1)))
(global-set-key (kbd "M-}") 'transpose-paragraphs)
(global-set-key (kbd "M-{") '(lambda()
			       (interactive)
			       (transpose-paragraphs -1)))

;; Editing
;; prefer utf-8
(prefer-coding-system 'utf-8)
;; enabling disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; tabs with respect to the previous non-blank line
;; (define-key global-map (kbd "S-<iso-lefttab>") 'indent-relative)
(define-key global-map (kbd "<backtab>") 'indent-relative)
;; (global-set-key '[(backtab)] 'indent-relative)

;; killing blocks of text
(global-set-key (kbd "C-c M-k") 'kill-paragraph)

;; inserting unicode
(require 'ucs-cmds)
(define-key global-map [remap ucs-insert] 'ucsc-insert)

;; undo-tree (not in vanilla Emacs)
(require 'undo-tree)
(global-undo-tree-mode)

;; ;; browse-kill-ring (not in vanilla Emacs)
;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)

;; kill-ring-search
(autoload 'kill-ring-search "kill-ring-search"
 "Search the kill ring in the minibuffer."
 (interactive))
(global-set-key (kbd "M-C-y") 'kill-ring-search)

;; ;; FIXME:
;; (defadvice isearch-yank-kill
;; (around toggle-case-fold-search-maybe)
;; "If case-fold-search is t toggle it. Restore it after isearch finishes."
;; (let ((tmp-case-fold-search case-fold-search))
;;   (if case-fold-search
;;       (progn (setq case-fold-search nil)
;; 	     ad-do-it)
;;     ad-do-it)
;;   (setq case-fold-search tmp-case-fold-search)))
;; (ad-activate 'isearch-yank-kill)


;; Skeleton pair insert
(setq skeleton-pair t)
;; (setq skeleton-pair-alist '((?( _ ?)) (?\))
;; 			    (?[ _ ?]) (?\])
;; 			    (?{ _ ?}) (?\})
;; 			    (?< _ ?>) (?\>)
;; 			    (?` _ ?')))

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)


;;; Abbreviations customisations
;; expand abbrevs in context
(defun sa-expand-abbrev-in-context (expand)
  "Expands abbreviations according to the context. Determines
whether within comments or source by looking at the face name. If
within comments the `basic-text-mode-abbrev-table' is used, the
major mode abbrev-table is used otherwise.

Expansion is done by the function passed as the argument. This is
controlled by the \"abnormal\" hook `abbrev-expand-functions'."
;; backward-char checks if end-of-buffer as when point at e-o-b face is `nil'
;; the function call expand does the expansion, usually `expand-abbrev'
  (if (not (save-excursion
	     (string-match "comment\\|string"
			   (symbol-name (if (< (point) (point-max))
					    (face-at-point)
					  (backward-char)
					  (face-at-point))))))
      (funcall expand)
    (let ((local-abbrev-table basic-text-mode-abbrev-table))
      (funcall expand))))

;; this hook wraps around the `expand-abbrev' function call
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (add-hook 'abbrev-expand-functions 'sa-expand-abbrev-in-context nil t)))


;; dired
(put 'dired-find-alternate-file 'disabled nil)

;; `dired-details' & `dired-details+' by Drew Adams
;; reported a bug for this 1 ;) (not in vanilla Emacs)
(require 'dired-details)
(require 'dired-details+)
(require 'dired-sort-menu)
(require 'dired-sort-menu+)
(require 'dired-sort-map)

;; ;; load dired-x in dired (enables omitting files)
;; (add-hook 'dired-load-hook
;;	  (lambda ()
;;	    (require 'dired-x)))
;; ;; turns omiting on by default
;; (setq dired-omit-mode t)

;; `dired-mode' key bindings ; okay here since dired is required above
(define-key dired-mode-map (kbd "C-<down>") 'dired-next-subdir)
(define-key dired-mode-map (kbd "C-<up>") 'dired-prev-subdir)
(define-key dired-mode-map (kbd "C-<left>") 'dired-tree-up)
(define-key dired-mode-map (kbd "C-<right>") 'dired-tree-down)
(define-key dired-mode-map (kbd "<tab>") 'dired-hide-subdir)

;; lazy-bones
(defalias 'yes-or-no-p 'y-or-n-p)
;; session management (not in vanilla Emacs).  Eventhough not a
;; keybinding, session settings stays here because of lazy-bones
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; coding system to use when writing `session-save-file'
(setq session-save-file-coding-system 'utf-8)
