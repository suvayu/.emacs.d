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
    '(lambda (map)
       (define-key map [(meta p)] 'previous-complete-history-element)
       (define-key map [(meta n)] 'next-complete-history-element))
    (nconc (list minibuffer-local-completion-map
                 minibuffer-local-isearch-map
                 minibuffer-local-map
                 minibuffer-local-must-match-map
		 minibuffer-local-ns-map)))

;; `occur-mode' customisations
(define-key occur-mode-map (kbd "TAB") 'occur-mode-display-occurrence)


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
(global-set-key "\M-\C-y" 'kill-ring-search)

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


;; Skeletons (and abbrev customisations)
(load-file "~/.emacs.d/lisp/skeletons.el")

;; Abbreviations
;; this hook wraps around the `expand-abbrev' function call
(add-hook 'after-change-major-mode-hook
	  (lambda ()
	    (add-hook 'abbrev-expand-functions 'sa-expand-abbrev-in-context nil t)))


;; font-lock customisations
(defface font-lock-global-todo-face
  '((t (:background "royalblue4" :foreground "thistle" :weight bold)))
  "Face for the TODO keyword globally."
  :group 'font-lock-faces)

(add-hook 'find-file-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil '(("\\<\\(FIXME\\)[: ]" 1 font-lock-warning-face prepend)
		   ;; ("\\<\\(NB\\|TODO\\)[: ]" 1 font-lock-global-todo-face prepend)
		   ))))


;; version control related customisations
;; auto-revert-mode for files under version control
(add-hook 'find-file-hook
	  (lambda ()
	    (if (vc-working-revision (buffer-file-name)) ; (egg-buf-git-name)
		(auto-revert-mode t))
	    ))

;; mode to edit git commit message
(autoload 'git-commit-mode "git-commit"
  "Major mode for editing git commit messages." t)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))
(add-hook 'git-commit-mode-hook
	  (lambda () (turn-on-orgstruct++)))


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

;; `dired-mode' key bindings
(define-key dired-mode-map (kbd "C-<down>") 'dired-next-subdir)
(define-key dired-mode-map (kbd "C-<up>") 'dired-prev-subdir)
(define-key dired-mode-map (kbd "C-<left>") 'dired-tree-up)
(define-key dired-mode-map (kbd "C-<right>") 'dired-tree-down)
(define-key dired-mode-map (kbd "<tab>") 'dired-hide-subdir)


;; lazy-bones
(defalias 'yes-or-no-p 'y-or-n-p)
;; session management (not in vanilla Emacs)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; coding system to use when writing `session-save-file'
(setq session-save-file-coding-system 'utf-8)

;; Show clock in the modeline
(display-time-mode 1)
