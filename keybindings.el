;;; keybindings.el --- my keybindings

;;; Commentary:

;;; Code:
(require 'nifty)
(require 'hydra)
(require 'neotree)
(require 'helm-rg)

;;; Navigation
;; side scrolling on
(put 'scroll-left 'disabled nil)
;; narrow-to-region enabled
(put 'narrow-to-region 'disabled nil)
;; navigate thru windows using M-<arrow>
(windmove-default-keybindings 'meta)

;; ;; define personal keymap
;; (define-prefix-command 'sa-map)
;; FIXME: find alternate for C-z
;; (define-key global-map (kbd "C-z") 'sa-map)

;; sexp movement (defaults commented out), overruled by smartparens
;; (global-set-key (kbd "C-M-n") 'forward-list)
;; (global-set-key (kbd "C-M-p") 'backward-list)
;; (global-set-key (kbd "C-M-d") 'down-list)
(global-set-key (kbd "C-M-u") 'up-list)
;; (global-set-key (kbd "C-M-f") 'forward-sexp)
;; (global-set-key (kbd "C-M-b") 'backward-sexp)

;; tree based directory browsing
(global-set-key [f9] 'neotree-toggle)

;; navigate frames
(defhydra hydra-framenav (global-map "C-x")
  "frame-nav"
  ("C-<left>" (other-frame -1) "prev")
  ("C-<right>" (other-frame 1) "next")
  ("q" nil "quit"))

;; navigate errors w/ hydra
;; see flyc-config.el for flycheck error navigation
(define-key global-map (kbd "M-g /") 'first-error) ; mnemonic: `?'
(defhydra hydra-error (global-map "M-g")
  "goto-error"
  ("." first-error "first")
  ("<down>" next-error "next")
  ("<up>" previous-error "prev")
  ("q" nil "quit"))

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

;; Isearch in other-window (TODO: hydrafy)
(global-set-key (kbd "M-s C-s") 'sa-isearch-forward-other-window)
(global-set-key (kbd "M-s C-r") 'sa-isearch-backward-other-window)
(global-set-key (kbd "M-s C-M-s") 'sa-isearch-forward-regexp-other-window)
(global-set-key (kbd "M-s C-M-r") 'sa-isearch-backward-regexp-other-window)
;; NB: C-c C-s was bound to (c-show-syntactic-information ARG) in c-mode

;; ripgrep
(global-set-key (kbd "C-x g") 'helm-rg)
(define-key helm-rg-map (kbd "<left>") 'backward-char)
(define-key helm-rg-map (kbd "<right>") 'forward-char)


;;; Editing
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

;; transpose-* keybindings
(defhydra hydra-paras (global-map "C-x")
  "drag-paras"
  ("M-}" transpose-paragraphs "down")
  ("M-{" (progn
	   (transpose-paragraphs -1)
	   (backward-paragraph)) "up")
  ("q" nil "quit"))

;; drag lines
(global-set-key (kbd "C-x C-<up>") 'sa-transpose-lines-up)
(global-set-key (kbd "C-x C-<down>") 'sa-transpose-lines-down)
(global-set-key (kbd "C-<up>") 'sa-backward-paragraph)
(global-set-key (kbd "C-<down>") 'sa-forward-paragraph)

;; inserting unicode
(require 'ucs-cmds)
(define-key global-map [remap ucs-insert] 'ucsc-insert)

;; undo-tree (not in vanilla Emacs)
(require 'undo-tree)
(global-undo-tree-mode)

;; kill-ring-search
(autoload 'kill-ring-search "kill-ring-search"
 "Search the kill ring in the minibuffer." t)
(global-set-key (kbd "M-C-y") 'kill-ring-search)

;; parallel kill ring, by Michael Heerdegen (see nifty.el)
(advice-add 'yank :before #'sa-yank--before-ad)
(global-set-key [(meta ?y)] #'sa-yank-pop)

;; context sensitive M-=
(global-set-key (kbd "M-=") #'sa-calc-or-count)

(require 'smartparens-config)
(smartparens-global-mode 1)
;; change `sp-smartparens-bindings' defaults
(cl-loop for key in '([C-left] [C-right]) do
	 (define-key sp-keymap key nil))
(define-key sp-keymap (kbd "C-c <up>") 'sp-rewrap-sexp)

(defhydra hydra-sp (sp-keymap "C-c")
  "sp-slurp/barf-fwd/bkwd"
  ("<right>" sp-forward-slurp-sexp "slurp forward")
  ("<left>" sp-backward-slurp-sexp "slurp backward")
  ("C-<left>" sp-forward-barf-sexp "barf forward")
  ("C-<right>" sp-backward-barf-sexp "barf backward")
  ("q" nil "quit"))

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


;;; Abbreviations customisations
;; expand abbrevs in context
(defun sa-expand-abbrev-in-context (expand)
  "Expands the abbreviation EXPAND according to the context.

Determines whether within comments or source by looking at the
face name.  If within comments the `basic-text-mode-abbrev-table'
is used, the major mode abbrev-table is used otherwise.

Expansion is done by the function passed as the argument.  This
is controlled by the \"abnormal\" hook `abbrev-expand-functions'."
;; backward-char checks if end-of-buffer as when point at e-o-b face is `nil'
;; the function call expand does the expansion, usually `expand-abbrev'
  (if (save-excursion
	(string-match "comment\\|string"
		      (symbol-name (if (< (point) (point-max))
				       (face-at-point)
				     (backward-char)
				     (face-at-point)))))
      (let ((local-abbrev-table basic-text-mode-abbrev-table))
	(funcall expand))
    (funcall expand)))

;; Disable abbrev expand in comments
;; ;; this hook wraps around the `expand-abbrev' function call
;; (add-hook 'after-change-major-mode-hook
;; 	  (lambda ()
;; 	    (add-hook 'abbrev-expand-functions 'sa-expand-abbrev-in-context nil t)))


;;; Dired
(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'dired
  ;; `dired-mode' key bindings
  (define-key dired-mode-map (kbd "C-<down>") 'dired-next-subdir)
  (define-key dired-mode-map (kbd "C-<up>") 'dired-prev-subdir)
  (define-key dired-mode-map (kbd "C-<left>") 'dired-tree-up)
  (define-key dired-mode-map (kbd "C-<right>") 'dired-tree-down)
  ;; (define-key dired-mode-map (kbd "<tab>") 'dired-hide-subdir) ; doesn't work
  )

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; lazy-bones
(defalias 'yes-or-no-p 'y-or-n-p)
;; session management (not in vanilla Emacs).  Eventhough not a
;; keybinding, session settings stays here because of lazy-bones
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; coding system to use when writing `session-save-file'
(setq session-save-file-coding-system 'utf-8)
(global-set-key (kbd "C-x C-_") 'session-jump-to-last-change)


;;; sx
;; keybindings
(add-hook 'sx-question-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<down>") 'next-line)
	    (local-set-key (kbd "<up>") 'previous-line)))

;;; keybindings.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
