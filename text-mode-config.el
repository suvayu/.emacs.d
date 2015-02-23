;; -*- mode: emacs-lisp -*-
;;; text-mode-settings.el --- settings and customisations for `text-mode'.

;;; Code:

;; text-mode hook
(defun sa-text-mode-hook()
  "My hook for modes which are like `text-mode'."
  ;; wrap long lines (if the window size is small)
  ;; (toggle-truncate-lines t)
  (local-set-key "`" 'skeleton-pair-insert-maybe)
  (unless (or (eq major-mode 'org-mode) (eq major-mode 'latex-mode))
    ;; line folding w/o actually folding it
    (visual-line-mode t)
    ;; text mode abbreviations
    (setq local-abbrev-table text-mode-abbrev-table)
    (local-set-key (kbd "C-M-@") 'mark-end-of-sentence)
    ))
(add-hook 'text-mode-hook 'sa-text-mode-hook)
;; (add-hook 'muse-mode-hook 'sa-text-mode-hook)

;;; text-mode-settings.el ends here
