;;; text-mode-like-settings.el --- settings and customisations for modes like `text-mode'.

;;; Code:

;; w3m key bindings
(defun my-w3m-mode-hook ()
  "Set up some w3m tabbed browsing key bindings."
  (toggle-truncate-lines t)
  (define-key w3m-mode-map (kbd "C-<tab>") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "C-<backtab>") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "C-S-<iso-lefttab>") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "s-<tab>") 'w3m-select-buffer))

;; text-mode hook
(defun my-text-mode-hook()
  "My hook for modes which are like `text-mode'."
  ;; wrap long lines (if the window size is small)
  ;; (toggle-truncate-lines t)
  (unless (eq major-mode 'org-mode)
    ;; turn on orgtbl minor mode
    (turn-on-orgtbl)
    ;; turn on enhanced orgstruct minor mode
    (turn-on-orgstruct++)
    ;; line folding w/o actually folding it
    (visual-line-mode t)
    ;; text mode abbreviations
    (setq local-abbrev-table text-mode-abbrev-table)
    ))

;;; text-mode-like-settings.el ends here
