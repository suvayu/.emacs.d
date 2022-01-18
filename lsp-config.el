;;; lsp-config.el --- LSP customisations

;;; Commentary:

;;; Code:
(require 'lsp-mode)

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [f7] #'lsp-ui-imenu)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c .") #'lsp-signature-activate)
  (define-key lsp-mode-map (kbd "C-c d") #'lsp-describe-thing-at-point))

;;; lsp-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
