;;; lsp-config.el --- LSP customisations

;;; Commentary:

;;; Code:
(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
	      ("C-c ." . #'lsp-signature-activate)
	      ("C-c d" . #'lsp-describe-thing-at-point)))

(use-package lsp-ui
  :after lsp-mode
  :config
  :bind
  (:map lsp-ui-mode-map
	([f7] . #'lsp-ui-imenu)
	([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
	([remap xref-find-references] . #'lsp-ui-peek-find-references)))

;;; lsp-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
