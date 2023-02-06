;;; flyc-config.el --- Flycheck config

;;; Commentary:

;;; Code:
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ;; many features are not supported well; e.g. openmp
;; (delete 'c/c++-clang flycheck-checkers)

(add-to-list 'flycheck-checkers 'python-flake8)
(add-to-list 'flycheck-checkers 'python-mypy)

;; navigate flycheck errors w/ hydra
(defhydra hydra-flycerr (flycheck-command-map)
  "goto-flycheck-error"
  ("<down>" flycheck-next-error "next")
  ("<up>" flycheck-previous-error "prev")
  ("<space>" flycheck-list-errors "list")
  ("q" nil "quit"))

;;; flyc-config.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
