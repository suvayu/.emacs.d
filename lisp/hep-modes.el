;; -*- mode: emacs-lisp -*-

;; `cmt-mode' for CMT requirements files
(autoload 'cmt-mode "cmt-mode"
  "Mode to fontify and syntax highlight buffer while editing
 CMT requirements file." t)
(add-to-list 'auto-mode-alist (cons "\\requirements\\'" 'cmt-mode))

;; Mode for EvtGen decay files
(load-library "lhcb-dec")
(add-to-list 'auto-mode-alist (cons "\\.dec\\'" 'lhcb-dec-mode))

;; `han-mode' for HAN configuration files
(autoload 'han-mode "han-mode"
  "Major mode for editing HAN configuration files" t)
(add-to-list 'auto-mode-alist
	     (cons "\\(run\\|minutes[0-9]\\{1,3\\}\\).config\\'" 'han-mode))

(provide 'hep-modes)

;; hep-modes.el ends here
