;;; copilot-config.el --- GitHub Copilot setup

;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.emacs.d/copilot.el/")
(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "RET") #'copilot-accept-completion)
  (define-key copilot-completion-map [right] #'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-RET") #'copilot-accept-completion-by-line)
  (define-key copilot-mode-map [f5] #'copilot-complete))

;;; copilot-config.el ends here
