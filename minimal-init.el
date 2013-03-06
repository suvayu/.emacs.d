;;; minimal-init.el -- for minimal emacs configuration

;;; Code:
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; because of XFCE clipboard manager bug
;; (setq x-select-enable-clipboard-manager nil)

;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon"))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold)))))


;; configs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(iswitchb-mode t)
 '(savehist-mode t nil (savehist))
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(windmove-wrap-around t))

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; navigate thru windows using M-<arrow>
(windmove-default-keybindings 'meta)

;; lazy-bones
(defalias 'yes-or-no-p 'y-or-n-p)
