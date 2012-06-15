;; -*- mode: emacs-lisp -*-

;; setup keybindings in mode hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key "[" 'skeleton-pair-insert-maybe)
	    (local-set-key "'" 'skeleton-pair-insert-maybe)
	    (local-set-key (kbd "M-RET") 'newline-and-indent)
	    (define-key python-mode-map (kbd "M-a") 'python-beginning-of-block)
	    (define-key python-mode-map (kbd "M-e") 'python-end-of-block)
	    (define-key python-mode-map (kbd "C-c h") 'pylookup-lookup)
	    ))

;; setup pylookup for easy access html docs from emacs
;; (make sure pylookup.el is in load-path)
(setq pylookup-program "~/build/pylookup/pylookup.py")
(setq pylookup-db-file "~/build/pylookup/pylookup.db")

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development tools: Pymacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Pymacs setup
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; ;;(eval-after-load "pymacs"
;; ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
