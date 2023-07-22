;;; ivy-config.el --- completion config

;;; Commentary:
;; Source: https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-ivy-deprecated-conf.el

;;; Code:
(require 'ivy)

(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((counsel-rg . ivy--regex-or-literal)
	  (counsel-fzf . ivy--regex-or-literal)
        (t . ivy--regex-plus)))
(setq ivy-initial-inputs-alist '((t . "")))
;; (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
;; (setq ivy-fixed-height-minibuffer nil)

(ivy-set-occur 'swiper-multi 'counsel-ag-occur)

(global-set-key (kbd "M-g '") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "M-s o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches)

(require 'counsel)

(setq counsel-fzf-cmd "fzf -e -f \"%s\"")
(setq counsel-rg-base-command
      '("rg" "--smart-case"
	"--max-columns" "100" "--max-columns-preview"
	"--with-filename" "--no-heading" "--line-number"
	"--color=never" "%s"))
(setq counsel-find-file-occur-cmd; TODO: Simplify this
      "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")

(defun sa-counsel-fzf-rg-files (&optional input dir)
  "Run `fzf' in tandem with `ripgrep' to find files in the present directory.

If invoked from inside a version-controlled repository, then the
corresponding root is used instead.  INPUT is the initial input."
  (interactive)
  (let* ((process-environment
          (cons (concat "FZF_DEFAULT_COMMAND=rg --files --no-follow")
                process-environment))
         (vc (vc-root-dir)))
    (if dir
        (counsel-fzf input dir)
      (if (eq vc nil)
          (counsel-fzf input default-directory)
        (counsel-fzf input vc)))))

(defun sa-counsel-fzf-dir (arg)
  "Set ARG as root directory for `counsel-fzf'."
  (sa-counsel-fzf-rg-files ivy-text
                           (read-directory-name
                            (concat (car (split-string counsel-fzf-cmd))
                                    " in directory: "))))

(defun sa-counsel-rg-dir (arg)
  "Set ARG as root directory for `counsel-rg'."
  (let ((current-prefix-arg '(4)))
    (counsel-rg ivy-text nil "")))

;; TODO: generalise for all relevant file/buffer counsel-*?
(defun sa-counsel-fzf-ace-window (arg)
  "Use `ace-window' on `sa-counsel-fzf-rg-files' candidate ARG."
  (ace-window t)
  (let ((default-directory (if (eq (vc-root-dir) nil)
                               counsel--fzf-dir
                             (vc-root-dir))))
    (if (> (length (aw-window-list)) 1)
        (find-file arg)
      (find-file-other-window arg))
    (balance-windows (current-buffer))))

;; Pass functions as appropriate Ivy actions (accessed via M-o)
(ivy-add-actions
 'counsel-fzf
 '(("r" sa-counsel-fzf-dir "change root directory")
   ("g" sa-counsel-rg-dir "use ripgrep in root directory")
   ("a" sa-counsel-fzf-ace-window "ace-window switch")))

(ivy-set-actions
 'counsel-rg
 '(("r" sa-counsel-rg-dir "change root directory")
   ("z" sa-counsel-fzf-dir "find file with fzf in root directory")))

(ivy-add-actions
 'counsel-find-file
 '(("g" sa-counsel-rg-dir "use ripgrep in root directory")
   ("z" sa-counsel-fzf-dir "find file with fzf in root directory")))

;; Remove commands that only work with key bindings
(put 'counsel-find-symbol 'no-counsel-M-x t)

(global-set-key (kbd "M-g x") 'counsel-M-x)
(global-set-key (kbd "M-g f") 'counsel-find-file)
(global-set-key (kbd "M-g r") 'counsel-recentf)
(global-set-key (kbd "M-g z") 'sa-counsel-fzf-rg-files)
(global-set-key (kbd "M-g m") 'counsel-mark-ring)
(global-set-key (kbd "C-M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-s r") 'counsel-rg)
(global-set-key (kbd "M-s g") 'counsel-git-grep)
;; (define-key ivy-minibuffer-map (kbd "s-y") 'ivy-next-line) ; Avoid 2× `counsel-yank-pop'
(define-key ivy-minibuffer-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'swiper)

;; (setq swiper-action-recenter t)
;; (setq swiper-goto-start-of-match t)
(setq swiper-include-line-number-in-search t)

(global-set-key (kbd "M-s s") 'swiper)
(global-set-key (kbd "M-s w") 'swiper-thing-at-point)
(define-key swiper-map (kbd "M-%") 'swiper-query-replace)

(require 'ivy-rich)

(setq ivy-rich-path-style 'abbreviate)

(setcdr (assq t ivy-format-functions-alist)
        #'ivy-format-function-line)

(add-hook 'after-init-hook 'ivy-rich-mode)

;;; ivy-config.el ends here
