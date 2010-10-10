(deftheme dark-emacs
  "Dark Emacs is designed to be give the same look and feel as `emacs --no-window-system'. Created 2010-10-10.")

(custom-theme-set-variables
 'dark-emacs
 '(org-todo-keyword-faces (quote (("IMP" :background "gold" :foreground "indianred3" :weight bold) ("CNCL" :background "snow3" :foreground "black" :weight bold)))))

(custom-theme-set-faces
 'dark-emacs
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:bold t :weight bold :foreground "salmon"))))
 '(info-menu-header ((t (:bold t :family "Sans Serif" :foreground "tomato" :weight bold))))
 '(info-node ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))
 '(info-xref ((t (:bold t :foreground "powder blue" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(woman-bold ((t (:bold t :weight bold :foreground "forest green"))))
 '(woman-italic ((t (:italic t :slant italic :foreground "salmon"))))
 '(cursor ((t (:background "red"))))
 '(show-paren-mismatch ((t (:foreground "white" :background "magenta"))))
 '(show-paren-match ((t (:background "SlateBlue1"))))
 '(secondary-selection ((t (:background "blue"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "red" :background "ghost white"))))
 '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "blue" :background "azure"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "Red1" :inherit (bold)))))
 '(font-lock-variable-name-face ((t (:foreground "orange" :inherit (bold)))))
 '(font-lock-preprocessor-face ((t (:foreground "IndianRed3" :inherit (font-lock-builtin-face)))))
 '(font-lock-type-face ((t (:foreground "orchid" :inherit (italic)))))
 '(font-lock-keyword-face ((t (:foreground "firebrick1" :inherit (italic)))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "white" :inherit (default)))))
 '(font-lock-constant-face ((t (:foreground "LightGoldenrod1" :inherit (default)))))
 '(font-lock-string-face ((t (:foreground "lawn green" :inherit (default)))))
 '(font-lock-comment-face ((t (:foreground "cyan3" :inherit (italic)))))
 '(font-lock-builtin-face ((t (:foreground "gold" :inherit (default)))))
 '(highlight ((t (:foreground "yellow" :background "blue" :inherit (default)))))
 '(underline ((((supports :underline t)) (:underline t :foreground "green" :inherit (default)))))
 '(bold ((t (:weight bold :inherit (default)))))
 '(bold-italic ((t (:weight bold :inherit (italic)))))
 '(italic ((((supports :slant italic)) (:slant italic :foreground "blanched almond" :inherit (default)))))
 '(default ((t (:foreground "ivory2" :background "black")))))

(provide-theme 'dark-emacs)


;; customise using ((class color) ()) if you wish to use both dark and
;; light backgrounds alternatively. This also takes values like tty
;; and nt to describe text terminals and windows machines.
;; enjoy

 ;; '(cursor ((t (:background "red"))))
 ;; '(show-paren-mismatch ((((class color)) (:foreground "white" :background "magenta"))))
 ;; '(show-paren-match ((((class color) (background dark)) (:background "SlateBlue1"))))
 ;; '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "blue"))))
 ;; '(mode-line-buffer-id ((t (:weight bold :foreground "red" :background "ghost white"))))
 ;; '(mode-line ((((class color) (min-colors 88)) (:box (:line-width -1 :color nil :style released-button) :foreground "blue" :background "azure"))))
 ;; '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:weight bold :foreground "Red1" :inherit (bold)))))
 ;; '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "orange" :inherit (bold)))))
 ;; '(font-lock-preprocessor-face ((t (:foreground "IndianRed3" :inherit (font-lock-builtin-face)))))
 ;; '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "orchid" :inherit (italic)))))
 ;; '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "firebrick1" :inherit (italic)))))
 ;; '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:weight bold :foreground "white" :inherit (default)))))
 ;; '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod1" :inherit (default)))))
 ;; '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "lawn green" :inherit (default)))))
 ;; '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "cyan3" :inherit (italic)))))
 ;; '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "gold" :inherit (default)))))
 ;; '(highlight ((((class color) (min-colors 88) (background dark)) (:foreground "yellow" :background "blue" :inherit (default)))))
 ;; '(underline ((((supports :underline t)) (:underline t :foreground "green" :inherit (default)))))
 ;; '(bold ((t (:weight bold :inherit (default)))))
 ;; '(bold-italic ((t (:weight bold :inherit (italic)))))
 ;; '(italic ((((supports :slant italic)) (:slant italic :foreground "blanched almond" :inherit (default)))))
 ;; '(default ((t (:foreground "ivory2" :background "black")))))
