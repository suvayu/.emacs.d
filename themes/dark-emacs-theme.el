(deftheme dark-emacs
  "Dark Emacs is designed to give the same look and feel as
  `emacs --no-window-system'. It is based on a theme made by the
  emacswiki user, ZWZ. The original can be found at
  http://www.emacswiki.org/emacs/zwz.

  It also tries to not interfere with the faces when emacs is
  running in a tty (since that is the goal in the first place).
  There are some `org-mode' related faces defined for TODO
  keywords and special tags as well.

  Color theme by Suvayu Ali. Created 2012-04-28.")

(custom-theme-set-variables
 'dark-emacs
 '(org-todo-keyword-faces
   '(("DBUG" . (:background "gold" :foreground "indianred3" :weight bold))
     ("LEAK" . (:background "gold" :foreground "indianred3" :weight bold))
     ("SEGF" . (:background "gold" :foreground "indianred3" :weight bold))
     ("CNCL" . (:background "snow3" :foreground "black" :weight bold))
     ))
 '(org-tag-faces
   '(("PROJ" . (:background "indianred3" :foreground "cornsilk2" :weight bold))
     ))
 )

(custom-theme-set-faces
 'dark-emacs
 '(completions-common-part ((t (:foreground "forest green"))))
 '(completions-first-difference ((t (:weight bold :foreground "salmon"))))
 '(font-lock-warning-face ((((type tty)) (:inherit error :weight bold))))
 '(info-menu-header ((t (:bold t :family "Sans Serif" :foreground "tomato" :weight bold))))
 '(info-node ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))
 '(info-xref ((t (:inherit link :foreground "powder blue" :weight bold))))
 '(info-xref-visited ((t (:foreground "violet" :underline t :weight bold))))
 '(link ((t (:foreground "cyan" :underline t :weight extra-bold))))
 '(minibuffer-prompt ((t (:foreground "dark cyan" :weight bold))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :background "snow" :foreground "DodgerBlue4" :weight bold))))
 '(org-done ((t (:background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
 '(org-inlinetask ((t (:inherit org-level-8 :slant oblique))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "sandy brown" :weight bold))))
 '(org-todo ((t (:background "royalblue4" :foreground "thistle" :weight bold))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))))
 '(woman-bold ((t (:weight bold :foreground "forest green"))))
 '(woman-italic ((t (:slant italic :foreground "salmon"))))
 '(cursor ((((type graphic)) (:background "red"))))
 '(show-paren-mismatch ((((type graphic)) (:foreground "white" :background "magenta"))))
 '(show-paren-match ((((type graphic)) (:background "SlateBlue1"))))
 '(secondary-selection ((((type graphic)) (:background "blue"))))
 '(mode-line-buffer-id ((((type graphic)) (:weight bold :foreground "red" :background "ghost white"))))
 '(mode-line ((((type graphic)) (:box (:line-width -1 :color nil :style released-button) :foreground "blue" :background "azure"))))
 '(font-lock-warning-face ((((type graphic)) (:weight bold :foreground "Red1" :inherit (bold)))))
 '(font-lock-variable-name-face ((((type graphic)) (:foreground "orange" :inherit (bold)))))
 '(font-lock-preprocessor-face ((((type graphic)) (:foreground "IndianRed3" :inherit (font-lock-builtin-face)))))
 '(font-lock-type-face ((((type graphic)) (:foreground "orchid" :inherit (italic)))))
 '(font-lock-keyword-face ((((type graphic)) (:foreground "firebrick1" :inherit (italic)))))
 '(font-lock-function-name-face ((((type graphic)) (:weight bold :foreground "white" :inherit (default)))))
 '(font-lock-constant-face ((((type graphic)) (:foreground "LightGoldenrod1" :inherit (default)))))
 '(font-lock-string-face ((((type graphic)) (:foreground "lawn green" :inherit (default)))))
 '(font-lock-comment-face ((((type graphic)) (:foreground "cyan3" :inherit (italic)))))
 '(font-lock-builtin-face ((((type graphic)) (:foreground "gold" :inherit (default)))))
 '(highlight ((((type graphic)) (:foreground "yellow" :background "blue" :inherit (default)))))
 '(underline ((((supports :underline t)) (:underline t :foreground "green" :inherit (default)))))
 '(bold ((((type graphic)) (:weight bold :inherit (default)))))
 '(bold-italic ((((type graphic)) (:weight bold :inherit (italic)))))
 '(italic ((((supports :slant italic)) (:slant italic :foreground "blanched almond" :inherit (default)))))
 '(default ((((type graphic)) (:foreground "ivory2" :background "black")))))

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
