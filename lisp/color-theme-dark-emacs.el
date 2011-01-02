;; Colour theme for Emacs
(eval-when-compile    (require 'color-theme))

(defun color-theme-dark-emacs ()
  "Dark Emacs is designed to be give a look and feel similar to
`emacs --no-window-system'. It is based on a theme made by the
emacswiki user, ZWZ. The original can be found at
http://www.emacswiki.org/emacs/zwz.

Color theme by Suvayu Ali, created 2010-10-10."
  (interactive)
    (color-theme-install
     '(color-theme-dark-emacs
       ((background-color . "black")
	(background-mode . dark)
	(border-color . "black")
	(cursor-color . "red")
	(foreground-color . "ivory2")
	(mouse-color . "black"))
       ((list-matching-lines-buffer-name-face . underline)
	(list-matching-lines-face . match)
	(org-goto-interface . outline)
	(view-highlight-face . highlight)
	(widget-mouse-face . highlight))
       (default ((t (:stipple nil :background "black" :foreground "ivory2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
       (bold ((t (:bold t :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "ivory2" :background "black" :stipple nil :weight bold :height 98))))
       (bold-italic ((t (:italic t :bold t :slant italic :foreground "blanched almond" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :width normal :foundry "unknown" :family "DejaVu Sans Mono" :weight bold :height 98))))
       (border ((t (nil))))
       (buffer-menu-buffer ((t (:bold t :weight bold))))
       (button ((t (:underline t))))
       (calc-nonselected-face ((t (:italic t :foreground "grey70" :slant italic))))
       (calc-selected-face ((t (:bold t :weight bold))))
       (calendar-today ((t (:underline t))))
       (comint-highlight-input ((t (:bold t :weight bold))))
       (comint-highlight-prompt ((t (:foreground "cyan1"))))
       (completions-annotations ((t (:italic t :slant italic :foreground "blanched almond" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (completions-common-part ((t (:foreground "forest green"))))
       (completions-first-difference ((t (:bold t :foreground "salmon" :weight bold))))
       (cursor ((t (:background "red"))))
       (diary ((t (:foreground "yellow1"))))
       (dired-directory ((t (:bold t :weight bold :foreground "white" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (dired-flagged ((t (:bold t :weight bold :foreground "Red1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :height 98))))
       (dired-header ((t (:italic t :foreground "orchid" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (dired-ignored ((t (:foreground "grey70"))))
       (dired-mark ((t (:foreground "LightGoldenrod1" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (dired-marked ((t (:bold t :weight bold :foreground "Red1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :height 98))))
       (dired-perm-write ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "cyan3" :height 98))))
       (dired-symlink ((t (:italic t :foreground "firebrick1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (dired-warning ((t (:bold t :weight bold :foreground "Red1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :height 98))))
       (dropdown-list-face ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :stipple nil :background "lightyellow" :foreground "black" :height 98))))
       (dropdown-list-selection-face ((t (:foreground "black" :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :background "purple" :height 98))))
       (eldoc-highlight-function-argument ((t (:bold t :weight bold :stipple nil :background "black" :foreground "ivory2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (escape-glyph ((t (:foreground "cyan"))))
       (file-name-shadow ((t (:foreground "grey70"))))
       (fixed-pitch ((t (:family "Monospace"))))
       (font-lock-builtin-face ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "gold" :height 98))))
       (font-lock-comment-delimiter-face ((t (:italic t :foreground "cyan3" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (font-lock-comment-face ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "cyan3" :height 98))))
       (font-lock-constant-face ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "LightGoldenrod1" :height 98))))
       (font-lock-doc-face ((t (:foreground "lawn green" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (font-lock-function-name-face ((t (:bold t :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "white" :weight bold :height 98))))
       (font-lock-keyword-face ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "firebrick1" :height 98))))
       (font-lock-negation-char-face ((t (nil))))
       (font-lock-preprocessor-face ((t (:stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "IndianRed3" :height 98))))
       (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold :stipple nil :background "black" :foreground "ivory2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (font-lock-regexp-grouping-construct ((t (:bold t :weight bold :stipple nil :background "black" :foreground "ivory2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (font-lock-string-face ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "lawn green" :height 98))))
       (font-lock-type-face ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "orchid" :height 98))))
       (font-lock-variable-name-face ((t (:bold t :weight bold :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "orange" :height 98))))
       (font-lock-warning-face ((t (:bold t :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "Red1" :weight bold :height 98))))
       (fringe ((t (:background "grey10"))))
       (glyphless-char ((t (:height 0.6))))
       (header-line ((t (:box (:line-width -1 :color nil :style released-button) :background "grey20" :foreground "grey90" :box nil))))
       (help-argument-name ((t (:italic t :slant italic :foreground "blanched almond" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (highlight ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :stipple nil :background "blue" :foreground "yellow" :height 98))))
       (holiday ((t (:background "chocolate4"))))
       (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "gold"))))
       (info-header-xref ((t (:bold t :weight bold :foreground "powder blue"))))
       (info-menu-header ((t (:bold t :foreground "tomato" :weight bold :family "Sans Serif"))))
       (info-menu-star ((t (:foreground "red1"))))
       (info-node ((t (:italic t :bold t :foreground "gold" :slant italic :weight bold))))
       (info-title-1 ((t (:bold t :weight bold :family "Sans Serif" :height 1.728))))
       (info-title-2 ((t (:bold t :family "Sans Serif" :weight bold :height 1.44))))
       (info-title-3 ((t (:bold t :weight bold :family "Sans Serif" :height 1.2))))
       (info-title-4 ((t (:bold t :family "Sans Serif" :weight bold))))
       (info-xref ((t (:bold t :foreground "powder blue" :weight bold))))
       (info-xref-visited ((t (:foreground "violet" :underline t))))
       (isearch ((t (:background "palevioletred2" :foreground "brown4"))))
       (isearch-fail ((t (:background "red4"))))
       (iswitchb-current-match ((t (:bold t :weight bold :foreground "white" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (iswitchb-invalid-regexp ((t (:bold t :weight bold :foreground "Red1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :height 98))))
       (iswitchb-single-match ((t (:italic t :foreground "cyan3" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (iswitchb-virtual-matches ((t (:foreground "gold" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (italic ((t (:italic t :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "blanched almond" :slant italic :height 98))))
       (lazy-highlight ((t (:background "paleturquoise4"))))
       (link ((t (:foreground "cyan1" :underline t))))
       (link-visited ((t (:underline t :foreground "violet"))))
       (match ((t (:background "RoyalBlue3"))))
       (menu ((t (nil))))
       (minibuffer-prompt ((t (:bold t :foreground "dark cyan" :weight bold))))
       (mode-line ((t (:background "azure" :foreground "blue" :box (:line-width -1 :color nil :style released-button)))))
       (mode-line-buffer-id ((t (:bold t :background "ghost white" :foreground "red" :weight bold))))
       (mode-line-emphasis ((t (:bold t :weight bold))))
       (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
       (mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
       (mouse ((t (nil))))
       (next-error ((t (:background "blue3"))))
       (nobreak-space ((t (:foreground "cyan" :underline t))))
       (org-agenda-clocking ((t (:background "blue"))))
       (org-agenda-column-dateline ((t (:family "DejaVu Sans Mono" :weight normal :slant normal :underline nil :strike-through nil :background "grey30" :height 98))))
       (org-agenda-current-time ((t (:bold t :background "snow" :foreground "DodgerBlue4" :weight bold))))
       (org-agenda-date ((t (:foreground "LightSkyBlue"))))
       (org-agenda-date-today ((t (:italic t :bold t :foreground "LightSkyBlue" :slant italic :weight bold))))
       (org-agenda-date-weekend ((t (:bold t :foreground "LightSkyBlue" :weight bold))))
       (org-agenda-diary ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "ivory2" :background "black" :stipple nil :height 98))))
       (org-agenda-dimmed-todo-face ((t (:foreground "grey50"))))
       (org-agenda-done ((t (:foreground "PaleGreen"))))
       (org-agenda-restriction-lock ((t (:background "skyblue4"))))
       (org-agenda-structure ((t (:foreground "LightSkyBlue"))))
       (org-archived ((t (:foreground "grey70"))))
       (org-block ((t (:foreground "grey70"))))
       (org-checkbox ((t (:bold t :weight bold :stipple nil :background "black" :foreground "ivory2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (org-checkbox-statistics-done ((t (:italic t :bold t :weight bold :slant oblique :foreground "DarkSeaGreen2" :background "ForestGreen"))))
       (org-checkbox-statistics-todo ((t (:bold t :weight bold :foreground "thistle" :background "royalblue4"))))
       (org-clock-overlay ((t (:background "SkyBlue4"))))
       (org-code ((t (:foreground "grey70"))))
       (org-column ((t (:background "grey30" :strike-through nil :underline nil :slant normal :weight normal :height 98 :family "DejaVu Sans Mono"))))
       (org-column-title ((t (:bold t :background "grey30" :underline t :weight bold))))
       (org-date ((t (:foreground "Cyan" :underline t))))
       (org-document-info ((t (:foreground "pale turquoise"))))
       (org-document-info-keyword ((t (:foreground "grey70"))))
       (org-document-title ((t (:bold t :foreground "pale turquoise" :weight bold :height 1.44))))
       (org-done ((t (:italic t :bold t :background "ForestGreen" :foreground "DarkSeaGreen2" :slant oblique :weight bold))))
       (org-drawer ((t (:foreground "LightSkyBlue"))))
       (org-ellipsis ((t (:foreground "LightGoldenrod" :underline t))))
       (org-footnote ((t (:foreground "Cyan" :underline t))))
       (org-formula ((t (:foreground "chocolate1"))))
       (org-headline-done ((t (:foreground "LightSalmon"))))
       (org-hide ((t (:foreground "black"))))
       (org-latex-and-export-specials ((t (:foreground "burlywood"))))
       (org-level-1 ((t (:bold t :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "white" :weight bold :height 98))))
       (org-level-2 ((t (:bold t :weight bold :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "orange" :height 98))))
       (org-level-3 ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "firebrick1" :height 98))))
       (org-level-4 ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "cyan3" :height 98))))
       (org-level-5 ((t (:italic t :slant italic :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :foreground "orchid" :height 98))))
       (org-level-6 ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "LightGoldenrod1" :height 98))))
       (org-level-7 ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "gold" :height 98))))
       (org-level-8 ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "lawn green" :height 98))))
       (org-link ((t (:foreground "Cyan" :underline t))))
       (org-meta-line ((t (:italic t :foreground "cyan3" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (org-mode-line-clock ((t (:box (:line-width -1 :color nil :style released-button) :foreground "blue" :background "azure"))))
       (org-mode-line-clock-overrun ((t (:box (:line-width -1 :color nil :style released-button) :foreground "blue" :background "red"))))
       (org-property-value ((t (nil))))
       (org-quote ((t (:foreground "grey70"))))
       (org-scheduled ((t (:foreground "PaleGreen"))))
       (org-scheduled-previously ((t (:foreground "chocolate1"))))
       (org-scheduled-today ((t (:foreground "PaleGreen"))))
       (org-sexp-date ((t (:foreground "Cyan"))))
       (org-special-keyword ((t (:foreground "LightSalmon"))))
       (org-table ((t (:foreground "LightSkyBlue"))))
       (org-tag ((t (:bold t :weight bold))))
       (org-target ((t (:underline t))))
       (org-time-grid ((t (:foreground "LightGoldenrod"))))
       (org-todo ((t (:bold t :background "royalblue4" :foreground "thistle" :weight bold))))
       (org-upcoming-deadline ((t (:foreground "chocolate1"))))
       (org-verbatim ((t (:foreground "grey70"))))
       (org-verse ((t (:foreground "grey70"))))
       (org-warning ((t (:bold t :weight bold :foreground "Red1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :height 98))))
       (outline-1 ((t (:bold t :weight bold :foreground "white" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (outline-2 ((t (:bold t :foreground "orange" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :weight bold :height 98))))
       (outline-3 ((t (:italic t :foreground "firebrick1" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (outline-4 ((t (:italic t :foreground "cyan3" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (outline-5 ((t (:italic t :foreground "orchid" :family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :slant italic :height 98))))
       (outline-6 ((t (:foreground "LightGoldenrod1" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (outline-7 ((t (:foreground "gold" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (outline-8 ((t (:foreground "lawn green" :stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono" :height 98))))
       (query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
       (region ((t (:background "blue3"))))
       (rst-level-1-face ((t (:background "grey85" :foreground "black"))))
       (scroll-bar ((t (nil))))
       (secondary-selection ((t (:background "blue"))))
       (shadow ((t (:foreground "grey70"))))
       (show-paren-match ((t (:background "SlateBlue1"))))
       (show-paren-mismatch ((t (:background "magenta" :foreground "white"))))
       (sr-active-path-face ((t (:bold t :background "#ace6ac" :foreground "yellow" :weight bold :height 120))))
       (sr-alt-marked-dir-face ((t (:bold t :foreground "DeepPink" :weight bold))))
       (sr-alt-marked-file-face ((t (:foreground "DeepPink"))))
       (sr-broken-link-face ((t (:italic t :foreground "red" :slant italic))))
       (sr-clex-hotchar-face ((t (:bold t :foreground "red" :weight bold))))
       (sr-compressed-face ((t (:foreground "magenta"))))
       (sr-directory-face ((t (:bold t :foreground "blue1" :weight bold))))
       (sr-editing-path-face ((t (:bold t :background "red" :foreground "yellow" :weight bold :height 120))))
       (sr-encrypted-face ((t (:foreground "DarkOrange1"))))
       (sr-highlight-path-face ((t (:bold t :background "yellow" :foreground "#ace6ac" :weight bold :height 120))))
       (sr-html-face ((t (:foreground "DarkOliveGreen"))))
       (sr-log-face ((t (:foreground "brown"))))
       (sr-marked-dir-face ((t (:bold t :foreground "red" :weight bold))))
       (sr-marked-file-face ((t (:foreground "red"))))
       (sr-mirror-path-face ((t (:bold t :background "blue" :foreground "yellow" :weight bold :height 120))))
       (sr-packaged-face ((t (:foreground "DarkMagenta"))))
       (sr-passive-path-face ((t (:bold t :background "white" :foreground "lightgray" :weight bold :height 120))))
       (sr-symlink-directory-face ((t (:italic t :foreground "blue1" :slant italic))))
       (sr-symlink-face ((t (:italic t :foreground "DeepSkyBlue" :slant italic))))
       (sr-xml-face ((t (:foreground "DarkGreen"))))
       (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
       (tooltip ((t (:family "Sans Serif" :background "lightyellow" :foreground "black"))))
       (trailing-whitespace ((t (:background "red1"))))
       (underline ((t (:family "DejaVu Sans Mono" :foundry "unknown" :width normal :weight normal :slant normal :overline nil :strike-through nil :box nil :inverse-video nil :background "black" :stipple nil :foreground "green" :underline t :height 98))))
       (variable-pitch ((t (:family "Sans Serif"))))
       (vertical-border ((t (nil))))
       (widget-button ((t (:bold t :weight bold))))
       (widget-button-pressed ((t (:foreground "red1"))))
       (widget-documentation ((t (:foreground "lime green"))))
       (widget-field ((t (:background "dim gray"))))
       (widget-inactive ((t (:foreground "grey70"))))
       (widget-single-line-field ((t (:background "dim gray")))))))
(add-to-list 'color-themes '(color-theme-dark-emacs  "Dark Emacs" "Suvayu Ali"))
