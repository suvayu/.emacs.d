;;; minimal.el --- Minimalist Emacs appearance

;; Copyright (C) 2010  Dan Davison

;; Author: Dan Davison <dandavison7 at gmail dot com>
;; Keywords: appearance, minimalism

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; minimalists may also like:
;;
;; (nconc default-frame-alist '((cursor-type . bar)))
;; (setq inhibit-startup-message t)
;; (blink-cursor-mode -1)

;;; Code:

(defvar minimal-zap-mode-line t
  "Should the mode line be turned into a thin separator line?")

(defvar minimal-zap-scroll-bar t
  "Should the scroll bars be removed?")

(defvar minimal-zap-menu-bar t
  "Should the menu bar be removed?")

(defvar minimal-zap-tool-bar t
  "Should the tool bar be removed?")

(defvar minimal-mode-line-background "darkred"
  "Background colour for active mode line face when minimal minor
  mode is active")

(defvar minimal-mode-line-inactive-background "dim grey"
  "Background colour for inactive mode line face when minimal
  minor mode is active")

(defvar minimal-mode-line-height 0.1
  "Height of mode line when minimal minor mode is active")

(defvar minimal-mode-hook nil
  "Hook run after switching to `minimal-mode'")

(defvar minimal-mode-map (make-sparse-keymap))
(defvar minimal-zapped-scroll-bar nil)
(defvar minimal-zapped-menu-bar nil)
(defvar minimal-zapped-tool-bar nil)

(define-minor-mode minimal-mode
  "Minimalist visual appearance.

\\{minimal-mode-map}"
  nil " min" nil
  (cond
   (minimal-mode
    ;; turn on
    (when minimal-zap-mode-line
      (minimal-save-and-zap-mode-line)
      (unless (facep 'minimal-mode-line)
	(copy-face 'mode-line 'minimal-mode-line))
      (set-face-attribute 'minimal-mode-line nil
			  :background minimal-mode-line-background
			  :height minimal-mode-line-height)
      (setq face-remapping-alist
	    (cons '(mode-line minimal-mode-line)
		  (assq-delete-all 'mode-line face-remapping-alist)))
      (unless (facep 'minimal-mode-line-inactive)
	(copy-face 'mode-line-inactive 'minimal-mode-line-inactive))
      (set-face-attribute 'minimal-mode-line-inactive nil
			  :background minimal-mode-line-inactive-background
			  :height minimal-mode-line-height)
      (setq face-remapping-alist
	    (cons '(mode-line-inactive minimal-mode-line-inactive)
		  (assq-delete-all 'mode-line-inactive face-remapping-alist))))
    (when (and scroll-bar-mode minimal-zap-scroll-bar)
      (setq minimal-zapped-scroll-bar t)
      (scroll-bar-mode -1))
    (when (and menu-bar-mode minimal-zap-menu-bar)
      (setq minimal-zapped-menu-bar t)
      (menu-bar-mode -1))
    (when (and tool-bar-mode minimal-zap-tool-bar)
      (setq minimal-zapped-tool-bar t)
      (tool-bar-mode -1))
    (add-hook 'after-change-major-mode-hook 'minimal-mode))
   (t
    ;; turn off
    ;; TODO: This turns off on a per-buffer basis.
    ;; how to turn off for all buffers?
    (when minimal-zap-mode-line
       (setq face-remapping-alist
	    (assq-delete-all 'mode-line
			     (assq-delete-all 'mode-line-inactive
					      face-remapping-alist))))
    (minimal-restore-mode-line)
   (when (and (not scroll-bar-mode) minimal-zapped-scroll-bar)
      (scroll-bar-mode +1))
    (when (and (not menu-bar-mode) minimal-zapped-menu-bar)
      (menu-bar-mode +1))
    (when (and (not tool-bar-mode) minimal-zapped-tool-bar)
      (tool-bar-mode +1)))
   (remove-hook 'after-change-major-mode-hook 'minimal-mode)))

(defun minimal-save-and-zap-mode-line (&optional buffer)
  ;; Save buffer's `mode-line-format' and remove mode line
  (interactive)
  (when buffer (set-buffer buffer))
  (unless (and (stringp mode-line-format) (string= mode-line-format "")
	       (not (minibufferp)))
    (set (make-local-variable 'minimal-saved-mode-line-format) mode-line-format)
    (setq mode-line-format "")))

(defun minimal-restore-mode-line (&optional buffer)
  ;; Restore buffer's saved `mode-line-format'
  (interactive)
  (when buffer (set-buffer buffer))
  (if (and (stringp mode-line-format) (string= mode-line-format "")
	   (not (minibufferp)))
      (if (boundp 'minimal-saved-mode-line-format)
	  (setq mode-line-format minimal-saved-mode-line-format)
	(message "Failed to restore `mode-line-format' in buffer %s" (buffer-name)))))

(provide 'minimal)
;; minimal.el ends here
