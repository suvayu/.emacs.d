;;; org-occur-goto.el -- search open org buffers with an occur interface
;;; Author: Tom <adatgyujto at gmail dot com>

(require 'cl)

;;; Commentary:
;;
;; Usage: M-x oog, then start typing
;;
;; select from the occur matches with up/down/pgup/pgdown and press enter
;;
;; the search string must be at least 3 characters long (by default)

;;; Code:

(defvar oog-idle-delay 0.5)

(defvar oog-minimum-input-length 3)

(defvar oog-map
 (let ((map (copy-keymap minibuffer-local-map)))
   (define-key map (kbd "<down>") 'oog-next-line)
   (define-key map (kbd "<up>") 'oog-previous-line)
   (define-key map (kbd "<prior>") 'oog-previous-page)
   (define-key map (kbd "<next>") 'oog-next-page)
  map))

(defun oog-previous-line ()
  "Goto previous match."
 (interactive)
 (oog-move-selection 'next-line -1))

(defun oog-next-line ()
  "Goto next match."
 (interactive)
 (oog-move-selection 'next-line 1))

(defun oog-previous-page ()
 (interactive)
 (oog-move-selection 'scroll-down nil))

(defun oog-next-page ()
 (interactive)
 (oog-move-selection 'scroll-up nil))

(defun oog-move-selection (movefunc movearg)
  "Move with function call to `MOVEFUNC' with argument `MOVEARG'."
 (let ((win (get-buffer-window "*Occur*")))
   (if win
       (with-selected-window win
         (condition-case nil
             (funcall movefunc movearg)
           (beginning-of-buffer (goto-char (point-min)))
           (end-of-buffer (goto-char (point-max))))))))

(defun oog-check-input ()
  "Read minibuffer input and call occur accordingly with proper check."
(when (sit-for oog-idle-delay)
   (unless (equal (minibuffer-contents) oog-current-input)
     (setq oog-current-input (minibuffer-contents))

     (if (< (length oog-current-input) oog-minimum-input-length)
         (let ((win (get-buffer-window "*Occur*")))
           (if win
             (with-selected-window win
               (setq buffer-read-only nil)
               (erase-buffer))))

       (save-excursion
         (flet ((message (&rest args) nil))  ;; suppress occur messages
           (multi-occur
            (remove nil (mapcar (lambda (buffer)
                                  (with-current-buffer buffer
                                    (if (eq major-mode 'org-mode)
                                        buffer)))
                                (buffer-list)))
            oog-current-input))
         (unless (get-buffer "*Occur*")
           (message "No matches.")))))))

(defun oog ()
  "Use `multi-occur' to navigate within org buffers.
Prompt for a search string and present with an *Occur* buffer navigable from
the minibuffer with arrow and scroll keys.  You can customise the
keymap `oog-map' to bind alternate keys."
 (interactive)
 (let (marker)
   (save-window-excursion
     (add-hook 'post-command-hook 'oog-check-input)
     (setq oog-current-input nil)

     (unwind-protect
         (let ((minibuffer-local-map oog-map))
           (read-string "string: "))

       (remove-hook 'post-command-hook 'oog-check-input))

     (let ((buf (get-buffer "*Occur*")))
       (if buf
           (with-current-buffer buf
             (unless (= (buffer-size) 0)
               (setq marker (occur-mode-find-occurrence)))))))

   (switch-to-buffer (marker-buffer marker))
   (goto-char marker)
   (when (outline-invisible-p)
     (save-excursion
       (outline-previous-visible-heading 1)
       (org-show-subtree)))))

(provide 'org-occur-goto)

;;; org-occur-goto.el ends here
