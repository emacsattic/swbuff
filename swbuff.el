;; @(#) swbuff.el -- Quick switch between Emacs buffers.
;; @(#) $Id: swbuff.el,v 1.2 1999/02/01 11:30:30 ebat311 Exp $

;; This file is not part of Emacs

;; Copyright (C) 1998 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      November 12 1998

;; LCD Archive Entry:
;; <el>|David Ponce|david.ponce@wanadoo.fr|
;; <docum>|
;; <date>|$Revision: 1.2 $|~/misc/|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:
;;
;;  This package provides a command to quick switch between Emacs buffers.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'swbuff)

;;; Usage:
;;
;;  M-x `swbuff-switch-to-next-buffer' or `C-f6'
;;     Switches to the next buffer in the buffer list.
;;
;;  M-x `swbuff-switch-to-previous-buffer' or `C-S-f6'
;;     Switches to the buffer at the end of the buffer-list.

;;; Customization:
;;
;;  M-x `swbuff-customize' to customize all the swbuff options.
;;
;;  The following variables could be set:
;;
;;  o `swbuff-load-hook'
;;        hook run when package has been loaded. The provided hook
;;        `swbuff-default-load-hook' defines the default key mapping.
;;
;;  o `swbuff-clear-delay'
;;        Time in seconds to delay before clearing the message in the minibuffer.
;;
;;  o `swbuff-current-buffer-face'
;;        Face used to display the current buffer name in the minibuffer message.

;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of swbuff was developed with NTEmacs 20.3.1 under MS Windows
;;  NT 4 WKS SP3 and also tested with Emacs 20.3 under Sun Solaris 2.5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:
  
(defconst swbuff-version "$Revision: 1.2 $"
  "swbuff version number."
  )

(defgroup swbuff nil
  "swbuff package customization"
  :group 'tools
  :prefix "swbuff-"
  )

(defcustom swbuff-clear-delay 3
  "*Time in seconds to delay before clearing the message in the minibuffer."
  :group 'swbuff
  :type '(number :tag "seconds")
  ) 

(defface swbuff-current-buffer-face
  '((((class grayscale) (background light)) (:foreground "red" :bold t))
    (((class grayscale) (background dark)) (:foreground "red" :bold t))
    (((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "red" :bold t))
    (t (:bold t)))
  "*Face used to display the current buffer name in the minibuffer message."
  :group 'swbuff
  )

(defcustom swbuff-load-hook '(swbuff-default-load-hook)
  "*Hook run when package has been loaded.
See also `swbuff-default-load-hook'."
  :group 'swbuff
  :type 'hook
  )

(defun swbuff-customize ()
  "Customization of the group swbuff."
  (interactive)
  (customize-group "swbuff")
  )

(defun swbuff-version-number ()
  "Returns swbuff version number."
  (string-match "[0123456789.]+" swbuff-version)
  (match-string 0 swbuff-version)
  )

(defun swbuff-display-version ()
  "Displays swbuff version."
  (interactive)
  (message "Using 'swbuff' version %s." (swbuff-version-number))
  )

(defun swbuff-buffer-list-string ()
  "Returns a string of buffer names in the buffer-list.
Buffer names beginning with a ' ' are excluded."
  (mapconcat 'buffer-name 
             (mapcan '(lambda (buffer)
                        (if (char-equal (aref (buffer-name buffer) 0) ?\ )
                            nil
                          (list buffer)))
                     (buffer-list))
             " ")
  )

(defun swbuff-display-buffer-list ()
  "Displays the buffer names string from `swbuff-buffer-string'. The name of
the current buffer is highlighted with the `swbuff-current-buffer-face' face."
  (let* ((display-text (swbuff-buffer-list-string))
         (cur-buf-name (regexp-quote (buffer-name (current-buffer))))
         (start        (string-match cur-buf-name display-text))
         (end          (match-end 0))
         (mini-window (minibuffer-window))
         (mini-buffer (window-buffer mini-window)))
    (set-text-properties start end '(face swbuff-current-buffer-face) display-text)
    (if (minibuffer-window-active-p mini-window)
        (abort-recursive-edit))
    (message nil)
    (with-current-buffer mini-buffer
      (erase-buffer)
      (insert display-text)
      (setq swbuff-mini-buffer mini-buffer)
      (add-hook 'pre-command-hook 'swbuff-pre-command-hook)
      (if (sit-for swbuff-clear-delay)
          (swbuff-pre-command-hook))
      )
    )
  )

(defvar swbuff-mini-buffer nil
  "Holds the minibuffer in which the last display occurs."
  )

(defun swbuff-pre-command-hook ()
  "`pre-command-hook' used to clear the minibuffer in which the last display occurs."
  (when swbuff-mini-buffer
    (with-current-buffer swbuff-mini-buffer (erase-buffer))
    (setq swbuff-mini-buffer nil)
    )
  (remove-hook 'pre-command-hook 'swbuff-pre-command-hook)
  )

(defun swbuff-switch-to-previous-buffer ()
  "Command to switch to the next buffer in the buffer-list."
  (interactive)
  (swbuff-previous-buffer)
  (swbuff-display-buffer-list)
  )

(defun swbuff-previous-buffer ()
  "Displays and activates the buffer at the end of the buffer-list."
  (let ((l (buffer-list)))
    (switch-to-buffer (nth (1- (length l)) l)))
  (if (char-equal (aref (buffer-name (current-buffer)) 0) ?\ )
      (swbuff-previous-buffer))
  )

(defun swbuff-switch-to-next-buffer ()
  "Command to switch to the buffer at the end of the buffer-list."
  (interactive)
  (swbuff-next-buffer)
  (swbuff-display-buffer-list)
  )

(defun swbuff-next-buffer ()
  "Displays and activates the next buffer in the buffer-list."
  (let ((l (nreverse (buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l))))
  (if (char-equal (aref (buffer-name (current-buffer)) 0) ?\ )
      (swbuff-next-buffer))
  )


(defun swbuff-default-load-hook ()
  "Default hook run when package has been loaded. It maps the global keys
`C-f6' and `C-S-f6' respectively to the `swbuff-switch-to-next-buffer'
and `swbuff-switch-to-previous-buffer' commands."
  (global-set-key [C-f6]   'swbuff-switch-to-next-buffer)
  (global-set-key [C-S-f6] 'swbuff-switch-to-previous-buffer)
  )

(provide 'swbuff)
(run-hooks 'swbuff-load-hook)

;;; Change History:

;;
;; $Log: swbuff.el,v $
;; Revision 1.2  1999/02/01 11:30:30  ebat311
;; No more use of `other-buffer' and `bury-buffer' so it
;; can now switch to any buffer in the `buffer-list'.
;;
;; Revision 1.1  1998/11/27 09:12:12  ebat311
;; Initial revision
;;
;;

;;; swbuff.el ends here.
