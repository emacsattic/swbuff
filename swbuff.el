;; @(#) swbuff.el -- Quick switch between Emacs buffers.
;; @(#) $Id: swbuff.el,v 1.9 2000/01/17 10:56:57 ebat311 Exp $

;; This file is not part of Emacs

;; Copyright (C) 1998 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      November 12 1998

;; LCD Archive Entry:
;; swbuff|David Ponce|david.ponce@wanadoo.fr|
;; Quick switch between Emacs buffers|
;; $Date: 2000/01/17 10:56:57 $|$Revision: 1.9 $|~/misc/|

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
;;
;;  o `swbuff-exclude-buffer-regexps'
;;        List of regular expressions for excluded buffers.
;;        The default setting excludes buffers whose name begin with a blank character.
;;        To exclude all the internal buffers (that is *scratch*, *Message*, etc...)
;;        you could use the following regexps '("^ .*" "^\*.*\*").


;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of swbuff was developed with NTEmacs 20.3.1 under MS Windows
;;  NT 4 WKS SP3 and also tested with Emacs 20.3 under Sun Solaris 2.5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:
(require 'cl)

(defconst swbuff-version "$Revision: 1.9 $"
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

(defcustom swbuff-exclude-buffer-regexps '("^ ")
  "*List of regular expressions for excluded buffers.
The default setting excludes buffers whose name begin with a blank character.
To exclude all the internal buffers (that is *scratch*, *Message*, etc...) you could
use the following regexps (\"^ \" \"^\\*.*\\*\")."
  :group 'swbuff
  :type '(repeat (regexp :format "%v"))
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

(defun swbuff-buffer-list ()
  "Returns a buffer list without the ones whose name matches `swbuff-exclude-buffer-regexps'."
  (let ((sw-buf-list
         (mapcan '(lambda (buff)
                    (and
                     (notany '(lambda (rexp)
                                (string-match rexp (buffer-name buff)))
                             swbuff-exclude-buffer-regexps)
                     (list buff)))
                 (buffer-list))))
    (unless (memq (current-buffer) sw-buf-list)
      (setq sw-buf-list (cons (current-buffer) sw-buf-list)))
    sw-buf-list)
  )

(defvar swbuff-buffer-list-string-holder nil
  "Holds the current displayed buffer list.")

(defun swbuff-buffer-list-string ()
  "Returns a string of buffer names in the buffer-list.
Buffer names beginning with a ' ' are excluded."
  (or swbuff-buffer-list-string-holder
      (setq swbuff-buffer-list-string-holder
            (mapconcat 'buffer-name
                       (swbuff-buffer-list)
                       " ")))
  )

(defun swbuff-display-buffer-list ()
  "Displays the buffer names string from `swbuff-buffer-string'. The name of
the current buffer is highlighted with the `swbuff-current-buffer-face' face.
If there are no buffers, then the message is \"No buffers eligible for switching.\""
  (let* ((display-text (swbuff-buffer-list-string))
         (display-size (length display-text))
         (cur-buf-name (concat "\\(^\\| \\)\\("
                               (regexp-quote (buffer-name (current-buffer)))
                               "\\)\\($\\| \\)"))
         (start        (string-match cur-buf-name display-text))
         (start        (match-beginning 2))
         (end          (match-end 2))
         (mini-window (minibuffer-window))
         (mini-buffer (window-buffer mini-window)))
    (if (and start (> display-size 0))
        (if (>= end (* (window-width mini-window) (window-height mini-window)))
            (progn
              (with-current-buffer mini-buffer
                (erase-buffer))
              (setq swbuff-buffer-list-string-holder nil)
              (swbuff-display-buffer-list))
          (progn
            (set-text-properties 0 display-size nil display-text)
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
                  (swbuff-undisplay-buffer-list))
              )))
      (progn
        (setq swbuff-buffer-list-string-holder nil)
        (message "No buffers eligible for switching."))))
  )

(defvar swbuff-mini-buffer nil
  "Holds the minibuffer in which the last display occurs."
  )

(defun swbuff-undisplay-buffer-list ()
  "Clears the minibuffer in which the last display occurs."
  (when swbuff-mini-buffer
    (with-current-buffer swbuff-mini-buffer
      (erase-buffer))
    (setq swbuff-mini-buffer nil)
    )
  )

(defun swbuff-pre-command-hook ()
  "`pre-command-hook' used to clear the minibuffer in which the last display occurs."
  (swbuff-undisplay-buffer-list)
  (if (not (or (eq 'swbuff-switch-to-previous-buffer this-command)
               (eq 'swbuff-switch-to-next-buffer this-command)))
      (setq swbuff-buffer-list-string-holder nil))
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
  (let ((l (swbuff-buffer-list)))
    (and l (switch-to-buffer (nth (1- (length l)) l))))
  )

(defun swbuff-switch-to-next-buffer ()
  "Command to switch to the buffer at the end of the buffer-list."
  (interactive)
  (swbuff-next-buffer)
  (swbuff-display-buffer-list)
  )

(defun swbuff-next-buffer ()
  "Displays and activates the next buffer in the buffer-list."
  (let ((l (nreverse (swbuff-buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l))))
  )

(defun swbuff-default-load-hook ()
  "Default hook run when package has been loaded. It maps the global keys
`C-f6' and `C-S-f6' respectively to the `swbuff-switch-to-next-buffer'
and `swbuff-switch-to-previous-buffer' commands."
  (global-set-key [(control f6)]       'swbuff-switch-to-next-buffer)
  (global-set-key [(control shift f6)] 'swbuff-switch-to-previous-buffer)
  )

(provide 'swbuff)
(run-hooks 'swbuff-load-hook)

;;; Change History:

;;
;; $Log: swbuff.el,v $
;; Revision 1.9  2000/01/17 10:56:57  ebat311
;; Fixed a little problem when switching to next buffer and current buffer
;; is excluded from the list of ones eligible for switching.
;;
;; Thanks to "Joe Casadonte" <joc@netaxs.com> who has reported this.
;;
;; Revision 1.8  1999-07-26 18:54:30+02  ebat311
;; Use Emacs/XEmacs compatible key mapping in `swbuff-default-load-hook'.
;;
;; Revision 1.7  1999-05-17 11:28:45+02  ebat311
;; Improved buffer list display:
;;   - The current highlighted buffer name is always visible.
;;     Previously, when the buffer list exceeded the size
;;     of the mini-buffer window the highlighted buffer name
;;     could be outside the displayed area.
;;   - New buffer name regexp handling to avoid bad highlighting
;;     of buffers which have a common part in their names.
;;
;; Revision 1.6  1999-05-07 13:48:31+02  ebat311
;; Removed a message displayed for debugging purpose.
;;
;; Revision 1.5  1999-05-07 13:45:33+02  ebat311
;; Improved buffer list display - the filenames list is kept static
;; (i.e., not shifting), while the current buffer highlight moves in
;; response to `swbuff-switch-to-next-buffer' and
;; `swbuff-switch-to-previous-buffer'.
;; No switching occurs if the eligible buffer list is empty.
;; Minor typo changes.
;;
;; Revision 1.4  1999-05-07 09:43:02+02  ebat311
;; Fixed a problem when no buffers are eligible for switching.
;; Added (require 'cl) to avoid problem using `mapcan' and
;; `notany' from `cl-extra'.
;; Simplified default exclude regexp from "^ .*" to "^ "
;; Thank you so much to "Paul Ford" <pford@chi.navtech.com>
;; for these fixes.
;;
;; Revision 1.3  1999-05-06 12:13:09+02  ebat311
;; Added a new customisable feature to exclude buffers whose
;; name matches a given list of regular expressions.
;;
;; Fixed - default key binding now works with XEmacs.
;;
;; Revision 1.2  1999-02-01 12:30:30+01  ebat311
;; No more use of `other-buffer' and `bury-buffer' so it
;; can now switch to any buffer in the `buffer-list'.
;;
;; Revision 1.1  1998/11/27 09:12:12  ebat311
;; Initial revision
;;
;;

;;; swbuff.el ends here.
