;; @(#) swbuff.el -- Quick switch between Emacs buffers.
;; @(#) $Id: swbuff.el,v 1.10 2000/04/18 14:05:26 david_ponce Exp $

(defconst swbuff-version "2.0 (beta1) $Date: 2000/04/18 14:05:26 $"
  "swbuff version information.")

;; This file is not part of Emacs

;; Copyright (C) 1998 by David Ponce
;; Author:       David Ponce <david@dponce.com>
;; Maintainer:   David Ponce <david@dponce.com>
;; Created:      November 12 1998

;; LCD Archive Entry:
;; swbuff|David Ponce|david.ponce@wanadoo.fr|
;; Quick switch between Emacs buffers|
;; $Date: 2000/04/18 14:05:26 $|2.0 (beta1)|~/misc/swbuff.el|

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
;; This package provides commands to quick switch between Emacs buffers.

;;; Installation:
;;
;; Put this file on your Emacs-Lisp load path and add following into your
;; ~/.emacs startup file
;;
;;   (require 'swbuff)

;;; Usage:
;;
;; M-x `swbuff-switch-to-next-buffer' or `C-f6'
;; Switches to the next buffer in the buffer list.
;;
;; M-x `swbuff-switch-to-previous-buffer' or `C-S-f6'
;; Switches to the buffer at the end of the buffer-list.

;;; Customization:
;;
;; M-x `swbuff-customize' to customize all the swbuff options.
;;
;; The following variables could be set:
;;
;; o `swbuff-load-hook'
;;    hook run when package has been loaded. The provided hook
;;    `swbuff-default-load-hook' defines the default key mapping.
;;
;; o `swbuff-clear-delay'
;;    Time in seconds to delay before discarding the status window.
;;
;; o `swbuff-current-buffer-face'
;;    Face used to display the current buffer name in the status window.
;;
;; o `swbuff-exclude-buffer-regexps'
;;    List of regular expressions for excluded buffers.
;;    The default setting excludes buffers whose name begin with a blank character.
;;    To exclude all the internal buffers (that is *scratch*, *Message*, etc...)
;;    you could use the following regexps '("^ .*" "^\*.*\*").

;;; Support:
;;
;; Latest version of swbuff can be downloaded from: <http://www.dponce.com/>
;;
;; Any comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce at <david@dponce.com>
;;
;; I currently develop and test jmaker with NTEmacs 20.6.1 and
;; Cygwin B20.1 tools (bash, make) under MS Windows NT 4 WKS SP5.

;;; Code:

(defconst swbuff-status-buffer-name "*swbuff*"
  "Name of the working buffer used to display the buffer list.")

(defgroup swbuff nil
  "Quick switch between Emacs buffers."
  :group 'tools
  :prefix "swbuff-")

(defcustom swbuff-clear-delay 3
  "*Time in seconds to delay before discarding the status window."
  :group 'swbuff
  :type '(number :tag "seconds")) 

(defface swbuff-current-buffer-face
  '((((class grayscale) (background light)) (:foreground "red" :bold t))
    (((class grayscale) (background dark)) (:foreground "red" :bold t))
    (((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "red" :bold t))
    (t (:bold t)))
  "*Face used to display the switched buffer name in the status window."
  :group 'swbuff)

(defcustom swbuff-exclude-buffer-regexps '("^ ")
  "*List of regular expressions for excluded buffers.
The default setting excludes buffers whose name begin with a blank character.
To exclude all the internal buffers (that is *scratch*, *Message*, etc...) you could
use the following regexps (\"^ \" \"^\\*.*\\*\")."
  :group 'swbuff
  :type '(repeat (regexp :format "%v")))

(defcustom swbuff-load-hook '(swbuff-default-load-hook)
  "*Hook run when package has been loaded.
See also `swbuff-default-load-hook'."
  :group 'swbuff
  :type 'hook)

(defun swbuff-customize ()
  "Show the swbuff customization options panel."
  (interactive)
  (customize-group "swbuff"))

(defun swbuff-include-p (name)
  "Return non-nil if NAME matches none of the `swbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote swbuff-status-buffer-name)
                  swbuff-exclude-buffer-regexps)))
    (while (and rl (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (null rl)))

(defun swbuff-buffer-list ()
  "Return the list of switchable buffers.
That is without the ones whose name matches `swbuff-exclude-buffer-regexps'."
  (let ((sw-buf-list (apply 'nconc
                            (mapcar '(lambda (buf)
                                       (and (swbuff-include-p (buffer-name buf))
                                            (list buf)))
                                    (buffer-list)))))
    (unless (memq (current-buffer) sw-buf-list)
      (setq sw-buf-list (cons (current-buffer) sw-buf-list)))
    sw-buf-list))

(defvar swbuff-buffer-list-string-holder nil
  "Hold the current displayed buffer list.")

(defun swbuff-buffer-list-string ()
  "Convert `swbuff-buffer-list' to a string of buffer names."
  (or swbuff-buffer-list-string-holder
      (setq swbuff-buffer-list-string-holder
            (mapconcat 'buffer-name
                       (swbuff-buffer-list)
                       " "))))

(defun swbuff-make-visible (window position)
  "Adjust horizontal scrolling of WINDOW to ensure that POSITION is visible."
  (save-selected-window
    (select-window window)
    (setq truncate-lines t)
    (let ((wdth (window-width))
          (hscr (window-hscroll))
          (xtra 3))                     ; truncated glyphs + an extra space
      (if (>= position (+ wdth hscr))
          (set-window-hscroll window (- (+ position xtra) wdth))
        (if (< position hscr)
            (set-window-hscroll window (- position xtra)))))))

(defun swbuff-show-status-window ()
  "Show the status window. Contains the list of switchable buffers where
the switched buffer name is highlighted using `swbuff-current-buffer-face'.
If it is not found in the list the function print a message.
The status window is automatically discarded after `swbuff-clear-delay'
elapsed time or any command is executed."
  (let* ((status-line (swbuff-buffer-list-string))
         (name-regexp (concat "\\(^\\| \\)\\("
                              (regexp-quote (buffer-name))
                              "\\)\\($\\| \\)"))
         (start        (and (string-match name-regexp status-line)
                            (match-beginning 2)))
         (end          (and start (match-end 2))))
    (if start
        (with-current-buffer (get-buffer-create swbuff-status-buffer-name)
          (set-text-properties 0 (length status-line) nil status-line)
          (set-text-properties start end '(face swbuff-current-buffer-face) status-line)
          (erase-buffer)
          (insert status-line)
          (let* ((window-min-height 2)
                 (w (or (get-buffer-window swbuff-status-buffer-name)
                        (split-window-vertically -2))))
            (set-window-buffer w (current-buffer))
            (swbuff-make-visible w end)
            (add-hook 'pre-command-hook 'swbuff-pre-command-hook)
            (if (sit-for swbuff-clear-delay)
                (swbuff-discard-status-window))))
      (setq swbuff-buffer-list-string-holder nil)
      (message "No buffers eligible for switching."))))

(defun swbuff-discard-status-window ()
  "Discard the status window."
  (let ((w (get-buffer-window swbuff-status-buffer-name))
        (b (get-buffer swbuff-status-buffer-name)))
    (and w (delete-window w))
    (and b (kill-buffer b))))

(defun swbuff-pre-command-hook ()
  "`pre-command-hook' used to track successive calls to switch commands."
  (when (not (or (eq 'swbuff-switch-to-previous-buffer this-command)
                 (eq 'swbuff-switch-to-next-buffer this-command)))
    (swbuff-discard-status-window)
    (setq swbuff-buffer-list-string-holder nil))
  (remove-hook 'pre-command-hook 'swbuff-pre-command-hook))

(defun swbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((l (swbuff-buffer-list)))
    (and l (switch-to-buffer (nth (1- (length l)) l)))))

(defun swbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((l (nreverse (swbuff-buffer-list))))
    (while (cdr l)
      (switch-to-buffer (car l))
      (setq l (cdr l)))))

;;;###autoload
(defun swbuff-switch-to-previous-buffer ()
  "Switch to the previous buffer in the buffer list."
  (interactive)
  (swbuff-previous-buffer)
  (swbuff-show-status-window))

;;;###autoload
(defun swbuff-switch-to-next-buffer ()
  "Switch to the next buffer in the buffer list."
  (interactive)
  (swbuff-next-buffer)
  (swbuff-show-status-window))

(defun swbuff-default-load-hook ()
  "Default hook run when package has been loaded. It maps the global keys
`C-f6' and `C-S-f6' respectively to the `swbuff-switch-to-next-buffer'
and `swbuff-switch-to-previous-buffer' commands."
  (global-set-key [(control f6)]       'swbuff-switch-to-next-buffer)
  (global-set-key [(control shift f6)] 'swbuff-switch-to-previous-buffer))

(provide 'swbuff)
(run-hooks 'swbuff-load-hook)

;;; Change History:

;;
;; $Log: swbuff.el,v $
;; Revision 1.10  2000/04/18 14:05:26  david_ponce
;; New major version.
;;  * swbuff now uses its own status window to display the buffer list.
;;    This fixes problem using the minibuffer with multiple frames.
;;  * Code cleanup:
;;    No more require cl.
;;    `swbuff-display-version' removed (use C-hv swbuff-version instead)
;;  * Documentation update.
;;
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
