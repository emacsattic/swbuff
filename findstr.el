;; findstr.el --- igrep-find use findstr if running a Windows NT shell.

;; Copyright (C) 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 21 Jul 2000
;; Version: 1.0
;; Keywords: tools
;; VC: $Id: findstr.el,v 1.1 2000/07/21 13:12:30 david_ponce Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library advise `igrep-find' to use the native Windows NT
;; command findstr to find expressions in multiple files.

;; Installation
;;
;; Put this file on your Emacs load-path and add (require 'findstr)
;; into your Emacs startup file.

;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce at <david@dponce.com>

;;; Change Log:

;; $Log: findstr.el,v $
;; Revision 1.1  2000/07/21 13:12:30  david_ponce
;; The first version.
;;

;;; Code:
(require 'igrep)

(defun findstr-nt-shell-p ()
  "Return non-nil if running a Windows NT shell."
  (and (eq system-type 'windows-nt)
       (let ((case-fold-search t))
         (string-match "cmd\\(\\.exe\\|proxy\\(\\.exe\\)?\\)?" shell-file-name))))
  
(defun findstr (program expression files &optional options)
  "Use the native Windows NT findstr command to find EXPRESSION in
given FILES.  OPTIONS specifies the findstr command options."
  (setq program "findstr")
  (if (null options)
      (setq options ""))
  (if (not (listp files))
      (setq files (list files)))
  (let ((command (format "%s /n %s /c:%s %s"
                         program
                         options
                         (shell-quote-argument expression)
                         (mapconcat 'identity files " "))))
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    (let ((temp last-nonmenu-event))
      (setq last-nonmenu-event t)
      (save-some-buffers (not compilation-ask-about-save) nil)
      (setq last-nonmenu-event temp))
    (message "%S" command)
    (compile-internal command
                      (format "No more %s matches" program)
                      "findstr"
                      nil
                      grep-regexp-alist)
    ))

(defadvice igrep-find (around my-igrep-find first (&rest igrep-args) activate)
  "`igrep-find' use findstr if running a Windows NT shell."
  (interactive
   (let ((igrep-find t)
         (igrep-program (if (findstr-nt-shell-p)
                            "findstr"
                          igrep-program))
         (igrep-options (if (findstr-nt-shell-p)
                            "/s"
                          igrep-options))
         (igrep-read-options (or (findstr-nt-shell-p)
                                 igrep-read-options)))
     (igrep-read-args)))
  (if (findstr-nt-shell-p)
      (apply 'findstr igrep-args)
    ad-do-it))
;;;     (let ((igrep-find t))
;;;       (apply 'igrep igrep-args))))
  
(provide 'findstr)

;;; findstr.el ends here
