;; findstr.el --- Use Windows NT findstr to match expression in files

;; Copyright (C) 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 21 Jul 2000
;; Version: 1.0
;; Keywords: tools
;; VC: $Id: findstr.el,v 1.3 2001/08/03 08:33:30 ponce Exp $

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

;; This library use the Windows NT native command findstr to match
;; expressions in multiple files.

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
;; Revision 1.3  2001/08/03 08:33:30  ponce
;; `findstr' is now Windows specific and no more modify `igrep-find'
;;  behaviour.
;;
;; Many improvements ;-)
;;
;; Revision 1.2  2000/07/21 13:20:44  david_ponce
;; Just changed the findstr `message' format!
;;
;; Revision 1.1  2000/07/21 13:12:30  david_ponce
;; The first version.
;;

;;; Code:
(require 'igrep)
(require 'compile)

;; The following default regexp take into account that Windows file
;; name can begin with a drive letter and contain spaces!
(defvar findstr-regexp-alist
  '(("^\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 3))
  "Regexps to match the `findstr' entries.")

(defun findstr-internal (program expression files &optional options)
  "Use PROGRAM to find EXPRESSION in FILES.
PROGRAM is the native Windows NT findstr command.
OPTIONS specifies the findstr command options."
  (or (stringp options)
      (setq options ""))
  (or (listp files)
      (setq files (list files)))
  (let ((shell-file-name (getenv "ComSpec"))
        (shell-command-switch "/c")
        (w32-quote-process-args nil)
        (command (mapconcat #'identity
                            `(,program ,options ,expression ,@files)
                            " ")))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (message "%s" command)
    (compile-internal command
                      (format "No more %s matches" program)
                      program
                      nil
                      findstr-regexp-alist)))

;;;###autoload
(defun findstr (&rest igrep-args)
  "Run Windows NT findstr to match expression in files.
All arguments IGREP-ARGS are handled by `findstr-internal'."
  (interactive
   (let ((igrep-program "findstr")
         (igrep-options "/n /s")
         (igrep-read-options nil))
     (igrep-read-args)))
  (apply 'findstr-internal igrep-args))

(provide 'findstr)

;;; findstr.el ends here
