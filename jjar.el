;; @(#) jjar.el -- Java Archive builder
;; @(#) $Id: jjar.el,v 1.3 1999/07/06 15:31:27 ebat311 Exp $

;; This file is not part of Emacs

;; Copyright (C) 1999 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      June 14 1999

;; LCD Archive Entry:
;; jjar|David Ponce|david.ponce@wanadoo.fr|
;; Java Archive builder|
;; $Date: 1999/07/06 15:31:27 $|$Revision: 1.3 $|~/misc/jjar.el|

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
;;  This elisp package provides commands to create or update (JDK 1.2) a
;;  Java ARrchive files (JAR). The files to be archived are searched in choosen
;;  sub-directories of a given base directory and selected by customized shell
;;  wildcards.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jjar)

;;; Usage:
;;
;;  M-x `jjar-create'
;;     To create a JAR file.
;;
;;  M-x `jjar-update'
;;     To update an existing JAR file (JDK 1.2).

;;; Customization:
;;
;;  M-x `jjar-customize' to customize all the jjar options.
;;
;;  The following variables could be set:
;;
;;  o `jjar-load-hook'
;;        hook run when package has been loaded. The provided hook
;;        `jjar-default-load-hook' does nothing.
;;
;;  o `jjar-jar-command'
;;        Specifies the path to the the jar command to be used.
;;
;;  o `jjar-jar-nocompress-option'
;;        If non-nil (default) jar uses no ZIP compression.
;;
;;  o `jjar-jar-verbose-option'
;;        If non-nil (default) jar generates verbose output on standard error.
;;
;;  o `jjar-include-wildcards'
;;     List of predefined shell wildcard expressions used to locate files.
;;     The default setting includes .class and .properties files.
;;
  
;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  Developed with NTEmacs 20.3.10 on MS Windows NT 4 WKS SP5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:

(require 'compile)
(require 'cl)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(defconst jjar-version "$Revision: 1.3 $"
  "jjar version number."
  )

(defgroup jjar nil
  "jjar package customization."
  :group 'tools
  :prefix "jjar-"
  )

(defcustom jjar-jar-command "/jdk1.2/bin/jar.exe"
  "*The path to the jar command to be used."
  :group 'jjar
  :type 'string
  )

(defcustom jjar-jar-nocompress-option t
  "*If non-nil jar uses no ZIP compression."
  :group 'jjar
  :type 'string
  )

(defcustom jjar-jar-verbose-option t
  "*If non-nil jar generates verbose output on standard error."
  :group 'jjar
  :type 'boolean
  )

(defcustom jjar-include-wildcards '("*.class" "*.properties")
  "*List of predefined shell wildcard expressions used to locate files.
The default setting includes class and properties files."
  :group 'jjar
  :type '(repeat (regexp :format "%s"))
  )

(defcustom jjar-load-hook '()
  "*Hook run when package has been loaded."
  :group 'jjar
  :type 'hook
  )

;;;###autoload
(defun jjar-customize ()
  "Customization of the group jjar."
  (interactive)
  (customize-group "jjar")
  )

(defun jjar-version-number ()
  "Return jjar version number."
  (string-match "[0123456789.]+" jjar-version)
  (match-string 0 jjar-version)
  )

;;;###autoload
(defun jjar-display-version ()
  "Display jjar version."
  (interactive)
  (message "Using 'jjar' version %s." (jjar-version-number))
  )

(defvar jjar-build-option nil
  "\"-c\" to create a new JAR file or \"-u\" to update an existing one.")

(defvar jjar-base-directory nil
  "The directory from which the JAR command is issued.")

(defvar jjar-selected-subdirs nil
  "List of sub-directories to scan.")

(defvar jjar-selected-wildcards nil
  "List of wildcards used to select files.")

(defvar jjar-jar-file nil
  "The name of the JAR file to be created/updated.")

;;;###autoload
(defun jjar-create (base)
  "Create a new jar file.

BASE is the directory from which the JAR command is issued, that is
where the files to be archived are searched.

See also `jjar-choose-base-subdirs' and `jjar-choose-wildcards'.
This function sets the `jjar-build-option', `jjar-base-directory'
and `jjar-jar-file' variables then calls `jjar-choose-base-subdirs'.
"
  (interactive "DBase-directory: ")
  (setq jjar-build-option "-c")
  (setq jjar-base-directory (file-name-as-directory (expand-file-name base)))
  (setq jjar-jar-file (file-relative-name (read-file-name "jar-file: "
                                                          jjar-base-directory)
                                          jjar-base-directory))
  (jjar-choose-base-subdirs)
  )

;;;###autoload
(defun jjar-update (base file)
  "Update an existing jar file.

BASE is the directory from which the JAR command is issued, that is
where the files to be archived are searched.
FILE is the name of the JAR file to update.

See also `jjar-choose-base-subdirs' and `jjar-choose-wildcards'.

This function sets the `jjar-build-option', `jjar-base-directory'
and `jjar-jar-file' variables then calls `jjar-choose-base-subdirs'.
"
  (interactive "DBase-directory: \nfJAR-file: ")
  (setq jjar-build-option "-u")
  (setq jjar-base-directory (file-name-as-directory (expand-file-name base)))
  (setq jjar-jar-file (file-relative-name file jjar-base-directory))
  (jjar-choose-base-subdirs)
  )

(defun jjar-choose-base-subdirs ()
  "Choose the sub-directories of `jjar-base-directory' to scan.
To scan `jjar-base-directory' select the sub-directory \".\".

This function sets the `jjar-selected-subdirs' variable
then calls `jjar-choose-wildcards'.
"
  (let ((subdirs (jjar-get-base-subdirs)))
    (with-current-buffer (get-buffer-create "*jjar*" )
      (switch-to-buffer (current-buffer))
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((all (overlay-lists)))
        ;; Delete all the overlays.
        (mapcar 'delete-overlay (car all))
        (mapcar 'delete-overlay (cdr all)))
      (setq jjar-selected-subdirs nil)
      ;; Insert the dialog header
      (widget-insert
       (format "Select the sub-directories of '%s' to scan.\n\n" jjar-base-directory))
      (widget-insert "Click Next to continue or Cancel to quit.\n" )
      ;; Insert the list of sub-directories as checkboxes
      (let (i n)
        (setq i 0)
        (setq n (length subdirs))
        (while (< i n)
          (let* ((subdir (nth i subdirs))
                 (args (list  
                        'checkbox
                        :value nil      ; unselected checkbox
                        :format "\n %[%v%]  %t"
                        :tag subdir
                        :notify (lambda (widget &rest ignore)
                                  (let ((value (widget-get widget ':tag)))
                                    ;; if value is already in the selected subdirs
                                    (if (find value jjar-selected-subdirs)
                                        ;; then remove it
                                        (progn
                                          (setq jjar-selected-subdirs
                                                (delq value jjar-selected-subdirs))
                                          (message "You have deselected: %s" value))
                                      ;; else add it
                                      (progn
                                        (setq jjar-selected-subdirs
                                              (nconc (list value)
                                                     jjar-selected-subdirs))
                                        (message "You have selected: %s" value))))))))
            (apply 'widget-create args)
            (setq i (+ i 1)))))
      ;; Insert the Next button
      (widget-insert "\n\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (let ((dialog-buffer (current-buffer)))
                                 (if jjar-selected-subdirs
                                     (progn (kill-buffer dialog-buffer)
                                            (message "Sub-directories: %S"
                                                     jjar-selected-subdirs)
                                            (jjar-choose-wildcards)
                                            )
                                   (message "No sub-directory selected."))))
                     "Next")
      (widget-insert " ")
      ;; Insert the Cancel button
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (let ((dialog-buffer (current-buffer)))
                                 (kill-buffer dialog-buffer)
                                 (message "Command canceled.")))
                     "Cancel")
      (use-local-map widget-keymap)
      (widget-setup)))
  )

(defun jjar-get-base-subdirs ()
  "Return the list of sub-directories of `jjar-base-directory'."
  (mapcan '(lambda (f)
             (let ((file (concat jjar-base-directory f)))
               (if  (and (file-directory-p file)
                         (not (string= f "..")))
                   (list f))))
          (directory-files jjar-base-directory))
  )

(defun jjar-choose-wildcards ()
  "Choose the wildcard expressions used to select files.

This function sets the `jjar-selected-wildcards' variable
then calls `jjar-execute'.
"
  (with-current-buffer (get-buffer-create "*jjar*" )
    (switch-to-buffer (current-buffer))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))
    (setq jjar-selected-wildcards jjar-include-wildcards)
    ;; Insert the dialog header
    (widget-insert
     "Customize the wildcard expressions used to select files.\n\n")
    (widget-insert "Click OK to build the JAR file or Cancel to quit.\n\n")
    ;; Insert an editable list of the predefined wildcard expressions
    (widget-create 'editable-list
                   :entry-format "%i %d %v"
                   :notify (lambda (widget &rest ignore)
                             (setq jjar-selected-wildcards (widget-value widget))
                                   (message "%S" jjar-selected-wildcards))
                   :value jjar-selected-wildcards
                   '(editable-field :value ""))
    ;; Insert the Ok button
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((dialog-buffer (current-buffer)))
                               (if jjar-selected-wildcards
                                   (progn (kill-buffer dialog-buffer)
                                          (message "Wildcard expressions: %S"
                                                   jjar-selected-wildcards)
                                          (jjar-execute)
                                          )
                                 (message "No wildcard expression selected."))))
                   "Ok")
    (widget-insert " ")
    ;; Insert the cancel button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((dialog-buffer
                                    (current-buffer)))
                               (kill-buffer dialog-buffer)
                               (message "Command canceled.")))
                   "Cancel")
    (use-local-map widget-keymap)
    (widget-setup))
  )

(defun jjar-execute ()
  "Build and run the JAR command in compile mode.

`jjar-base-directory'     - directory from which the command is issued.
`jjar-jar-file'           - name of the JAR file to be created/updated.
`jjar-selected-subdirs'   - list of sub-directories to scan.
`jjar-selected-wildcards' - list of wildcards used to select files.
"
  (let ((compile-command (jjar-make-jar-command)))
    (when compile-command
      ;; Force save-some-buffers to use the minibuffer
      ;; to query user about whether to save modified buffers.
      ;; Otherwise, when user invokes the command from
      ;; menu, save-some-buffers tries to popup a menu
      ;; which seems not to be supported--at least on
      ;; the PC.
      (if (eq system-type 'windows-nt)
          (let ((temp last-nonmenu-event))
            ;; The next line makes emacs think that the command
            ;; was invoked from the minibuffer, even when it
            ;; is actually invoked from the menu-bar.
            (setq last-nonmenu-event t)
            (save-some-buffers (not compilation-ask-about-save) nil)
            (setq last-nonmenu-event temp))
        (save-some-buffers (not compilation-ask-about-save) nil))
      (let ((default-directory jjar-base-directory))
        ;;(message "%s" compile-command)
        (compile-internal compile-command "No more errors"))))
  )

(defun jjar-make-jar-command ()
  "Build and return the jar command."
  (let ((files (jjar-get-matching-wilcards)))
    (and files
         (concat jjar-jar-command " " 
                 (jjar-get-jar-options) " "
                 jjar-jar-file " "
                 (mapconcat 'identity files " "))))
  )

(defun jjar-get-jar-options ()
  "Build and return the jar command options.
See `jjar-jar-nocompress-option' and `jjar-jar-verbose-option'."
  (let ((options jjar-build-option))
    (if jjar-jar-nocompress-option
        (setq options (concat options "0")))
    (if jjar-jar-verbose-option
        (setq options (concat options "v")))
    (setq options (concat options "f"))
    options)
  )

(defun jjar-get-matching-wilcards ()
  "Return a list of wildcard expressions corresponding to files in
`jjar-selected-subdirs' that match one of `jjar-selected-wildcards'
wildcard expressions. Each wildcard expression returned is relative
to `jjar-base-directory'."
  (message "Searching matching files, please wait...")
  (mapcan '(lambda (subdir)
             (let ((dir (concat jjar-base-directory subdir)))
               (if (string= subdir ".")
                   (jjar-get-matching-wildcards-in-dir dir)
                 (jjar-get-matching-wildcards-in-tree dir))))
          jjar-selected-subdirs)
  )

(defun jjar-get-matching-wildcards-in-tree (dir)
  "Return a list of wildcard expressions corresponding to files in
DIR tree that match one of `jjar-selected-wildcards' wildcard expressions."
  (nconc (jjar-get-matching-wildcards-in-dir dir)
         (jjar-get-matching-wildcards-in-tree-aux dir))
  )

(defun jjar-get-matching-wildcards-in-tree-aux (dir)
  "Auxiliary function used by `jjar-get-matching-wildcards-in-tree'."
  (let ((dir (file-name-as-directory dir)))
    (mapcan  '(lambda (f)
                (let ((file (concat dir f)))
                  (unless (or (string= f "..")
                              (string= f ".")
                              (not (file-directory-p file)))
                    (nconc (jjar-get-matching-wildcards-in-dir file)
                           (jjar-get-matching-wildcards-in-tree-aux file)))))
             (directory-files dir)))
  )

(defun jjar-get-matching-wildcards-in-dir (dir)
  "Return a list of wildcard expressions corresponding to files in
DIR that match one of `jjar-selected-wildcards' wildcard expressions."
  (mapcan '(lambda (wildcard)
             (and (directory-files dir nil (wildcard-to-regexp wildcard))
                  (list (concat (file-name-as-directory
                                 (file-relative-name dir jjar-base-directory))
                                wildcard))))
          jjar-selected-wildcards)
  )

(provide 'jjar)
(run-hooks 'jjar-load-hook)

;;; Change History:

;;
;; $Log: jjar.el,v $
;; Revision 1.3  1999/07/06 15:31:27  ebat311
;; Major rewrite!
;; Allow update of an existing jar file (JDK 1.2).
;; Improved comments and dialog behavior.
;;
;; Revision 1.2  1999-07-06 12:39:20+02  ebat311
;; FIXED:
;;   - missing selection of files in the classpath-root directory.
;;   - comments.
;;
;; Revision 1.1  1999-07-05 23:30:12+02  ebat311
;; Initial revision
;;
;;

;;; jjar.el ends here.
