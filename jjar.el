;; @(#) jjar.el -- Java Archive builder
;; @(#) $Id: jjar.el,v 1.1 1999/07/05 21:30:12 ebat311 Exp $

;; This file is not part of Emacs

;; Copyright (C) 1999 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      June 14 1999

;; LCD Archive Entry:
;; jjar|David Ponce|david.ponce@wanadoo.fr|
;; Java Archive builder|
;; $Date: 1999/07/05 21:30:12 $|$Revision: 1.1 $|~/misc/jjar.el|

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
;;  This elisp package provides the `jjar-build' command to create a
;;  Java ARrchive files (JAR) containing all the files in a given directory
;;  that match predefined shell wildcards.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jjar)

;;; Usage:
;;
;;  M-x `jjar-build'
;;     Builds a jar file containing all the files that match `jjar-include-wildcards'
;;     in a given directory tree. This one must be accessible from a given classpath root
;;     to ensure that a correct directory structure (that is Java package structure) is
;;     stored in the jar file.
;;

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
;;     List of shell wildcard expressions for files included in archives.
;;     The default setting includes .class and .properties files.
;;
  
;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of jjar was developed with NTEmacs 20.3.10 under MS Windows
;;  NT 4 WKS SP5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:

(require 'compile)
(require 'cl)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(defconst jjar-version "$Revision: 1.1 $"
  "jjar version number."
  )

(defgroup jjar nil
  "jjar package customization."
  :group 'tools
  :prefix "jjar-"
  )

(defcustom jjar-jar-command "/jdk1.1/bin/jar.exe"
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
  "*List of shell wildcard expressions for files included in archives.
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
  "Returns jjar version number."
  (string-match "[0123456789.]+" jjar-version)
  (match-string 0 jjar-version)
  )

;;;###autoload
(defun jjar-display-version ()
  "Displays jjar version."
  (interactive)
  (message "Using 'jjar' version %s." (jjar-version-number))
  )

;;;###autoload
(defun jjar-build (classpath-root)
  "Builds a jar file containing all the files in choosen sub-directories of ROOT
that match one of the `jjar-include-wildcards' wildcard expressions.
See also `jjar-choose-classpath-root-subdirs'."
  (interactive "DClasspath-root: ")
  (let* ((default-directory (expand-file-name classpath-root))
         (jar-file (file-relative-name (read-file-name "jar-file: ") default-directory)))
    (jjar-choose-classpath-root-subdirs jar-file)
    )
  )

(defvar jjar-selected-subdirs nil
  "List of selected sub-directories.")

(defvar jjar-jar-file nil
  "The JAR file to be created.")

(defun jjar-choose-classpath-root-subdirs (jar-file)
  "Prompts the user to select subdirs of the classpath root. When the user click the
Ok button the `jjar-build-internal-1' function is called.
JAR-FILE is the JAR file to be created.
`default-directory' is the CLASSPATH root."
  (let ((classpath-root default-directory)
        (subdirs (jjar-get-classpath-root-subdirs)))
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
      (setq jjar-jar-file jar-file)
      (setq default-directory classpath-root)
      (widget-insert "Select directories to include in JAR file.\n")
      (widget-insert "Then click the OK button to build the JAR file.\n" )
      (widget-insert "Or click the Cancel button to quit.\n" )
      (let (i n)                        ; Iteration vars
        (setq i 0)
        (setq n (length subdirs))
        (while (< i n)
          (let* ((subdir 
                  (nth i subdirs))
                 (args (list  
                        ;; widget of type checkbox
                        'checkbox
                        :value nil      ; subdir
                        ;; with a value equal to the subdir
                        :format "\n %[%v%]  %t"
                        :tag subdir
                        ;; When we activate this button we need to add to the
                        ;; selected subdirs
                        :notify (lambda (widget &rest ignore)
                                  (let ((value (widget-get widget ':tag)))
                                    ;; if the widgets value is already in the selected subdirs
                                    (if (find value jjar-selected-subdirs)
                                        ;; then we need to remove it
                                        (progn
                                          (setq jjar-selected-subdirs
                                                (delq value jjar-selected-subdirs))
                                          (message "You have deselected: %s" value))
                                      ;; else we need to add it
                                      (progn
                                        (setq jjar-selected-subdirs
                                              (nconc (list value)
                                                     jjar-selected-subdirs))
                                        (message "You have selected: %s" value))))))))
            (apply 'widget-create args)
            (setq i (+ i 1)))))
      ;; Then insert the Ok button
      (widget-insert "\n\n")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (let ((dialog-buffer (current-buffer))
                                     (classpath-root default-directory))
                                 ;;                               (delete-window)
                                 (kill-buffer dialog-buffer)
                                 (if jjar-selected-subdirs
                                     (progn (message "Selected: %S" jjar-selected-subdirs)
                                            (jjar-build-internal-1 classpath-root
                                                                   jjar-jar-file
                                                                   jjar-selected-subdirs)
                                            )
                                   (message "No subdirs selected."))))
                     "Ok")
      (widget-insert " ")
      ;; Then insert the cancel button
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (let ((dialog-buffer
                                      (current-buffer)))
                                 ;;                               (delete-window)
                                 (kill-buffer dialog-buffer)
                                 (message "Dialog canceled.")))
                     "Cancel")
      (use-local-map widget-keymap)
      (widget-setup)))
  )

(defun jjar-get-classpath-root-subdirs ()
  "Returns the list of sub-directories of the CLASSPATH root.
`default-directory' is the CLASSPATH root."
  (let ((classpath-root (file-name-as-directory default-directory)))
    (mapcan '(lambda (f)
               (let ((rel-f (concat classpath-root f)))
                 (if  (and (file-directory-p rel-f)
                           (not (string= f ".."))
                           (not (string= f ".")))
                     (list f))
                 )
               )
            (directory-files classpath-root)))
  )

(defun jjar-build-internal-1 (classpath-root jar-file subdirs)
  "Actually runs the JAR command. Called from `jjar-choose-classpath-root-subdirs' when the user
click on the Ok button.
CLASSPATH-ROOT is the classpath root directory.
JAR-FILE is the name of the JAR file to be created.
SUBDIRS is the list of sub-directories of DIR to be included in JAR-FILE."
  (let* ((default-directory classpath-root)
         (compile-command (jjar-make-jar-command jar-file subdirs)))
    (if compile-command
        (progn
          ;; Force save-some-buffers to use the minibuffer
          ;; to query user about whether to save modified buffers.
          ;; Otherwise, when user invokes jjar-build from
          ;; menu, save-some-buffers tries to popup a menu
          ;; which seems not to be supported--at least on
          ;; the PC.
          (if (eq system-type 'windows-nt)
              (let ((temp last-nonmenu-event))
                ;; The next line makes emacs think that jjar-build
                ;; was invoked from the minibuffer, even when it
                ;; is actually invoked from the menu-bar.
                (setq last-nonmenu-event t)
                (save-some-buffers (not compilation-ask-about-save) nil)
                (setq last-nonmenu-event temp))
            (save-some-buffers (not compilation-ask-about-save) nil))
          ;;(message "%s" compile-command)
          (compile-internal compile-command "No more errors")
          )))
  )

(defun jjar-make-jar-command (jar-file subdirs)
  "Returns the jar command needed to build JAR-FILE. SUBDIRS is the list of
directory trees where files stored in JAR-FILE are searched (see `jjar-build')."
  (let ((files
         (mapcan '(lambda (subdir)
                    (jjar-get-matching-wildcards-in-tree
                     (concat (file-name-as-directory default-directory) subdir)))
                 subdirs)))
    (and files
         (concat jjar-jar-command " " 
                 (jjar-get-jar-options) " "
                 jar-file " "
                 (mapconcat 'identity files " "))))
  )

(defun jjar-get-jar-options ()
  "Returns the jar command options. See `jjar-jar-nocompress-option' and
`jjar-jar-verbose-option'."
  (let ((options "-c"))
    (if jjar-jar-nocompress-option
        (setq options (concat options "0")))
    (if jjar-jar-verbose-option
        (setq options (concat options "v")))
    (setq options (concat options "f"))
    options)
  )

(defun jjar-get-matching-wildcards-in-tree (dir)
  "Returns a list of wildcard expressions corresponding to files in directory
DIR and its subdirs that match one of `jjar-include-wildcards' wildcard
expressions. Each wildcard expression returned is relative to `default-directory'."
  (message "Searching matching files, please wait...")
  (nconc
   (jjar-get-matching-wildcards-in-dir dir)
   (jjar-get-matching-wildcards-in-tree-aux dir))
  )

(defun jjar-get-matching-wildcards-in-tree-aux (dir)
  "Auxiliary function used by `jjar-get-matching-wildcards-in-tree'."
  (let ((dir (file-name-as-directory dir)))
    (mapcan  '(lambda (f)
                (let ((rel-f (concat dir f)))
                  (unless (or (string= f "..") (string= f ".")
                              (not (file-directory-p rel-f)))
                    (nconc
                     (jjar-get-matching-wildcards-in-dir rel-f)
                     (jjar-get-matching-wildcards-in-tree-aux rel-f)))))
             (directory-files dir)))
  )

(defun jjar-get-matching-wildcards-in-dir (dir)
  "Returns a list of wildcard expressions corresponding to files in directory
DIR that match one of `jjar-include-wildcards' wildcard expressions.
Each wildcard expression returned is relative to `default-directory'."
  (mapcan '(lambda (wildcard)
             (and (directory-files dir nil (wildcard-to-regexp wildcard))
                  (list (concat
                         (file-name-as-directory (file-relative-name dir default-directory))
                         wildcard))))
          jjar-include-wildcards)
  )

(provide 'jjar)
(run-hooks 'jjar-load-hook)

;;; Change History:

;;
;; $Log: jjar.el,v $
;; Revision 1.1  1999/07/05 21:30:12  ebat311
;; Initial revision
;;
;;

;;; jjar.el ends here.
