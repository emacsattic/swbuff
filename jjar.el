;; jjar.el --- Java Archive builder

;; Copyright (C) 1999, 2000 by David Ponce

;; Author: David Ponce david@dponce.com
;; Maintainer: David Ponce david@dponce.com
;; Created: June 14 1999
;; Version: 1.7 (beta2)
;; Keywords: tools
;; VC: $Id: jjar.el,v 1.8 2000/08/11 14:44:43 david_ponce Exp $
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

;; This library provides commands to create or update (since JDK 1.2) a
;; Java ARrchive files (.jar). The files to be archived are searched in
;; a given directory tree in this way:
;;
;; 1 - A dialog is shown to customize shell wildcard expressions used to
;;     locate the files.
;; 2 - When the files are located, a second dialog is shown to select the
;;     ones to archive.
;; 3 - The jar command is built and run in compile-mode.

;; Installation:
;;
;; Put this file on your Emacs load-path. Then, into your Emacs
;; startup file, add:
;;   (require 'jjar) 
;; or:
;;   (autoload 'jjar-customize "jjar" "Customize jjar options."      t nil)
;;   (autoload 'jjar-create    "jjar" "Create a new jar file."       t nil)
;;   (autoload 'jjar-update    "jjar" "Update an existing jar file." t nil)

;; Usage:
;;
;; M-x `jjar-create' to create a new jar file.
;;
;; M-x `jjar-update' to update an existing jar file (since JDK 1.2).
;;
;; M-x `jjar-customize' to customize all the jjar options.
;;
;; You should at least customize the `jjar-jar-command' variable to
;; setup the path to the jar command in your installation.
  
;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce at <david@dponce.com>

;;; Change Log:

;; $Log: jjar.el,v $
;; Revision 1.8  2000/08/11 14:44:43  david_ponce
;; Improved dialog handling.
;; Comments follow standard Emacs conventions.
;;
;; Revision 1.7  2000/04/12 14:31:01  david_ponce
;; New improved file selection scheme.
;; The jar command path default value is now platform independant.
;; Code cleanup.
;; Documentation rewrite.
;;
;; Revision 1.6  2000/03/10 15:28:20  ebat311
;; No more require cl.
;; New directory selection scheme.
;;
;; Revision 1.5  1999-07-21 16:45:35+02  ebat311
;; FIXED:
;;   bad :type 'string of customizable variable `jjar-jar-nocompress-option'.
;;   Thanks to "Phillip Lord" <plord@hgmp.mrc.ac.uk> who has reported
;;   this bug.
;;
;; Revision 1.4  1999-07-06 17:38:42+02  ebat311
;; Added comments for autoload installation.
;;
;; Revision 1.3  1999-07-06 17:31:27+02  ebat311
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

;;; Code:
(require 'compile)
(require 'wid-edit)

(defconst jjar-version "1.7 (beta2) $Date: 2000/08/11 14:44:43 $"
  "jjar version information.")

(defgroup jjar nil
  "Java Archive builder."
  :group 'tools
  :prefix "jjar-")

(defcustom jjar-jar-command "jar"
  "*Path to the Java JDK jar command."
  :group 'jjar
  :type 'string)

(defcustom jjar-jar-nocompress-option t
  "*If non-nil jar uses no ZIP compression."
  :group 'jjar
  :type 'boolean)

(defcustom jjar-jar-verbose-option t
  "*If non-nil jar generates verbose output on standard error."
  :group 'jjar
  :type 'boolean)

(defcustom jjar-include-wildcards '("*.class" "*.properties")
  "*List of predefined shell wildcard expressions used to locate files.
The default setting includes class and properties files."
  :group 'jjar
  :type '(repeat (regexp :format "%s")))

(defcustom jjar-load-hook nil
  "*Hook run when package has been loaded."
  :group 'jjar
  :type 'hook)

(defvar jjar-build-option nil
  "\"-c\" to create a new jar file or \"-u\" to update an existing one.")

(defvar jjar-base-directory nil
  "The jar command is issued (and the files are located) in this directory.")

(defvar jjar-jar-file nil
  "Name of the jar file to be created or updated.")

(defvar jjar-wildcards nil
  "List of shell wildcards used to locate the files to be archived.")

(defvar jjar-files nil
  "List of file expressions to give to the jar command.")

(defun jjar-get-matching-files ()
  "Return a list of file expressions to give to the jar command. File expressions
are the shell wildcard expressions based on `jjar-wildcards' that match files in
`jjar-base-directory' tree.
Called by `jjar-files-dialog' to initialize `jjar-files'."
  (message "Searching matching files...")
  (let ((file-list
         (nconc (and (file-directory-p jjar-base-directory)
                     (jjar-get-matching-files-in-dir jjar-base-directory))
                (jjar-get-matching-files-in-tree jjar-base-directory))))
    (message "Searching matching files...Done")
    file-list))

(defun jjar-get-matching-files-in-dir (dir)
  "Return a list file expressions corresponding to files in DIR that match
one of `jjar-wildcards' expressions."
  (delq nil
        (mapcar (function
                 (lambda (wildcard)
                   (and (directory-files dir nil (wildcard-to-regexp wildcard))
                        (concat (file-name-as-directory
                                 (file-relative-name dir jjar-base-directory))
                                wildcard))))
                jjar-wildcards)))

(defun jjar-get-matching-files-in-tree (dir)
  "Auxiliary function used by `jjar-get-matching-files'.
DIR is a subdirectory in `jjar-base-directory' tree."
  (apply 'nconc
         (mapcar (function
                  (lambda (entry)
                    (and (not (string= (substring entry -1) "."))
                         (file-directory-p entry)
                         (nconc (jjar-get-matching-files-in-dir  entry)
                                (jjar-get-matching-files-in-tree entry)))))
                 (directory-files dir t))))

(defun jjar-make-jar-command ()
  "Build and return the jar command.
See `jjar-jar-command' `jjar-get-jar-options' `jjar-jar-file' `jjar-files'."
  (and jjar-files
       (concat jjar-jar-command " " 
               (jjar-get-jar-options) " "
               jjar-jar-file " "
               (mapconcat 'identity jjar-files " "))))

(defun jjar-get-jar-options ()
  "Build and return the jar command options.
See `jjar-jar-nocompress-option' and `jjar-jar-verbose-option'."
  (let ((options jjar-build-option))
    (if jjar-jar-nocompress-option
        (setq options (concat options "0")))
    (if jjar-jar-verbose-option
        (setq options (concat options "v")))
    (setq options (concat options "f"))
    options))

(defun jjar-execute ()
  "Build and run the jar command in compile mode.
See `jjar-make-jar-command'."
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
        (compile-internal compile-command "No more errors")
        ))))

;;;
;;; Dialogs stuff
;;;

(defun jjar-cancel-dialog (&rest ignore)
  "Cancel the current dialog."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Dialog canceled."))

(defvar jjar-dialog-mode-map nil
  "`jjar-dialog-mode' keymap.")

(if jjar-dialog-mode-map
    ()
  (setq jjar-dialog-mode-map (make-sparse-keymap))
  (define-key jjar-dialog-mode-map "q" 'jjar-cancel-dialog)
  (define-key jjar-dialog-mode-map [down-mouse-1] 'widget-button-click)
  (set-keymap-parent jjar-dialog-mode-map widget-keymap))

(defun jjar-dialog-mode ()
  "Major mode used in jjar dialogs.

These are the special commands of jjar-dialog-mode mode:
    q            -- to cancel the dialog.
    down-mouse-1 -- to click on button."
  (interactive)
  (setq major-mode 'jjar-dialog-mode)
  (setq mode-name "jjar-dialog")
  (use-local-map jjar-dialog-mode-map))

(defun jjar-wildcards-dialog (wildcards)
  "Show the dialog used to customize the shell wildcard expressions used to locate
the files to archive.
WILDCARDS is a list of wildcard expressions used to initialize the dialog."
  (with-current-buffer (get-buffer-create "*jjar-wildcards-dialog*" )
    (switch-to-buffer (current-buffer))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))

    ;; Initialize the wildcards with the predefined ones
    (setq jjar-wildcards wildcards)

    ;; Insert the dialog header
    (widget-insert "Customize the wildcard expressions used to locate files.\n\n")
    
    ;; Insert an editable list of the wildcard expressions
    (widget-create 'editable-list
                   :entry-format "%i %d %v"
                   :notify (lambda (widget &rest ignore)
                             (setq jjar-wildcards (widget-value widget))
                             ;;(message "%S" jjar-wildcards)
                             )
                   :value jjar-wildcards
                   '(editable-field :value ""))
    
    ;; Insert the Next button
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (if (not jjar-wildcards)
                                 (message "No wildcard expression selected.")
                               (kill-buffer (current-buffer))
                               ;;(message "Wildcard expressions: %S" jjar-wildcards)
                               (jjar-files-dialog)
                               ))
                   "Next")

    ;; Insert the cancel button
    (widget-insert " ")
    (widget-create 'push-button
                   :notify 'jjar-cancel-dialog
                   "Cancel")
    (jjar-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

(defun jjar-files-dialog-toggle-selection (widget &rest ignore)
  "Checkbox widget action used by `jjar-files-dialog' to select or
unselect a directory."
  (let ((value (widget-get widget ':tag)))
    ;; if value is already in the selected subdirs
    (cond ((memq value jjar-files)
           ;; then remove it
           (setq jjar-files (delq value jjar-files))
           (message "You have deselected: %s" value))
          ;; else add it
          (t
           (setq jjar-files (nconc (list value) jjar-files))
           (message "You have selected: %s" value)))))

(defun jjar-files-dialog ()
  "Show the dialog used to select the file expressions given to the jar command."
  (with-current-buffer (get-buffer-create "*jjar-files-dialog*")
    (switch-to-buffer (current-buffer))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))

    ;; Initialize the list of files
    (setq jjar-files (jjar-get-matching-files))

    ;; Insert the dialog header
    (widget-insert (format "Select the files in '%s' to archive.\n\n"
                           jjar-base-directory))
      
    ;; Insert the list of sub-directories as checkboxes
    (mapcar (function
             (lambda (dir)
               (widget-create 'checkbox
                              :value (memq dir jjar-files)
                              :format "\n %[%v%]  %t"
                              :tag dir
                              :notify 'jjar-files-dialog-toggle-selection)))
            jjar-files)

    ;; Insert the Back button
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (kill-buffer (current-buffer))
                             (jjar-wildcards-dialog jjar-wildcards))
                   "Back")
      
    ;; Insert the Next button
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (if (not jjar-files)
                                 (message "No file selected.")  
                               (kill-buffer (current-buffer))
                               ;;(message "Files: %S" jjar-files)
                               (jjar-execute)
                               ))
                   "Next")

    ;; Insert the Cancel button
    (widget-insert " ")
    (widget-create 'push-button
                   :notify 'jjar-cancel-dialog
                   "Cancel")
    (jjar-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

;;;
;;; Commands
;;;

;;;###autoload
(defun jjar-customize ()
  "Customize jjar options."
  (interactive)
  (customize-group "jjar"))

;;;###autoload
(defun jjar-create (base)
  "Create a new jar file. BASE is the directory from which the jar
command is issued, that is where the files to be archived are
searched."
  (interactive "DBase-directory: ")
  (setq jjar-build-option "-c")
  (setq jjar-base-directory (file-name-as-directory (expand-file-name base)))
  (setq jjar-jar-file (file-relative-name (read-file-name "jar-file: "
                                                          jjar-base-directory)
                                          jjar-base-directory))
  (jjar-wildcards-dialog jjar-include-wildcards))

;;;###autoload
(defun jjar-update (base file)
  "Update an existing jar file. BASE is the directory from which the
jar command is issued, that is where the files to be archived are
searched. FILE is the name of the jar file to update."
  (interactive "DBase-directory: \nfJAR-file: ")
  (setq jjar-build-option "-u")
  (setq jjar-base-directory (file-name-as-directory (expand-file-name base)))
  (setq jjar-jar-file (file-relative-name file jjar-base-directory))
  (jjar-wildcards-dialog jjar-include-wildcards))

(provide 'jjar)
(run-hooks 'jjar-load-hook)

;;; jjar.el ends here
