;;; jmaker.el --- Java Makefile generator

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: July 22 1998
;; Keywords: tools
;; Revision: $Id: jmaker.el,v 1.28 2003/07/27 07:47:22 ponced Exp $

(defconst jmaker-version "2.3")

;; This file is not part of GNU Emacs

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
;;
;; This package is an add-on to the Java Development Environment for
;; Emacs (JDEE).  It automatically generates Makefiles to build Java
;; projects using make. The default Makefile template provided uses
;; the java compiler and its options from JDEE and generates targets
;; to compile all .java files in the current directory and its
;; sub-directories.
;;
;; M-x `jmaker-generate-makefile' generate Java Makefiles.
;;
;; CAUTION: jmaker doesn't take back any modifications you could have
;; made to existing Makefiles!
;;
;; To build a project with make you can use the `compile' command or
;; the `jde-build' command if `jde-build-function' is `jde-make'.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (autoload 'jmaker-generate-makefile "jmaker"
;;   "Generate and edit a Java Makefile in directory ROOT." t)
;;
;; A "JMaker" menu item is added to the menu bar in `jde-mode' after
;; jmaker has been loaded.
;;

;;; History:
;;

;;; Code:
(require 'compile)
(require 'tempo)
(require 'wid-edit)
(eval-when-compile (require 'cl))

;;; Compatibility
;;
(if (fboundp 'overlay-lists)
    (defalias 'jmaker-overlay-lists
      'overlay-lists)
  (defalias 'jmaker-overlay-lists
    '(lambda () (list (extent-list)))))

(if (fboundp 'delete-overlay)
    (defalias 'jmaker-delete-overlay
      'delete-overlay)
  (defalias 'jmaker-delete-overlay
    'delete-extent))

;;; Options
;;
(defgroup jmaker nil
  "Java Makefile generator."
  :group 'tools
  :prefix "jmaker-")

(defcustom jmaker-makefile-buffer-template
  '(
    "\"####\" 'n"
    "\"#### Java Makefile automatically generated by jmaker \""
    "jmaker-version 'n"
    "\"#### Creation date: \" (current-time-string) 'n"
    "\"####\" 'n"
    "'n"
    "\"#### Java compiler settings\" 'n"
    "\"JAVAC       = \" (jmaker-java-compiler)  'n"
    "\"JAVAC_FLAGS = \" (jmaker-java-compiler-options) 'n"
    "'n"
    "\"#### Targets settings\" 'n"
    "\"CLASS_FILES   = \\\\\" 'n (jmaker-make-get-file-targets) 'n"
    "\"SUBDIRS       = \\\\\" 'n (jmaker-make-get-subdir-targets) 'n"
    "\"SUBDIRS_CLEAN = $(patsubst %,%.clean,$(SUBDIRS))\" 'n"
    "\"SUBDIRS_MAKE  = $(patsubst %,%.make,$(SUBDIRS))\" 'n"
    "'n"
    "\"#### Main targets\" 'n"
    "'n"
    "\"# Default\" 'n"
    "\"all: $(CLASS_FILES) $(SUBDIRS_MAKE)\" 'n"
    "'n"
    "\"# Cleanup\" 'n"
    "\"clean: $(SUBDIRS_CLEAN)\" 'n"
    "\"\\t$(RM) *.class\" 'n"
    "\"\\t@echo \\\"Cleanup done.\\\"\" 'n"
    "'n"
    "\"# Rebuild\" 'n"
    "\"build: clean all\" 'n"
    "\"\\t@echo \\\"Rebuild done.\\\"\" 'n"
    "'n"
    "\"#### Aux targets\" 'n"
    "'n"
    "\"# Files compilation\" 'n"
    "\"%.class: %.java\" 'n"
    "\"\\t$(JAVAC) $(JAVAC_FLAGS) $<\" 'n"
    "'n"
    "\"# Sub-directories compilation\" 'n"
    "\"%.make:\" 'n"
    "\"\\t$(MAKE) -k -C $(subst .make,,$@)\" 'n"
    "'n"
    "\"# Sub-directories cleanup\" 'n"
    "\"%.clean:\" 'n"
    "\"\\t$(MAKE) -k -C $(subst .clean,,$@) clean\" 'n"
    "'n"
    "\"# Phony Targets\" 'n"
    "\".PHONY: clean build help\" 'n"
    "'n"
    "\"#### Help\" 'n"
    "\"help:\" 'n"
    "\"\\t@echo \\\"Usage: make [targets...]\\\"\" 'n"
    "\"\\t@echo \\\"\\\"\" 'n"
    "\"\\t@echo \\\"where targets include:\\\"\" 'n"
    "\"\\t@echo \\\"\\\"\" 'n"
    "\"\\t@echo \\\"  help           display this help\\\"\" 'n"
    "\"\\t@echo \\\"  all            compile all (default)\\\"\" 'n"
    "\"\\t@echo \\\"  clean          remove all class files\\\"\" 'n"
    "\"\\t@echo \\\"  build          rebuild all inconditionnally\\\"\" 'n"
    "\"\\t@echo \\\"  <class file>   compile the given file\\\"\" 'n"
    "\"\\t@echo \\\"  <subdir>.make  compile the given subdir\\\"\" 'n"
    "\"\\t@echo \\\"  <subdir>.clean clean the given subdir\\\"\" 'n"
    "'n"
    )
  "*Template for Java Makefile.
Setting this variable defines the command `jmaker-insert-makefile',
that inserts a Java Makefile at point in current buffer."
  :group 'jmaker
  :type '(repeat string)
  :set '(lambda (sym val)
          (defalias 'jmaker-insert-makefile
            (tempo-define-template
             "jmaker-insert-makefile"
             (read (format "(%s)" (mapconcat 'identity val " ")))
            "Insert a Java Makefile at point in current buffer."))
          (set-default sym val))
  )

(defcustom jmaker-end-of-line-format 'unix
  "*Specify end of line format used when writing a Makefile.
The possible choices are:

- 'Default' Use the default end of line format.
- 'Unix'    Use UNIX (LF) end of line format (the default).
- 'Dos'     Use DOS (CRLF) end of line format.
- 'Mac'     Use MAC (CR) end of line format."
  :group 'jmaker
  :type '(choice (const :tag "Default" nil )
                 (const :tag "Unix (LF)" unix)
                 (const :tag "Dos (CRLF)" dos)
                 (const :tag "Mac (CR)" mac)))

(defcustom jmaker-make-use-jde-settings
  (condition-case nil
      (progn (require 'jde) t)
    (error nil))
  "*non-nil to use JDE's current compilation options.
Otherwise use value of jmaker variables `jmaker-java-compiler' and
`jmaker-java-compiler-options'."
  :group 'jmaker
  :type 'boolean)

(defcustom jmaker-java-compiler "javac"
  "The default compiler used by jmaker."
  :group 'jmaker
  :type 'string)

(defcustom jmaker-java-compiler-options "-g -deprecation"
  "The default compiler options used by jmaker."
  :group 'jmaker
  :type 'string)

;;; Common functions
;;
(defun jmaker-get-java-names ()
  "Return Java file names sans extension found in `default-directory'."
  (mapcar 'file-name-sans-extension
          (directory-files default-directory nil "\\.java\\'")))

(defun jmaker-contains-java-files-p (dir)
  "Return non-nil if there is a Java file in directory tree DIR."
  (and (file-directory-p dir)
       (not (string-match "\\(\\.\\.?\\|CVS\\)$" dir))
       (let ((flist (directory-files dir t))
             file found)
         (while (and flist (not found))
           (setq file  (car flist)
                 flist (cdr flist)
                 found (if (file-regular-p file)
                            (string-match "\\.java\\'" file)
                         (jmaker-contains-java-files-p file))))
         found)))

(defun jmaker-get-subdirs-with-makefile (dir)
  "Return a list of DIR sub-directories containing a Makefile."
  (delq nil
        (mapcar
         #'(lambda (f)
             (and (file-directory-p f)
                  (not (string-match "\\(\\.\\.?\\|CVS\\)$" f))
                  (file-regular-p (expand-file-name "Makefile" f))
                  (file-relative-name f dir)))
         (directory-files dir t))))

(defun jmaker-set-buffer-end-of-line-format ()
  "Set the end of line format used when writing a Makefile.
See also variable `jmaker-end-of-line-format'."
  (when jmaker-end-of-line-format
    (let* ((eol (symbol-name jmaker-end-of-line-format))
           (cs  (symbol-name buffer-file-coding-system)))
      (setq cs (if (string-match "\\(dos\\|unix\\|mac\\)$" cs)
                   (intern (replace-match eol t t cs))))
      (and (coding-system-p cs)
           (not (eq cs buffer-file-coding-system))
           (setq buffer-file-coding-system cs)
           (message "End of line format set to %s" eol)))))

;;; Makefile content
;;
(defun jmaker-java-compiler ()
  "Return the Java compiler to use.
That is the current JDE's one if not the server or value of the
variable `jmaker-java-compiler' by default."
  (let (compiler)
    (when jmaker-make-use-jde-settings
      (setq compiler (jde-compile-get-the-compiler)
            compiler (unless (oref compiler :use-server-p)
                       (expand-file-name (oref compiler :path)))))
    (or compiler jmaker-java-compiler)))

(defun jmaker-java-compiler-options ()
  "Return the Java compiler args.
That is args of the current JDE's compiler or value of the variable
`jmaker-java-compiler-options' by default."
  (if jmaker-make-use-jde-settings
      (let* ((compiler (jde-compile-get-the-compiler))
             (cp (jde-compile-classpath-arg compiler)))
        (concat (format "%s %S " (car cp) (cadr cp))
                (mapconcat
                 #'identity
                 (nthcdr 2 (jde-compile-get-args compiler))
                 " ")))
    jmaker-java-compiler-options))

(defun jmaker-make-get-file-targets ()
  "Return target .class files in a string."
  (mapconcat #'(lambda (n) (concat n ".class"))
             (jmaker-get-java-names)
             " \\\n"))

(defun jmaker-make-get-subdir-targets ()
  "Return target sub-directories in a string."
  (mapconcat 'identity
             (jmaker-get-subdirs-with-makefile default-directory)
             " \\\n"))

;;; Makefile generation
;;
(defun jmaker-makefile-generator ()
  "Generate a full java Makefile in the current buffer.
If `jmaker-make-use-jde-settings' is non-nil, call
`jde-load-project-file' to update the JDE project settings for the
`default-directory'."
  (if jmaker-make-use-jde-settings
      (jde-load-project-file))
  (jmaker-insert-makefile))

(defun jmaker-generate-file-noselect (dir name generator &optional over)
  "Edit, in DIR, the file NAME and return the updated file buffer.
GENERATOR is the function used to generate the file buffer contents.
If the file NAME already exists the command requires confirmation to
overwrite it unless OVER is non-nil."
  (let ((file (expand-file-name name (file-name-as-directory dir))))
    (or over
        (and (file-exists-p file)
             (or (y-or-n-p (format "File `%s' exists; overwrite? " file))
                 (error "Command canceled"))))

    (with-current-buffer (find-file-noselect file nil t)
      (erase-buffer)
      (jmaker-set-buffer-end-of-line-format)
      (funcall generator)
      (current-buffer))))

(defun jmaker-create-makefile (dir)
  "Create a Java Makefile in directory DIR.
If the Makefile already exists it is overwritten."
  (with-current-buffer
      (jmaker-generate-file-noselect dir
                                     "Makefile"
                                     'jmaker-makefile-generator
                                     'overwrite)
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun jmaker-edit-makefile (root)
  "Generate and edit Java Makefile in directory ROOT."
  (with-current-buffer
      (jmaker-generate-file-noselect root
                                     "Makefile"
                                     'jmaker-makefile-generator)
    (makefile-mode)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

;;; Dialog
;;
(defvar jmaker-generall-dialog-selected nil
  "The list of directories where to generate a Makefile.")

(defun jmaker-generall-dialog-setup-selected-aux (dir)
  "Starting from DIR, add directories where to generate a Makefile."
  (dolist (file (directory-files dir t))
    (and (jmaker-contains-java-files-p file)
         (add-to-list 'jmaker-generall-dialog-selected
                      (file-name-as-directory file))
         (jmaker-generall-dialog-setup-selected-aux file))))

(defun jmaker-generall-dialog-setup-selected (root)
  "Create the list of directories where to generate a Makefile.
Start from ROOT directory."
  (message "Scanning directories...")
  (setq jmaker-generall-dialog-selected nil)
  (if (file-directory-p root)
      (jmaker-generall-dialog-setup-selected-aux root))
  (message "Scanning directories...Done"))

(defun jmaker-generall-dialog-toggle-selection (widget &rest ignore)
  "Toggle selection of a directory where to generate a Makefile.
WIDGET is the target checkbox widget.  IGNORE other arguments."
  (let ((value (widget-get widget ':tag))
        (item (widget-get widget ':doc)))
    ;; if value is already in the selected items
    (if (memq value jmaker-generall-dialog-selected)
        ;; then remove it
        (progn
          (setq jmaker-generall-dialog-selected
                (delq value jmaker-generall-dialog-selected))
          (message "%s removed from selection" item))
      ;; else add it
      (push value jmaker-generall-dialog-selected)
      (message "%s added to selection" item))))

(defun jmaker-cancel-dialog (&rest ignore)
  "Cancel the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer))
  (error "Command canceled"))

(defvar jmaker-dialog-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km widget-keymap)
    (define-key km "q" 'jmaker-cancel-dialog)
    (define-key km [down-mouse-1] 'widget-button-click)
    km)
  "jmaker dialog mode keymap.")

(defun jmaker-dialog-mode ()
  "jmaker dialog mode.

\\{jmaker-dialog-mode-map}."
  (interactive)
  (setq major-mode 'jmaker-dialog-mode)
  (setq mode-name "jmaker-dialog")
  (use-local-map jmaker-dialog-mode-map))

(defconst jmaker-generall-dialog-header
  "\
Select which Makefiles `jmaker' will generate in directory %s.
Then choose OK to generate, or Cancel to quit.

 (NEW)  indicate that this Makefile don't exist and will be created.
 (OVER) indicate that this Makefile exists and will be overwritten.
\n\n"
  "jmaker dialog header.")

(defvar jmaker-generall-dialog-callback-fun nil
  "Function called after Makefiles have been generated.
Local in jmaker dialog buffer.")

(defvar jmaker-generall-dialog-callback-arg nil
  "Argument passed to `jmaker-generall-dialog-callback-fun'.
Local in jmaker dialog buffer.")

(defun jmaker-generall-dialog (root &optional callback)
  "Dialog to select directories where to generate a Makefile.
List subdirectories of ROOT where Java files are found.  Optional
argument CALLBACK is a function that will be called with the argument
ROOT, after Makefile have been generated."
  (with-current-buffer (get-buffer-create "*jmaker*")
    (switch-to-buffer (current-buffer))

    ;; Cleanup buffer
    (kill-all-local-variables)
    (let ((inhibit-read-only t)
          (ol (jmaker-overlay-lists)))
      (erase-buffer)
      ;; Delete all the overlays.
      (mapc 'jmaker-delete-overlay (car ol))
      (mapc 'jmaker-delete-overlay (cdr ol)))

    ;; Save the callback function and its argument.
    (make-local-variable 'jmaker-generall-dialog-callback-fun)
    (make-local-variable 'jmaker-generall-dialog-callback-arg)
    (setq jmaker-generall-dialog-callback-fun callback)
    (setq jmaker-generall-dialog-callback-arg root)

    ;; Pre-select directories where to generate a Makefile.
    (jmaker-generall-dialog-setup-selected root)

    ;; Insert the dialog header.
    (widget-insert (format jmaker-generall-dialog-header root))

    ;; Insert the selection checkboxes.
    (mapcar
     #'(lambda (dir)
         (let ((file (expand-file-name "Makefile" dir)))
           (widget-create
            'checkbox
            :value  (memq dir jmaker-generall-dialog-selected)
            :format "%[%v%] %d"
            :tag    dir
            :doc    (concat (file-relative-name file root)
                            (if (file-exists-p file)
                                " (OVER)"
                              " (NEW)"))
            :notify 'jmaker-generall-dialog-toggle-selection)))
     jmaker-generall-dialog-selected)

    (widget-insert "\n\n")

    ;; Insert the Ok button.
    (widget-create
     'push-button
     :notify (lambda (&rest ignore)
               (let ((n (length jmaker-generall-dialog-selected))
                     (callback jmaker-generall-dialog-callback-fun)
                     (root jmaker-generall-dialog-callback-arg))
                 (kill-buffer (current-buffer))
                 (mapcar 'jmaker-create-makefile
                         jmaker-generall-dialog-selected)
                 (message "%S Makefile%s generated."
                          (if (= n 0) "No" n)
                          (if (> n 1) "s" ""))
                 (and callback (funcall callback root))))
     "Ok")

    (widget-insert " ")

    ;; Insert the Cancel button.
    (widget-create 'push-button
                   :notify 'jmaker-cancel-dialog
                   "Cancel")

    ;; Display the dialog.
    (jmaker-dialog-mode)
    (widget-setup)
    (goto-char (point-min))))

;;; User commands
;;
;;;###autoload
(defun jmaker-generate-makefile (root)
  "Generate and edit a Java Makefile in directory ROOT.
If a Makefile already exists the ask for confirmation to overwrite it.
Give also the option to update or create Makefiles in subdirectories
of ROOT where Java files exist.  If so, display a dialog to select the
Makefiles that will be created or overwritten."
  (interactive "DDirectory: ")
  (if (y-or-n-p (format "Update or create the Makefiles in %s?" root))
      (jmaker-generall-dialog root 'jmaker-edit-makefile)
    (jmaker-edit-makefile root)))

;;;###autoload
(defun jmaker-customize ()
  "Customize jmaker options."
  (interactive)
  (customize-group "jmaker"))

;;; JMaker menu
;;
(defvar jmaker-menu
  (list "JMaker"
        ["New Makefile..."  jmaker-generate-makefile t]
        ["Options..."       jmaker-customize t]
        ["Make"             compile t]
        ["-"                ignore nil]
        (concat "jmaker " jmaker-version))
  "jmaker menu.")

(add-hook 'jde-mode-hook
          #'(lambda ()
              (require 'easymenu)
              (if (featurep 'xemacs)
                  (add-submenu '("JDE") jmaker-menu)
                (easy-menu-add-item
                 jde-mode-map '("menu-bar") jmaker-menu))))

(provide 'jmaker)

;;; jmaker.el ends here
