;; jsee.el --- Java source documentation viewer

;; Copyright (C) 1998, 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 3 Dec 1998
;; Version: 2.0
;; Keywords: jde
;; VC: $Id: jsee.el,v 1.6 2000/08/11 14:59:51 david_ponce Exp $

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

;; This package is a JDE add-on which automatically generates and view
;; documentation generated from the source of the java file in the
;; current buffer.  The default implementation uses javadoc as
;; generator and view the documentation with the default browser.

;; Installation:
;;
;; Put this file on your Emacs load-path and add (require 'jsee) into
;; your Emacs startup file.

;; Usage:
;;
;; M-x `jsee-browse-api-doc' to generates and view the source
;; documentation of the java file in the current buffer.
;;
;; M-x `jsee-customize' to customize all the jsee options.

;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce at <david@dponce.com>

;;; Change Log:

;; $Log: jsee.el,v $
;; Revision 1.6  2000/08/11 14:59:51  david_ponce
;; Major rewrite:
;;
;; Follow standard Emacs coding convention.
;; Better separation between generic and javadoc specific code.
;; Use JDE semantic bovinator parser to retrieve the package statement.
;; Automatically handle javadoc version 1 or version 2 HTML style.
;; Some customizable variable names have changed so update you
;; configuration.
;;
;; Revision 1.5  1999/07/05 21:27:11  ebat311
;; Use Emacs/XEmacs compatible key mapping in `jsee-default-load-hook'.
;;
;; Revision 1.4  1999-05-26 10:22:30+02  ebat311
;; Fixed - XEmacs compatibility of key mapping in
;; `jsee-default-load-hook'.
;;
;; Revision 1.3  1999-04-22 23:28:44+02  ebat311
;; Added `autoload' cookies.
;;
;; Revision 1.2  1999-03-01 14:56:25+01  ebat311
;; FIXED - when a java file doesn't have a package name the
;; `jsee-get-javadoc-url' and `jsee-get-javadoc1.2-url' functions
;; built invalid URL.
;; Thanks to Leif Jonsson <albedo@hem2.passagen.se> who
;; has reported this bug and suggested the correction.
;;
;; Revision 1.1  1998/12/08 09:43:55  ebat311
;; Initial revision
;;

;;; Code:
(require 'jde)

(defconst jsee-version "2.0 $Date: 2000/08/11 14:59:51 $"
  "jsee version information.")

;;;
;;; Global customization
;;;

;; ALL CUSTOMIZABLE VARIABLES ARE PREFIXED WITH `jde-' TO ALLOW THEIR
;; SAVING IN A JDE PROJECT FILE.

(defgroup jde-jsee nil
  "Global jsee customization."
  :group 'jde
  :prefix "jde-jsee-")

(defcustom jde-jsee-doc-generator "javadoc"
  "*The Java API Documentation Generator.
Specifies the path to the tool to be used to generate API
documentation for the program in the current buffer.  The default is
the JDK tool (javadoc)."
  :group 'jde-jsee
  :type 'string)

(defcustom jde-jsee-get-doc-generator-options-function 'jsee-javadoc-get-options
  "*Function used to get the doc generator command line options.
The default function provided builds command line options for the JDK
javadoc tool."
  :group 'jde-jsee
  :type 'function)

(defcustom jde-jsee-pre-generate-function 'jsee-javadoc-delete-previous-output
  "*Function called before running the doc generator.
If nil does nothing. The default function provided
`jsee-javadoc-delete-previous-output' deletes any HTML file previously
generated by javadoc for the current Java file."
  :group 'jde-jsee
  :type 'function)

(defcustom jde-jsee-post-generate-function 'jsee-javadoc-browse-output
  "*Function called after the doc generator completed.
If nil does nothing. The default function provided
`jsee-javadoc-browse-output' browses the HTML file generated by
javadoc for the current Java file."
  :group 'jde-jsee
  :type 'function)

(defcustom jde-jsee-load-hook '(jsee-default-load-hook)
  "*Hook run when package has been loaded.
The default hook provided `jsee-default-load-hook' maps the `jde-mode'
key `C-f1' to the `jsee-browse-api-doc' command."
  :group 'jde-jsee
  :type 'hook)

;;;
;;; Common functions
;;;

(defun jsee-fullpath (path)
  "Return the full path of the given PATH.
~ (HOME) and environment variables references are expanded."
  (expand-file-name (substitute-in-file-name path)))

(defun jsee-call (fvar &optional with-error &rest with-args)
  "Wrapper function to `funcall'. FVAR is a symbol bound to the
function to be called. If optional parameter WITH-ERROR in non-nil an
error is signaled if FVAR does not define a valid function. WITH-ARGS
defines the parameters passed to the function."
  (if (and (symbolp fvar) (fboundp fvar))
      (apply fvar with-args)
    (if with-error
        (error "Invalid function %s" fvar))))

(defun jsee-delete-file (file)
  "Helper function used to delete the file FILE. Returns non-nil if
the file is deleted or nil if not."
  (if (stringp file)
      (condition-case err
          (progn
            (delete-file file)
            (message "File %s deleted." file)
            t)
        (error
         (message "%s" (error-message-string err))
         nil))))

(defun jsee-get-package-name ()
  "Return the package name in the current file.
This function uses the semantic bovinator parser table to find the
package statement."
  (let* ((tokens (semantic-bovinate-toplevel nil t t))
         (packages (semantic-find-nonterminal-by-token 'package tokens)))
    (if packages
        (semantic-token-name (car packages)))))

(defun jsee-get-file-class ()
  "Return the qualified class name of the current file."
  (let ((package (jsee-get-package-name)))
    (setq package
          (if package
              (concat package ".")
            ""))
    (concat package
            (file-name-sans-extension
             (file-name-nondirectory buffer-file-name)))))

(defvar jsee-file-class nil
  "Hold the qualified class name of the current file. This variable is
local to the compilation buffer and can be used by the
`jde-jsee-post-generate-function'.")

;;;
;;; Specific to javadoc generator
;;;

(defgroup jde-jsee-javadoc nil
  "jsee customization specific to the javadoc generator."
  :group 'jde-jsee
  :prefix "jde-jsee-javadoc-")

(defcustom jde-jsee-javadoc-output-dir "$TEMP/jsee"
  "*The working directory in which javadoc will generate HTML files.
This directory must exists. For better results with JDK 1 javadoc
style you can copy into it the 'images' sub-directory of the JDK 1 API
documentation."
  :group 'jde-jsee-javadoc
  :type 'string)

(defcustom jde-jsee-javadoc-others-options ""
  "*javadoc options as a string of command-line arguments.
The value of this variable should be a string of switches understood
by javadoc, for example, \"-author -version\". This variable is
intended to be used to set javadoc options not otherwise defined by
jsee."
  :group 'jde-jsee-javadoc
  :type 'string)

(defcustom jde-jsee-javadoc-version-option t
  "*If non-nil javadoc will include @version paragraphs."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-nodeprecated-option nil
  "*If non-nil javadoc will exclude @deprecated paragraphs."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-author-option t
  "*If non-nil javadoc will include @author paragraphs."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-noindex-option t
  "*If non-nil javadoc will not generate method and field index."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-notree-option t
  "*If non-nil javadoc will not generate the class/interface hierarchy."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-public-option nil
  "*If non-nil javadoc will show only public classes and members."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-protected-option t
  "*If non-nil javadoc will show protected/public classes and members."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-package-option nil
  "*If non-nil javadoc will show package/protected/public classes and members."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defcustom jde-jsee-javadoc-private-option nil
  "*If non-nil javadoc will show all classes and members."
  :group 'jde-jsee-javadoc
  :type 'boolean)

(defun jsee-javadoc-get-output-dir ()
  "Return the full directory name from `jde-jsee-javadoc-output-dir'
or `default-directory' if nil."
  (if (stringp jde-jsee-javadoc-output-dir)
      (file-name-as-directory (jsee-fullpath jde-jsee-javadoc-output-dir))
    default-directory))

(defun jsee-javadoc-get-classpath-option ()
  "Return the javadoc -classpath value from respectively
`jde-compile-option-classpath' or `jde-global-classpath'."
  (if jde-compile-option-classpath
      (jde-build-classpath-arg jde-compile-option-classpath
                               jde-quote-classpath)
    (if jde-global-classpath
        (jde-build-classpath-arg jde-global-classpath
                                 jde-quote-classpath)
      "")))

(defun jsee-javadoc-get-options ()
  "Return the javadoc command line options."
  (let ((options (jsee-javadoc-get-classpath-option)))
    (setq options (concat options " -d " (jsee-javadoc-get-output-dir)))
    (if jde-jsee-javadoc-version-option
        (setq options (concat options " -version")))
    (if jde-jsee-javadoc-nodeprecated-option
        (setq options (concat options " -nodeprecated")))
    (if jde-jsee-javadoc-author-option
        (setq options (concat options " -author")))
    (if jde-jsee-javadoc-noindex-option
        (setq options (concat options " -noindex")))
    (if jde-jsee-javadoc-notree-option
        (setq options (concat options " -notree")))
    (if jde-jsee-javadoc-public-option
        (setq options (concat options " -public")))
    (if jde-jsee-javadoc-protected-option
        (setq options (concat options " -protected")))
    (if jde-jsee-javadoc-package-option
        (setq options (concat options " -package")))
    (if jde-jsee-javadoc-private-option
        (setq options (concat options " -private")))
    (if (not (string-equal jde-jsee-javadoc-others-options ""))
        (setq options (concat options " " jde-jsee-javadoc-others-options)))
    options))

(defun jsee-javadoc-get-output-filename ()
  "Return the javadoc (version 1 or 2) HTML file name associated to
the current Java file. If not found return nil."
  (let ((docset (jsee-javadoc-get-output-dir))
        (class  jsee-file-class))
    (or (jde-help-lookup-java1-javadoc class docset)
        (jde-help-lookup-java2-javadoc class docset))))

(defun jsee-javadoc-delete-previous-output ()
  "Remove any previous javadoc (all versions) HTML file generated for
the current Java file."
  (let ((docset (jsee-javadoc-get-output-dir))
        (class  jsee-file-class))
    (jsee-delete-file (jde-help-lookup-java1-javadoc class docset))
    (jsee-delete-file (jde-help-lookup-java2-javadoc class docset))))
  
(defun jsee-javadoc-browse-output ()
  "Browse the HTML file generated by javadoc (version 1 or 2) for the
current Java file."
  (let ((output (jsee-javadoc-get-output-filename)))
    (cond (output
           (message "browse-url-of-file %s" output)
           (browse-url-of-file output))
          (t
           (message "Doc file not found for %s in %s"
                    jsee-file-class
                    (jsee-javadoc-get-output-dir))))))

;;;
;;; Doc generator process handling
;;;

(defun jsee-pre-generate-handler ()
  "Handle operations done before running the doc generator."
  (message "jsee-pre-generate-handler in buffer %s" (buffer-name))
  (make-local-variable 'jsee-file-class)
  (jsee-call jde-jsee-pre-generate-function)
  (set (make-local-variable 'compilation-finish-function)
       'jsee-post-generate-handler))

(defun jsee-post-generate-handler (buffer msg)
  "Handle operation done after running the doc generator."
  (message "jsee-post-generate-handler in buffer %s" (buffer-name))
  (if (string-equal msg "finished\n")
      (jsee-call jde-jsee-post-generate-function)))

(defun jsee-make-doc-generator-command ()
  "Return the full command used to generate the Java documentation."
  (concat jde-jsee-doc-generator " " 
          (jsee-call jde-jsee-get-doc-generator-options-function 'with-error) 
          " "
          (file-name-nondirectory buffer-file-name)))

(defun jsee-run-doc-generator ()
  "Run the Java API Documentation Generator."
  (let ((compile-command (jsee-make-doc-generator-command)))
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-compile from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (eq system-type 'windows-nt)
        (let ((temp last-nonmenu-event))
          ;; The next line makes emacs think that jde-compile
          ;; was invoked from the minibuffer, even when it
          ;; is actually invoked from the menu-bar.
          (setq last-nonmenu-event t)
          (save-some-buffers (not compilation-ask-about-save) nil)
          (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (let ((compilation-process-setup-function 'jsee-pre-generate-handler))
      (setq jsee-file-class (jsee-get-file-class))
      (compile-internal compile-command "No more errors"))))

;;;
;;; Hooks and commands
;;;

;;;###autoload
(defun jsee-browse-api-doc ()
  "Run the Java API Documentation Generator for the Java source file
in the current buffer and browse the resulting HTML file."
  (interactive)
  (if (eq major-mode 'jde-mode)
      (if (local-variable-p 'semantic-toplevel-bovine-table)
          (jsee-run-doc-generator)
        (error "This command needs a version of JDE with the semantic bovinator."))
    (error "This command must be run in 'jde-mode'.")))

;;;###autoload
(defun jsee-customize ()
  "Customization of the group jde-jsee."
  (interactive)
  (customize-group "jde-jsee"))

(defun jsee-default-load-hook ()
  "Default hook run when package has been loaded. It maps C-f1 to
`jsee-browse-api-doc'."
  (define-key java-mode-map [(control f1)] 'jsee-browse-api-doc))

(provide 'jsee)
(run-hooks 'jde-jsee-load-hook)

;;; jsee.el ends here
