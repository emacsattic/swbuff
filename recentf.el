;; recentf.el --- setup a menu of recently opened files

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: David Ponce <david.ponce@wanadoo.fr>
;; Created: July 19 1999
;; Keywords: customization

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'easymenu)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(defconst recentf-save-file-header
  ";;; Automatically generated by `recentf' on %s.\n"
  "Header to be written into the `recentf-save-file'.")

(defvar recentf-list nil
  "List of recently opened files.")

(defvar recentf-update-menu-p t
  "Non-nil if the recentf menu must be updated.")

(defvar recentf-initialized-p nil
  "Non-nil if recentf already initialized.")

;; IMPORTANT: This function must be defined before the following defcustoms
;; because it is used in their :set clause. To avoid byte-compiler warnings
;; the `symbol-value' function is used to access the `recentf-menu-path'
;; and `recentf-menu-title' values.
(defun recentf-menu-customization-changed (sym val)
  "Function called when menu customization has changed.
It removes the recentf menu and forces its complete redrawing."
  (when recentf-initialized-p
    (easy-menu-remove-item nil 
                           (symbol-value 'recentf-menu-path)
                           (symbol-value 'recentf-menu-title))
    (setq recentf-update-menu-p t))
  (custom-set-default sym val))

(defgroup recentf nil
  "Maintain a menu of recently opened files."
  :version "21.1"
  :group 'files)

(defcustom recentf-max-saved-items 20
  "*Maximum number of items saved to `recentf-save-file'."
  :group 'recentf
  :type 'integer)

(defcustom recentf-save-file (expand-file-name "~/.recentf")
  "*File to save `recentf-list' into."
  :group 'recentf
  :type 'file)

(defcustom recentf-exclude nil
  "*List of regexps for filenames excluded from `recentf-list'."
  :group 'recentf
  :type '(repeat regexp))

(defcustom recentf-menu-title "Open Recent"
  "*Name of the recentf menu."
  :group 'recentf
  :type 'string
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-path '("files")
  "*Path where to add the recentf menu.
If nil add it at top-level (see also `easy-menu-change')."
  :group 'recentf
  :type '(choice (const :tag "Top Level" nil)
                 (sexp :tag "Menu Path"))
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-before "open-file"
  "*Name of the menu before which the recentf menu will be added.
If nil add it at end of menu (see also `easy-menu-change')."
  :group 'recentf
  :type '(choice (string :tag "Name")
                 (const :tag "Last" nil))
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-action 'recentf-find-file
  "*Function to invoke with a filename item of the recentf menu.
The default action `recentf-find-file' calls `find-file' to edit an
existing file.  If the file does not exist or is not readable, it is
not edited and its name is removed from `recentf-list'. You can use
`find-file' instead to open non-existing files and keep them in the
list of recently opened files."
  :group 'recentf
  :type 'function
  :set 'recentf-menu-customization-changed)

(defcustom recentf-max-menu-items 10
  "*Maximum number of items in the recentf menu."
  :group 'recentf
  :type 'integer
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-filter nil
  "*Function used to filter files displayed in the recentf menu.
Nil means no filter.  The following functions are predefined:

- - `recentf-sort-ascending' to sort menu items in ascending order.
- - `recentf-sort-descending' to sort menu items in descending order.
- - `recentf-sort-basenames-ascending' to sort file names in descending order.
- - `recentf-sort-basenames-descending' to sort file names in descending order.
- - `recentf-show-basenames' to show file names (no directories) in menu items.
- - `recentf-show-basenames-ascending' to show file names in ascending order.
- - `recentf-show-basenames-descending' to show file names in descending order.

The filter function is called with one argument, the list of menu elements
used to build the menu and must return a new list of menu elements (see
`recentf-menu-elements' for menu element form)."
  :group 'recentf
  :type 'function
  :set 'recentf-menu-customization-changed)

(defcustom recentf-menu-append-commands-p t
  "*If not-nil command items are appended to the menu."
  :group 'recentf
  :type 'boolean
  :set 'recentf-menu-customization-changed)

(defcustom recentf-keep-non-readable-files-p nil
  "*If nil (default), non-readable files are not kept in `recentf-list'."
  :group 'recentf
  :type 'boolean
  :set  '(lambda (sym val)
           (if val
               (remove-hook 'kill-buffer-hook 'recentf-remove-file-hook)
             (add-hook 'kill-buffer-hook 'recentf-remove-file-hook))
           (custom-set-default sym val)))

(defcustom recentf-mode nil
  "Toggle recentf mode.
When recentf mode is enabled, it maintains a menu for visiting files that
were operated on recently.
Setting this variable directly does not take effect;
use either \\[customize] or the function `recentf-mode'."
  :set (lambda (symbol value)
         (recentf-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'recentf
  :require 'recentf)

(defcustom recentf-load-hook nil
   "*Normal hook run at end of loading the `recentf' package."
  :group 'recentf
  :type 'hook)

;;;###autoload
(defun recentf-mode (&optional arg)
  "Toggle recentf mode.
With prefix ARG, turn recentf mode on if and only if ARG is positive.
Returns the new status of recentf mode (non-nil means on).

When recentf mode is enabled, it maintains a menu for visiting files that
were operated on recently."
  (interactive "P")
  (let ((on-p (if arg
                  (> (prefix-numeric-value arg) 0)
                (not recentf-mode))))
    (if on-p
        (unless recentf-initialized-p
          (setq recentf-initialized-p t)
          (if (file-readable-p recentf-save-file)
              (load-file recentf-save-file))
          (setq recentf-update-menu-p t)
          (add-hook 'find-file-hooks       'recentf-add-file-hook)
          (add-hook 'write-file-hooks      'recentf-add-file-hook)
          ;;    (add-hook 'activate-menubar-hook 'recentf-update-menu-hook)
          (add-hook 'menu-bar-update-hook  'recentf-update-menu-hook)
          (add-hook 'kill-emacs-hook       'recentf-save-list))
      (when recentf-initialized-p
        (setq recentf-initialized-p nil)
        (recentf-save-list)
        (easy-menu-remove-item nil recentf-menu-path recentf-menu-title)
        (remove-hook 'find-file-hooks       'recentf-add-file-hook)
        (remove-hook 'write-file-hooks      'recentf-add-file-hook)
        ;;    (remove-hook 'activate-menubar-hook 'recentf-update-menu-hook)
        (remove-hook 'menu-bar-update-hook  'recentf-update-menu-hook)
        (remove-hook 'kill-emacs-hook       'recentf-save-list)))
    (setq recentf-mode on-p)))

(defun recentf-add-file-hook ()
  "Insert the name of the file just opened or written into `recentf-list'."
  (and buffer-file-name (recentf-add-file buffer-file-name))
  nil)

(defun recentf-remove-file-hook ()
  "When a buffer is killed remove a non readable file from `recentf-list'."
  (and buffer-file-name (recentf-remove-if-non-readable buffer-file-name))
  nil)

(defun recentf-update-menu-hook ()
  "Update the recentf menu from the current `recentf-list'."
  (when recentf-update-menu-p
    (condition-case nil
        (progn
          (setq recentf-update-menu-p nil)
          (easy-menu-change recentf-menu-path
                            recentf-menu-title
                            (recentf-make-menu-items)
                            recentf-menu-before))
      (error nil))))

;;;###autoload
(defun recentf-save-list ()
  "Save the current `recentf-list' to the file `recentf-save-file'."
  (interactive)
  (let ((saved-list (recentf-elements recentf-max-saved-items)))
    (with-temp-buffer
      (erase-buffer)
      (insert (format recentf-save-file-header (current-time-string)))
      (insert "(setq recentf-list\n      '(\n")
      (mapcar '(lambda (e)
                 (insert (format "        %S\n" e)))
              saved-list)
      (insert "        ))")
      (if (file-writable-p recentf-save-file)
          (write-region (point-min) (point-max) recentf-save-file))
      (kill-buffer (current-buffer))))
  nil)

(defvar recentf-edit-selected-items nil
  "Used by `recentf-edit-list' to hold the list of files to be deleted
from `recentf-list'.")

(defun recentf-edit-list-action (widget &rest ignore)
  "Checkbox widget action used by `recentf-edit-list' to select/unselect a file."
  (let ((value (widget-get widget ':tag)))
    ;; if value is already in the selected items
    (if (memq value recentf-edit-selected-items)
        ;; then remove it
        (progn
          (setq recentf-edit-selected-items
                (delq value recentf-edit-selected-items))
          (message "%s removed from selection." value))
      ;; else add it
      (progn
        (setq recentf-edit-selected-items
              (nconc (list value) recentf-edit-selected-items))
        (message "%s added to selection." value)))))
  
;;;###autoload
(defun recentf-edit-list ()
  "Allow the user to edit the files that are kept in the recent list."
  (interactive)
  (with-current-buffer (get-buffer-create (concat "*" recentf-menu-title " - Edit list*"))
    (switch-to-buffer (current-buffer))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))
    (setq recentf-edit-selected-items nil)
    ;; Insert the dialog header
    (widget-insert "Select the files to be deleted from the 'recentf-list'.\n\n")
    (widget-insert "Click on Ok to update the list or on Cancel to quit.\n" )
    ;; Insert the list of files as checkboxes
    (mapcar '(lambda (item)
               (widget-create 'checkbox
                              :value nil ; unselected checkbox
                              :format "\n %[%v%]  %t"
                              :tag item
                              :notify 'recentf-edit-list-action))
            recentf-list)
    (widget-insert "\n\n")
    ;; Insert the Ok button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (if recentf-edit-selected-items
                                 (progn (kill-buffer (current-buffer))
                                        (mapcar '(lambda (item)
                                                   (setq recentf-list
                                                         (delq item recentf-list)))
                                                recentf-edit-selected-items)
                                        (message "%S file(s) removed from the list"
                                                 (length recentf-edit-selected-items))
                                        (setq recentf-update-menu-p t))
                               (message "No file selected.")))
                   "Ok")
    (widget-insert " ")
    ;; Insert the Cancel button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (kill-buffer (current-buffer))
                             (message "Command canceled."))
                   "Cancel")
    (use-local-map widget-keymap)
    (widget-setup)))

;;;###autoload
(defun recentf-cleanup ()
  "Remove all non-readable and excluded files from `recentf-list'."
  (interactive)
  (setq recentf-list
        (delq nil
              (mapcar '(lambda (filename)
                         (and (file-readable-p filename)
                              (recentf-include-p filename)
                              filename))
                      recentf-list)))
  (setq recentf-update-menu-p t))

(defun recentf-open-more-files-action (widget &rest ignore)
  "Button widget action used by `recentf-open-more-files' to open a file."
  (kill-buffer (current-buffer))
  (funcall recentf-menu-action (widget-value widget)))

;;;###autoload
(defun recentf-open-more-files ()
  "Allow the user to open files that are not in the menu."
  (interactive)
  (with-current-buffer (get-buffer-create (concat "*" recentf-menu-title " - More*"))
    (switch-to-buffer (current-buffer))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (let ((all (overlay-lists)))
      ;; Delete all the overlays.
      (mapcar 'delete-overlay (car all))
      (mapcar 'delete-overlay (cdr all)))
    ;; Insert the dialog header
    (widget-insert "Click on a file to open it or on Cancel to quit.\n\n")
    ;; Insert the list of files as buttons
    (mapcar '(lambda (menu-element)
               (let ((menu-item (car menu-element))
                     (file-path (cdr menu-element)))
                 (widget-create 'push-button
                                :button-face 'default
                                :tag menu-item
                                :help-echo (concat "Open " file-path)
                                :format "%[%t%]"
                                :notify 'recentf-open-more-files-action
                                file-path)
                 (widget-insert "\n")))
            (funcall (or recentf-menu-filter 'identity)
                     (mapcar '(lambda (item) (cons item item))
                             (nthcdr recentf-max-menu-items recentf-list))))
    (widget-insert "\n")
    ;; Insert the Cancel button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (kill-buffer (current-buffer))
                             (message "Command canceled."))
                   "Cancel")
    (use-local-map widget-keymap)
    (widget-setup)))

(defvar recentf-menu-items-for-commands
  (list ["Cleanup list" recentf-cleanup t]
        ["Edit list..." recentf-edit-list t]
        ["Save list now" recentf-save-list t]
        (vector "Recentf Options..." '(customize-group "recentf") t))
  "List of menu items for recentf commands.")

(defun recentf-make-menu-items ()
  "Make menu items from `recentf-list'."
  (let ((file-items
         (mapcar 'recentf-make-menu-item
                 (funcall (or recentf-menu-filter 'identity)
                          (recentf-menu-elements recentf-max-menu-items)))))
    (append (or file-items (list ["No files" t nil]))
            (and (< recentf-max-menu-items (length recentf-list))
                 (list ["More..." recentf-open-more-files t]))
            (and recentf-menu-append-commands-p
                 (cons ["---" nil nil]
                       recentf-menu-items-for-commands)))))

(defun recentf-make-menu-item (menu-element)
  "Make a menu item from a menu element (see `recentf-menu-elements')."
  (vector (car menu-element) (list recentf-menu-action (cdr menu-element)) t))

(defun recentf-add-file (filename)
  "Add or move FILENAME at the beginning of `recentf-list'.
Does nothing if FILENAME matches one of the `recentf-exclude' regexps."
  (when (recentf-include-p filename)
    (setq recentf-list (cons filename (delete filename recentf-list)))
    (setq recentf-update-menu-p t)))

(defun recentf-remove-if-non-readable (filename)
  "Remove FILENAME from `recentf-list' if not readable."
  (unless (file-readable-p filename)
    (setq recentf-list (delete filename recentf-list))
    (setq recentf-update-menu-p t)))

(defun recentf-find-file (filename)
  "Edit file FILENAME using `find-file'.
If FILENAME is not readable it is removed from `recentf-list'."
  (if (file-readable-p filename)
      (find-file filename)
    (progn
      (message "File `%s' not found." filename)
      (setq recentf-list (delete filename recentf-list))
      (setq recentf-update-menu-p t))))

(defun recentf-include-p (filename)
  "Return t if FILENAME matches none of the `recentf-exclude' regexps."
  (let ((rl recentf-exclude))
    (while (and rl (not (string-match (car rl) filename)))
      (setq rl (cdr rl)))
    (null rl)))

(defun recentf-elements (n)
  "Return a list of the first N elements of `recentf-list'."
  (let ((lh nil) (l recentf-list))
    (while (and l (> n 0))
      (setq lh (cons (car l) lh))
      (setq n (1- n))
      (setq l (cdr l)))
    (nreverse lh)))

(defun recentf-menu-elements (n)
  "Return a list of the first N menu elements from `recentf-list'.
Each menu element has this form:

 (MENU-ITEM . FILE-PATH)

MENU-ITEM is the menu item string displayed.

FILE-PATH is the path used to open the file when the corresponding MENU-ITEM
is selected.

At the start each MENU-ITEM is set to its corresponding FILE-PATH."
  (mapcar '(lambda (item) (cons item item)) (recentf-elements n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined menu filter functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recentf-sort-ascending (l)
  "Sort the list of menu elements L in ascending order.
The MENU-ITEM part of each menu element is compared."
  (sort l '(lambda (e1 e2) (string-lessp (car e1) (car e2)))))

(defun recentf-sort-descending (l)
  "Sort the list of menu elements L in descending order.
The MENU-ITEM part of each menu element is compared."
  (sort l '(lambda (e1 e2) (string-lessp (car e2) (car e1)))))

(defun recentf-sort-basenames-ascending (l)
  "Sort the list of menu elements L in ascending order.
Only file names (without directories) are compared."
  (sort l '(lambda (e1 e2) (string-lessp
                            (file-name-nondirectory (cdr e1))
                            (file-name-nondirectory (cdr e2))))))

(defun recentf-sort-basenames-descending (l)
  "Sort the list of menu elements L in descending order.
Only file names (without directories) are compared."
  (sort l '(lambda (e1 e2) (string-lessp
                            (file-name-nondirectory (cdr e2))
                            (file-name-nondirectory (cdr e1))))))

(defun recentf-show-basenames (l)
  "Filter the list of menu elements L to show only file names (no directories)
in the menu. When file names are duplicated their directory component is added."
  (let ((names  (mapcar '(lambda (item) (file-name-nondirectory (cdr item))) l))
        (dirs   (mapcar '(lambda (item) (file-name-directory (cdr item))) l))
        (pathes (mapcar 'cdr l))
        (pos    -1)
        item filtered-items filtered-list)
    (while names
      (setq item  (car names))
      (setq names (cdr names))
      (setq pos   (1+ pos))
      (setq filtered-list
            (cons (cons (if (or (member item names) (member item filtered-items))
                            (concat item " (" (nth pos dirs) ")")
                          item)
                        (nth pos pathes))
                  filtered-list))
      (setq filtered-items (cons item filtered-items)))
    (nreverse filtered-list)))

(defun recentf-show-basenames-ascending (l)
  "Filter the list of menu elements L to show only file names in the menu,
sorted in ascending order. This filter combines the `recentf-sort-basenames-ascending'
and `recentf-show-basenames' filters."
  (recentf-show-basenames (recentf-sort-basenames-ascending l)))

(defun recentf-show-basenames-descending (l)
  "Filter the list of menu elements L to show only file names in the menu,
sorted in descending order. This filter combines the `recentf-sort-basenames-descending'
and `recentf-show-basenames' filters."
  (recentf-show-basenames (recentf-sort-basenames-descending l)))

(provide 'recentf)

(run-hooks 'recentf-load-hook)

;;; recentf.el ends here.
