;;; tabbar.el --- Display a tab bar in the header line

;; Copyright (C) 2003 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 25 February 2003
;; Keywords: convenience
;; Revision: $Id: tabbar.el,v 1.7 2003/03/10 16:55:40 ponce Exp $

(defconst tabbar-version "1.0")

;; This file is not part of GNU Emacs.

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
;; This library provides a minor mode to display tabs in the header
;; line.  It works only on GNU Emacs 21, when a mouse is available.
;;
;; M-x `tabbar-mode' toggle the display of the tab bar, globally.
;;
;; M-x `tabbar-local-mode' toggle the display of the tab bar, locally
;; in the current buffer, when the global mode in on.  This mode
;; permit to see the tab bar in a buffer where the header line is
;; already used by another mode (like `info' buffers).  That command
;; is particularly useful when it is given a keyboard shortcut, like
;; this:
;;
;;   (global-set-key [(control f10)] 'tabbar-local-mode)
;;
;; Core
;; ----
;;
;; The content of the tab bar is represented by an internal data
;; structure: a tab set.  A tab set is a collection of tabs,
;; identified by an unique name.  In a tab set, at any time, one and
;; only one tab is designated as selected within the tab set.
;;
;; A tab is a simple data structure giving: the value of the tab, and
;; a reference to its tab set container.  A tab value can be any Lisp
;; object, even if the most common value is probably a string.  Each
;; tab object is guaranteed to be unique.

;; A tab set is displayed on the tab bar through a "view" defined by
;; the index of the leftmost tab shown.  Thus, it is possible to
;; scroll the tab bar horizontally, by changing the start index of the
;; tab set view.
;;
;; The visual representation of a tab set is a list a
;; `header-line-format' template elements.  Each template element is
;; the visual representation of a tab.  When the visual representation
;; of a tab is required, the function specified in the variable
;; `tabbar-tab-label-function' is called to obtain a label (a text
;; representation) for that tab.  Also, the function specified in the
;; variable `tabbar-help-on-tab-function' is called when the mouse is
;; on a tab.  That function is passed the tab and can return a help
;; string to display.  Finally, when a tab is selected by clicking on
;; it, the function specified in the variable
;; `tabbar-select-tab-function' is called with the mouse event
;; received, and the tab.
;;
;; To increase performance, the tab set automatically maintains its
;; visual representation in a cache.  As far as possible, that cache
;; is used to display the tab set, and refreshed only when necessary.
;;
;; Several tab sets can be maintained at the same time.  Only one is
;; displayed on the tab bar, it is obtained by calling the function
;; specified in the variable `tabbar-current-tabset-function'.
;;
;; A special tab set is maintained, that contains the list of
;; currently selected tabs, in existing tab sets.  For example, a such
;; tab set can be used to display a tab bar with a tab for each
;; created tab set, allowing to switch to another tab set by clicking
;; on the corresponding tab.
;;
;; Three buttons are displayed to the left, on the tab bar: the "home"
;; button, the "scroll left" and the "scroll right" buttons.  The
;; "home" button is a general purpose button used to change something
;; on the tab bar.  The scroll left and scroll right buttons are used
;; to scroll tabs horizontally.  The following variables are
;; available, for respectively the `home', `scroll-left' and
;; `scroll-right' value of `<button>':
;;
;; `tabbar-<button>-function'
;;    Specify a function called when clicking on the button.  The
;;    function is passed the mouse event received.
;;
;; `tabbar-<button>-help-function'
;;    Specify a function to obtain a help string displayed when the
;;    mouse is onto the button.  The function is called with no
;;    arguments.
;;
;; The appearance of tabs and buttons is also customizable (see the
;; code for more details).
;;
;; Buffer tabs
;; -----------
;;
;; The default tab bar implementation provided, displays buffers in
;; dedicated tabs.  Selecting a tab, switch (mouse-1), or pop
;; (mouse-2), to the buffer it contains.
;;
;; The list of buffers put in tabs is provided by the function
;; specified in the variable `tabbar-buffer-list-function'.  The
;; default function: `tabbar-buffer-list', excludes buffers that are
;; not visiting a file and whose name starts with a space.
;;
;; Buffers are organized in groups, each one represented by a tab set.
;; A buffer can have no group, or belong to more than one group.  The
;; function specified by the variable `tabbar-buffer-groups-function'
;; is called for each buffer to obtain its groups.  The default
;; function provided: `tabbar-buffer-groups' organizes buffers
;; depending on their major mode (see that function for details).
;;
;; The "home" button toggles display of buffer groups on the tab bar,
;; allowing to easily choose another buffer group by clicking on its
;; tab.
;;
;; The scroll buttons permit to scroll tabs when some of them are
;; outside the tab bar visible area.

;;; History:
;;

;;; Code:

;;; Options
;;
(defgroup tabbar nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom tabbar-current-tabset-function
  'tabbar-buffer-tabs
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-tab-label-function
  'tabbar-buffer-tab-label
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-select-tab-function
  'tabbar-buffer-select-tab
  "Function that select a tab.
The function is passed a mouse event and a tab, and should make it the
selected tab."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-help-on-tab-function
  'tabbar-buffer-help-on-tab
  "Function to obtain a help string for a tab.
The help string is displayed when the mouse is onto the button.  The
function is passed the tab and should return a help string or nil for
none."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-home-function
  'tabbar-buffer-toggle-group-mode
  "Function called when clicking on the tab bar home button.
The function is passed the mouse event received."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-home-help-function
  'tabbar-buffer-toggle-group-mode-help
  "Function to obtain a help string for the tab bar home button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-scroll-left-function
  'tabbar-scroll-left
  "Function that scrolls tabs on left.
The function is passed the mouse event received when clicking on the
scroll left button.  It should scroll the current tab set."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-scroll-left-help-function
  'tabbar-scroll-left-help
  "Function to obtain a help string for the scroll left button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-scroll-right-function
  'tabbar-scroll-right
  "Function that scrolls tabs on right.
The function is passed the mouse event received when clicking on the
scroll right button.  It should scroll the current tab set."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-scroll-right-help-function
  'tabbar-scroll-right-help
  "Function to obtain a help string for the scroll right button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments."
  :group 'tabbar
  :type 'function)

;;; Tab and tab sets
;;
;; A tab set is a collection of tabs identified by an unique name.  In
;; each tab set, at any time, one and only one tab is designated as
;; selected within the tab set.  A tab set is displayed on the tab bar
;; through a "view" defined by the index of the leftmost tab shown.
;;
;; A tab can be any Lisp object.  The most common value is probably a
;; string.  Each tab is guaranteed to be unique across all defined tab
;; sets.  So, having a tab, it is possible to find the tab set it
;; belongs to.
(defconst tabbar-tabsets-tabset-name "tabbar-tabsets-tabset"
  "Name of the special tab set of existing tab sets.")

(defsubst tabbar-make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst tabbar-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst tabbar-tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar tabbar-tabsets nil
  "The tab sets store.")

(defvar tabbar-current-tabset nil
  "The tab set currently displayed on the tab bar.")

(defvar tabbar-last-selected-tab nil
  "The last selected tab.")

(defun tabbar-init-tabsets-store ()
  "Initialize the tab set store."
  (setq tabbar-tabsets (make-vector 31 0)))

(defun tabbar-free-tabsets-store ()
  "Free the tab set store."
  (setq tabbar-tabsets nil))

(defmacro tabbar-map-tabsets (function)
  "Apply FUNCTION to each existing tab set.
Return the list of the results."
  (let ((result (make-symbol "result"))
        (tabset (make-symbol "tabset")))
    `(let (,result)
       (mapatoms #'(lambda (,tabset)
                     (setq ,result
                           (cons (funcall ,function ,tabset)
                                 ,result)))
                 tabbar-tabsets)
       (nreverse ,result))))

(defun tabbar-make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name tabbar-tabsets))
         (tabs (mapcar #'(lambda (object)
                           (tabbar-make-tab object tabset))
                       objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst tabbar-get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name tabbar-tabsets))

(defsubst tabbar-delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset tabbar-tabsets))

(defsubst tabbar-tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst tabbar-tab-values (tabset)
  "Return the list of tab values in TABSET."
  (mapcar 'tabbar-tab-value (tabbar-tabs tabset)))

(defsubst tabbar-get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (tabbar-tabs tabset)))

(defsubst tabbar-member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (tabbar-tab-tabset tab) tabset)
      (memq tab (tabbar-tabs tabset))))

(defsubst tabbar-template (tabset)
  "Return the template to display TABSET in the header line."
  (get tabset 'template))

(defsubst tabbar-set-template (tabset template)
  "Set the TABSET's header line format with TEMPLATE."
  (put tabset 'template template))

(defun tabbar-add-tab (tabset object &optional append)
  "Add to TABSET a tab with value OBJECT if there isn't one there yet.
If the tab is added, it is added at the beginning of the tab list,
unless the optional argument APPEND is non-nil, in which case it is
added at the end."
  (let ((tabs (tabbar-tabs tabset)))
    (if (tabbar-get-tab object tabset)
        tabs
      (let ((tab (tabbar-make-tab object tabset)))
        (tabbar-set-template tabset nil)
        (set tabset (if append
                        (append tabs (list tab))
                      (cons tab tabs)))))))

(defsubst tabbar-delete-tab (tab)
  "Remove TAB from its TABSET."
  (let ((tabset (tabbar-tab-tabset tab)))
    (tabbar-set-template tabset nil)
    (set tabset (delq tab (tabbar-tabs tabset)))))

(defsubst tabbar-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst tabbar-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (tabbar-tab-value (tabbar-selected-tab tabset)))

(defsubst tabbar-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (tabbar-selected-tab tabset)))

(defsubst tabbar-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (tabbar-member tab tabset)
    (or (tabbar-selected-p tab tabset)
        (tabbar-set-template tabset nil))
    (put tabset 'select tab)))

(defsubst tabbar-select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (tabbar-select-tab (tabbar-get-tab object tabset) tabset))

(defsubst tabbar-start (tabset)
  "Return the index of the first tab in the TABSET's view."
  (get tabset 'start))

(defsubst tabbar-view (tabset)
  "Return the list of tabs in the TABSET's view."
  (nthcdr (tabbar-start tabset) (tabbar-tabs tabset)))

(defun tabbar-scroll (tabset count)
  "Scroll the TABSET's view of COUNT tabs.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (tabbar-start tabset) count))
                    (1- (length (tabbar-tabs tabset))))))
    (when (/= start (tabbar-start tabset))
      (tabbar-set-template tabset nil)
      (put tabset 'start start))))

(defun tabbar-current-tabset (&optional update)
  "Return the current tab set, that will be displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`tabbar-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (when (and update tabbar-current-tabset-function)
    (setq tabbar-current-tabset
          (funcall tabbar-current-tabset-function))
    (or tabbar-last-selected-tab
        (setq tabbar-last-selected-tab
              (tabbar-selected-tab tabbar-current-tabset))))
  tabbar-current-tabset)

(defun tabbar-get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tab sets."
  (let ((tabsets-tabset
         (or (tabbar-get-tabset tabbar-tabsets-tabset-name)
             (tabbar-make-tabset tabbar-tabsets-tabset-name))))
    (set tabsets-tabset
         (delq t
               (tabbar-map-tabsets
                #'(lambda (tabset)
                    (or (eq tabset tabsets-tabset)
                        (tabbar-selected-tab tabset))))))
    (tabbar-scroll tabsets-tabset 0)
    (tabbar-set-template tabsets-tabset nil)
    tabsets-tabset))

;;; Buttons and separators
;;
(defconst tabbar-widget
  '(cons (string " ")
         (repeat :tag "Image"
                 :extra-offset 2
                 (restricted-sexp :tag "Spec"
                                  :match-alternatives (listp))))
  "Widget for editing a tab bar button or separator.")

(defun tabbar-setup-separator (variable value)
  "Set VARIABLE with specification of tab separator in VALUE.
Initialize `VARIABLE-value' with the template element to use in header
line, to display a separator on the tab bar."
  (let ((text (intern (format "%s-value" variable)))
        (image (condition-case nil
                   (find-image (cdr value))
                 (error nil))))
    (set text (propertize (if image " " (car value))
                          'face 'tabbar-separator-face
                          'display image))
    (custom-set-default variable value)
    ))

(defvar tabbar-separator-value nil
  "Text of the separator used between tabs.")

(defcustom tabbar-separator (list " ")
  "Separator used between tabs.
A separator is a string and a list of image specifications.
If image specifications are non-nil, try to use an image as separator.
Otherwise, use the given string.
To suppress the separator, use a zero length string and nil as image
specifications."
  :group 'tabbar
  :type tabbar-widget
  :set 'tabbar-setup-separator)

(defconst tabbar-button-widget
  '(cons
    (cons :tag "Enabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    (cons :tag "Disabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    )
  "Widget for editing a tab bar button.")

(defun tabbar-setup-button (variable value)
  "Set VARIABLE with the button specification in VALUE.
Initialize `VARIABLE-enable' and `VARIABLE-disable' with the template
elements to use in the header line, to respectively display an enabled
and a disabled button on the tab bar.
The variable `VARIABLE-keymap' must be set with the keymap used for the
enabled button.
The function `VARIABLE-help' must be defined to return the `help-echo'
string shown when the mouse is on the button."
  (let ((enabled  (intern (format "%s-enabled" variable)))
        (disabled (intern (format "%s-disabled" variable)))
        (keymap   (intern (format "%s-keymap" variable)))
        (help     (intern (format "%s-help" variable)))
        (image-en (condition-case nil
                      (find-image (cdar value))
                    (error nil)))
        (image-di (condition-case nil
                      (find-image (cddr value))
                    (error nil)))
        )
    (set enabled (propertize (if image-en " " (caar value))
                             'display image-en
                             'face 'tabbar-button-face
                             'local-map (symbol-value keymap)
                             'help-echo help))
    (set disabled (propertize (if image-di " " (cadr value))
                              'display image-di
                              'face 'tabbar-button-face))
    (custom-set-default variable value)
    ))

(defun tabbar-make-button-keymap (callback)
  "Return a button keymap that call CALLBACK on mouse events.
CALLBACK is passed the received mouse event."
  (let ((keymap (make-sparse-keymap)))
    ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line mouse-1] callback)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-2] callback)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-3] callback)
    keymap))

(defvar tabbar-home-button-enabled nil
  "Text of the enabled home button.")

(defvar tabbar-home-button-disabled nil
  "Text of the disabled home button.")

(defconst tabbar-home-button-keymap
  (tabbar-make-button-keymap 'tabbar-home-button-callback)
  "Keymap of the home button.")

(defun tabbar-home-button-callback (event)
  "Handle a mouse EVENT on the home button.
Call `tabbar-home-function'."
  (interactive "e")
  (when tabbar-home-function
    (funcall tabbar-home-function event)
    (force-mode-line-update)
    (sit-for 0)
    ))

(defun tabbar-home-button-help (window object position)
  "Return a help string or nil for none, for the home button.
Call `tabbar-home-help-function'.
Arguments WINDOW, OBJECT and POSITION, are not used."
  (when tabbar-home-help-function
    (funcall tabbar-home-help-function)))

(defconst tabbar-home-button-enabled-image
  '((:type pbm :ascent 80 :data "\
P2
10 10
255
184 184 184 184 0 184 184 184 184 184 184 184 184 0 0 0 184 184 184 184
184 184 0 0 0 0 0 184 184 184 184 0 0 0 0 0 0 0 184 184 184 184 255 0 0
0 255 255 255 184 184 0 0 0 0 0 0 0 184 184 184 184 0 0 0 0 0 255 255 184
184 184 184 0 0 0 255 255 184 184 184 184 184 184 0 255 255 184 184 184
184 184 184 184 184 255 184 184 184 184
"))
  "Default image for the enabled home button.")

(defconst tabbar-home-button-disabled-image
  '((:type pbm :ascent 80 :data "\
P2
10 10
255
184 184 184 184 120 184 184 184 184 184 184 184 184 120 120 120 184 184
184 184 184 184 120 184 184 184 120 184 184 184 184 120 120 160 184 160
120 120 184 184 184 184 255 120 184 120 255 255 255 184 184 120 120 160
184 160 120 120 184 184 184 184 120 184 184 184 120 255 255 184 184 184
184 120 120 120 255 255 184 184 184 184 184 184 120 255 255 184 184 184
184 184 184 184 184 255 184 184 184 184
"))
  "Default image for the disabled home button.")

(defcustom tabbar-home-button
  (cons (cons "[o]" tabbar-home-button-enabled-image)
        (cons "[x]" tabbar-home-button-disabled-image))
  "The home button."
  :group 'tabbar
  :type tabbar-button-widget
  :set 'tabbar-setup-button)

(defvar tabbar-scroll-left-button-enabled nil
  "Text of the enabled scroll left button.")

(defvar tabbar-scroll-left-button-disabled nil
  "Text of the disabled scroll left button.")

(defconst tabbar-scroll-left-button-keymap
  (tabbar-make-button-keymap 'tabbar-scroll-left-button-callback)
  "Keymap of the scroll left button.")

(defun tabbar-scroll-left-button-callback (event)
  "Handle a mouse EVENT on the scroll left button.
Call `tabbar-scroll-left-function'."
  (interactive "e")
  (when tabbar-scroll-left-function
    (funcall tabbar-scroll-left-function event)
    (force-mode-line-update)
    (sit-for 0)
    ))

(defun tabbar-scroll-left-button-help (window object position)
  "Return a help string or nil for none, for the scroll left button.
Call `tabbar-scroll-left-help-function'.
Arguments WINDOW, OBJECT and POSITION, are not used."
  (when tabbar-scroll-left-help-function
    (funcall tabbar-scroll-left-help-function)))

(defconst tabbar-scroll-left-button-enabled-image
  '((:type pbm :ascent 80 :data "\
P2
8 10
255
184 184 184 184 184 184 184 184 184 184 184 184 184 0 184 184 184 184 184
184 0 0 255 184 184 184 184 0 0 0 255 184 184 184 0 0 0 0 255 184 184 184
184 0 0 0 255 184 184 184 184 184 0 0 255 184 184 184 184 184 184 0 255
184 184 184 184 184 184 184 255 184 184 184 184 184 184 184 184 184
"))
  "Default image for the enabled scroll left button.")

(defconst tabbar-scroll-left-button-disabled-image
  '((:type pbm :ascent 80 :data "\
P2
8 10
255
184 184 184 184 184 184 184 184 184 184 184 184 184 120 184 184 184 184
184 184 120 120 255 184 184 184 184 120 184 120 255 184 184 184 120 184
184 120 255 184 184 184 184 120 184 120 255 184 184 184 184 184 120 120
255 184 184 184 184 184 184 120 255 184 184 184 184 184 184 184 255 184
184 184 184 184 184 184 184 184
"))
  "Default image for the disabled scroll left button.")

(defcustom tabbar-scroll-left-button
  (cons (cons " <" tabbar-scroll-left-button-enabled-image)
        (cons " =" tabbar-scroll-left-button-disabled-image))
  "The scroll left button."
  :group 'tabbar
  :type tabbar-button-widget
  :set 'tabbar-setup-button)

(defvar tabbar-scroll-right-button-enabled nil
  "Text of the enabled scroll right button.")

(defvar tabbar-scroll-right-button-disabled nil
  "Text of the disabled scroll right button.")

(defconst tabbar-scroll-right-button-keymap
  (tabbar-make-button-keymap 'tabbar-scroll-right-button-callback)
  "Keymap of the scroll right button.")

(defun tabbar-scroll-right-button-callback (event)
  "Handle a mouse EVENT on the scroll right button.
Call `tabbar-scroll-right-function'."
  (interactive "e")
  (when tabbar-scroll-right-function
    (funcall tabbar-scroll-right-function event)
    (force-mode-line-update)
    (sit-for 0)
    ))

(defun tabbar-scroll-right-button-help (window object position)
  "Return a help string or nil for none, for the scroll right button.
Call `tabbar-scroll-right-help-function'.
Arguments WINDOW, OBJECT and POSITION, are not used."
  (when tabbar-scroll-right-help-function
    (funcall tabbar-scroll-right-help-function)))

(defconst tabbar-scroll-right-button-enabled-image
  '((:type pbm :ascent 80 :data "\
P2
8 10
255
184 184 184 184 184 184 184 184 184 0 184 184 184 184 184 184 184 0 0 184
184 184 184 184 184 0 0 0 184 184 184 184 184 0 0 0 0 184 184 184 184 0
0 0 255 255 184 184 184 0 0 255 255 184 184 184 184 0 255 255 184 184 184
184 184 184 255 184 184 184 184 184 184 184 184 184 184 184 184 184
"))
  "Default image for the enabled scroll right button.")

(defconst tabbar-scroll-right-button-disabled-image
  '((:type pbm :ascent 80 :data "\
P2
8 10
255
184 184 184 184 184 184 184 184 184 120 184 184 184 184 184 184 184 120
120 184 184 184 184 184 184 120 184 120 184 184 184 184 184 120 184 184
120 184 184 184 184 120 184 120 255 255 184 184 184 120 120 255 255 184
184 184 184 120 255 255 184 184 184 184 184 184 255 184 184 184 184 184
184 184 184 184 184 184 184 184
"))
  "Default image for the disabled scroll right button.")

(defcustom tabbar-scroll-right-button
  (cons (cons " >" tabbar-scroll-right-button-enabled-image)
        (cons " =" tabbar-scroll-right-button-disabled-image))
  "The scroll right button."
  :group 'tabbar
  :type tabbar-button-widget
  :set 'tabbar-setup-button)

;;; Faces
;;
(defface tabbar-default-face
  '(
    (t
     (:inherit variable-pitch
               ;;:family "Verdana"
               :height 0.8
               :foreground "gray60"
               :background "gray72"
               )
     )
    )
  "Default face used in the tab bar."
  :group 'tabbar)

(defface tabbar-unselected-face
  '(
    (t
     (:inherit tabbar-default-face
               :box (:line-width 2 :color "white" :style pressed-button)
               )
     )
    )
  "Face used for uselected tabs."
  :group 'tabbar)

(defface tabbar-selected-face
  '(
    (t
     (:inherit tabbar-default-face
               :box (:line-width 2 :color "white" :style released-button)
               :foreground "blue"
               )
     )
    )
  "Face used for the selected tab."
  :group 'tabbar)

(defface tabbar-separator-face
  '(
    (t
     (:inherit tabbar-default-face
               :height 0.2
               )
     )
    )
  "Face used for the select mode button."
  :group 'tabbar)

(defface tabbar-button-face
  '(
    (t
     (:inherit tabbar-default-face
               :box (:line-width 2 :color "white" :style released-button)
               :foreground "dark red"
               )
     )
    )
  "Face used for the select mode button."
  :group 'tabbar)

;;; Wrappers
;;
(defun tabbar-scroll-left (event)
  "On mouse EVENT, scroll current tab set on left."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) 1)
    ))

(defun tabbar-scroll-left-help ()
  "Return the help string shown when mouse is onto the scroll left button."
  "mouse-1: scroll tabs left.")

(defun tabbar-scroll-right (event)
  "On mouse EVENT, scroll current tab set on right."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) -1)
    ))

(defun tabbar-scroll-right-help ()
  "Return the help string shown when mouse is onto the scroll right button."
  "mouse-1: scroll tabs right.")

(defun tabbar-make-select-tab-command (tab)
  "Return a command to handle TAB selection.
That command calls `tabbar-select-tab-function' with the received
mouse event and TAB."
  (let ((event (make-symbol "event")))
    `(lambda (,event)
       (interactive "e")
       (setq tabbar-last-selected-tab ',tab)
       (when tabbar-select-tab-function
         (select-window (posn-window (event-start ,event)))
         (funcall tabbar-select-tab-function ,event ',tab)
         (force-mode-line-update)
         (sit-for 0)))))

(defun tabbar-make-help-on-tab-function (tab)
  "Return a function that return a help string on TAB.
That command calls `tabbar-help-on-tab-function' with TAB."
  (let ((window (make-symbol "window"))
        (object (make-symbol "object"))
        (position (make-symbol "position"))
        )
    `(lambda (,window ,object ,position)
       (when tabbar-help-on-tab-function
         (funcall tabbar-help-on-tab-function ',tab)))))

(defun tabbar-line-element (tab)
  "Return an `header-line-format' template element from TAB.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ((keymap (make-sparse-keymap))
         (select (tabbar-make-select-tab-command tab))
         (help   (tabbar-make-help-on-tab-function tab))
         (label  (if tabbar-tab-label-function
                     (funcall tabbar-tab-label-function tab)
                   tab)))
    ;; Call `tabbar-select-tab-function' on mouse events.
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line mouse-1] select)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-2] select)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-3] select)
    ;; Return the tab followed by a separator.
    (list (propertize label 'local-map keymap 'help-echo help
                      'face (if (tabbar-selected-p
                                 tab(tabbar-current-tabset))
                                'tabbar-selected-face
                              'tabbar-unselected-face))
          tabbar-separator-value)))

(defconst tabbar-pad
  (propertize (make-string 1000 ?\ ) 'face 'tabbar-default-face))

(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Call `tabbar-current-tabset-function' to obtain the current tab set to
display.  Then call `tabbar-line-element' on each tab in current tab
set's view to build a list of template elements for
`header-line-format'."
  (let ((tabset (tabbar-current-tabset t)))
    (when tabset
      (list (format "%s%s%s"
                    (if tabbar-home-function
                        tabbar-home-button-enabled
                      tabbar-home-button-disabled)
                    (if (< (tabbar-start tabset)
                           (1- (length (tabbar-tabs tabset))))
                        tabbar-scroll-left-button-enabled
                      tabbar-scroll-left-button-disabled)
                    (if (> (tabbar-start tabset) 0)
                        tabbar-scroll-right-button-enabled
                      tabbar-scroll-right-button-disabled))
            tabbar-separator-value
            (or
             ;; If a cached template exists, use it.
             (tabbar-template tabset)
             ;; Otherwise use a refeshed value.
             (tabbar-set-template tabset
                                  (mapcar 'tabbar-line-element
                                          (tabbar-view tabset))))
            tabbar-pad))))

;;; Minor modes
;;
(defvar tabbar-old-global-hlf nil
  "Global value of the header line when entering tab bar mode.")

(defconst tabbar-header-line-format '(:eval (tabbar-line))
  "The tab bar header line format.")

;;;###autoload
(define-minor-mode tabbar-mode
  "Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'tabbar
  (unless (display-mouse-p)
    (message "Sorry, tab bar don't work without a mouse")
    (setq tabbar-mode nil))
  (if tabbar-mode
;;; ON
      (unless (eq header-line-format tabbar-header-line-format)
        ;; Save current default value of `header-line-format'.
        (setq tabbar-old-global-hlf (default-value 'header-line-format))
        (add-hook 'kill-buffer-hook 'tabbar-buffer-kill-buffer-hook)
        (tabbar-init-tabsets-store)
        (setq-default header-line-format tabbar-header-line-format))
;;; OFF
    ;; Restore previous `header-line-format', if it has not changed.
    (when (eq (default-value 'header-line-format)
              tabbar-header-line-format)
      (setq-default header-line-format tabbar-old-global-hlf))
    (remove-hook 'kill-buffer-hook 'tabbar-buffer-kill-buffer-hook)
    (tabbar-free-tabsets-store)
    ;; Turn off locals tab bar mode
    (mapc #'(lambda (b)
              (with-current-buffer b
                (tabbar-local-mode -1)))
          (buffer-list))
    ))

(defvar tabbar-old-local-hlf nil
  "Local value of the header line when entering tab bar local mode.")
(make-variable-buffer-local 'tabbar-old-local-hlf)

;;;###autoload
(define-minor-mode tabbar-local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar."
  :global nil
  :group 'tabbar
;;; ON
  (if tabbar-local-mode
      (if (and tabbar-mode (local-variable-p 'header-line-format)
               (not (local-variable-p 'tabbar-old-local-hlf)))
          (progn
            (setq tabbar-old-local-hlf header-line-format)
            (kill-local-variable 'header-line-format))
        (setq tabbar-local-mode nil))
;;; OFF
    (when (local-variable-p 'tabbar-old-local-hlf)
      (setq header-line-format tabbar-old-local-hlf)
      (kill-local-variable 'tabbar-old-local-hlf))
    ))

(defun tabbar-buffer-kill-buffer-hook ()
  "Hook run just before actually killing a buffer.
In tab bar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and tabbar-mode
       (eq tabbar-current-tabset-function 'tabbar-buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (tabbar-tab-values (tabbar-current-tabset)))
             (bn (buffer-name))
             found sibling)
         (while (and bl (not found))
           (if (equal bn (car bl))
               (setq found t)
             (setq sibling (car bl)))
           (setq bl (cdr bl)))
         (when (setq sibling (or (car bl) sibling))
           ;; Move sibling buffer in front of the buffer list.
           (save-current-buffer
             (switch-to-buffer sibling))))))

;;; Buffer tabs
;;
(defcustom tabbar-buffer-list-function
  'tabbar-buffer-list
  "*Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-buffer-groups-function
  'tabbar-buffer-groups
  "*Function that gives the group names a buffer belongs to.
That function is passed a buffer and must return a list of group
names, or nil if the buffer has no group.
Notice that it is better that a buffer belongs to one group."
  :group 'tabbar
  :type 'function)

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers that are not visiting a file and whose name starts
with a space."
  (delq t
        (mapcar #'(lambda (b)
                    (cond
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)))
                     (b)))
                (buffer-list))))

(defun tabbar-buffer-groups (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond
     ((or (get-buffer-process (current-buffer))
          (memq major-mode
                '(comint-mode compilation-mode)))
      '("Process")
      )
     ((member (buffer-name)
              '("*scratch*" "*Messages*"))
      '("Common")
      )
     ((eq major-mode 'dired-mode)
      '("Dired")
      )
     ((memq major-mode
            '(help-mode apropos-mode Info-mode Man-mode))
      '("Help")
      )
     ((memq major-mode
            '(rmail-mode
              rmail-edit-mode vm-summary-mode vm-mode mail-mode
              mh-letter-mode mh-show-mode mh-folder-mode
              gnus-summary-mode message-mode gnus-group-mode
              gnus-article-mode score-mode gnus-browse-killed-mode))
      '("Mail")
      )
     (t
      (list
       (if (and (stringp mode-name) (string-match "[^ ]" mode-name))
           mode-name
         (symbol-name major-mode)))
      )
     )))

;;; Group buffers in tab sets.
;;
(defun tabbar-buffer-cleanup-tabsets (buffers)
  "Remove obsolete tabs from existing tab sets.
That is tabs whose value is a killed buffer or a buffer not in
BUFFERS.  Delete tab sets that no more contain tabs."
  (mapc 'tabbar-delete-tabset
        (tabbar-map-tabsets
         #'(lambda (tabset)
             (mapc #'(lambda (tab)
                       (let ((b (get-buffer (tabbar-tab-value tab))))
                         (unless (and b (memq b buffers))
                           (tabbar-delete-tab tab))))
                   (tabbar-tabs tabset))
             (unless (tabbar-tabs tabset)
               tabset)))))

(defun tabbar-buffer-update-groups ()
  "Update group of buffers.
Return the the first group where the current buffer is."
  ;; Ensure that the current buffer will always have a tab!
  (let ((buffers (cons (current-buffer)
                       (funcall tabbar-buffer-list-function)))
        current-group)
    (mapc
     #'(lambda (buffer)
         (let* ((name (buffer-name buffer))
                (groups (funcall tabbar-buffer-groups-function name)))
           (when (eq buffer (current-buffer))
             (setq current-group (car groups)))
           (mapc #'(lambda (group)
                     (let ((tabset (tabbar-get-tabset group)))
                       (if tabset
                           (tabbar-add-tab tabset name t)
                         (tabbar-make-tabset group name))))
                 groups)))
     buffers)
    (tabbar-buffer-cleanup-tabsets buffers)
    current-group))

;;; Tab bar callbacks
;;
(defvar tabbar-buffer-group-mode nil
  "Display tabs for group of buffers, when non-nil.")

(defun tabbar-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((group (tabbar-buffer-update-groups))
        (buffer (buffer-name))
        tabset curtab)
    (if tabbar-buffer-group-mode
        (progn
          (setq tabset (tabbar-get-tabsets-tabset)
                curtab (tabbar-selected-tab (tabbar-current-tabset)))
          (unless (and (equal buffer (tabbar-tab-value curtab))
                       (tabbar-select-tab curtab tabset))
            (tabbar-select-tab-value buffer tabset)))
      (setq tabset (tabbar-tab-tabset tabbar-last-selected-tab))
      (unless (and tabset (tabbar-get-tab buffer tabset))
        (setq tabset (tabbar-get-tabset group)))
      (tabbar-select-tab-value buffer tabset))
    tabset))

(defun tabbar-buffer-tab-label (tab)
  "Return the label to display TAB.
Must be a valid `header-line-format' template element."
  (if tabbar-buffer-group-mode
      (format "[%s]" (tabbar-tab-tabset tab))
    (format " %s " (tabbar-tab-value tab))))

(defun tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar-buffer-group-mode
      "mouse-1: switch to selected tab in group"
    "\
mouse-1: switch to buffer, \
mouse-2: pop to buffer, \
mouse-3: delete other windows"
    ))

(defun tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-1)
      (switch-to-buffer buffer))
     ((eq mouse-button 'mouse-2)
      (pop-to-buffer buffer t))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows)))
    ;; Disable group mode.
    (setq tabbar-buffer-group-mode nil)
    ))

(defun tabbar-buffer-toggle-group-mode (event)
  "On mouse EVENT, toggle group mode.
When enabled, display tabs for group of buffers, instead of buffer
tabs."
  (setq tabbar-buffer-group-mode (not tabbar-buffer-group-mode)))

(defun tabbar-buffer-toggle-group-mode-help ()
  "Return the help string shown when mouse is onto the toggle button."
  (if tabbar-buffer-group-mode
      "mouse-1: show buffers in selected group"
    "mouse-1: show groups of buffers"
    ))

(provide 'tabbar)

;;; tabbar.el ends here
