;;; tabbar.el --- Display a tab bar in the header line

;; Copyright (C) 2003, 2004 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 25 February 2003
;; Keywords: convenience
;; Revision: $Id: tabbar.el,v 1.44 2004/10/07 10:25:47 ponced Exp $

(defconst tabbar-version "1.4")

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
;; line.  It works only on GNU Emacs 21.
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
;; It is possible to navigate through tabs using commands (that is,
;; using the keyboard).  The main commands to cycle through tabs are:
;;
;; - `tabbar-forward' select the next available tab.
;; - `tabbar-backward' select the previous available tab.
;;
;; It is worth defining keys for them.  For example:
;;
;;   (global-set-key [(control shift tab)] 'tabbar-backward)
;;   (global-set-key [(control tab)]       'tabbar-forward)
;;
;; The default cycle is to first try to select the tab just
;; after/before the selected tab.  If this is the last/first tab, then
;; the first/last tab of the next/previous group of tabs is selected.
;; That behavior is controlled by the `tabbar-cycling-scope' option.
;;
;; The following specialized commands can be useful too:
;;
;; - `tabbar-forward-tab'/`tabbar-backward-tab'
;;      Navigate through visible tabs only.
;;
;; - `tabbar-forward-group'/`tabbar-backward-group'
;;      Navigate through tab groups only.
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
;;
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
;; default function: `tabbar-buffer-list', excludes buffers whose name
;; starts with a space, when they are not visiting a file.
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
;;
;; Known problems:
;;
;; Bug item #858306 at <http://sf.net/tracker/?group_id=79309>:
;; tabbar-mode crashes GNU Emacs 21.3 on MS-Windows 98/95.
;;

;;; History:
;;

;;; Code:

;;; Options
;;
(defgroup tabbar nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom tabbar-cycling-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'tabbar
  :type '(choice :tag "Cycle through..."
                 (const :tag "Visible Tabs Only" tabs)
                 (const :tag "Tab Groups Only" groups)
                 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom tabbar-selected-tab-always-visible t
  "*non-nil means to keep the selected tab visible."
  :group 'tabbar
  :type 'boolean)

(defcustom tabbar-inhibit-functions
  '(tabbar-default-inhibit-function)
  "List of functions to be called before displaying the tab bar.
Those functions are called one by one, with no arguments, until one of
them returns a non-nil value, and thus, prevent to display the tab
bar."
  :group 'tabbar
  :type 'hook)

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

(defcustom tabbar-button-label-function
  'tabbar-buffer-button-label
  "Function that obtains a button label displayed on the tab bar.
The function is passed a button name should return a propertized
string to display."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-home-function
  'tabbar-buffer-click-on-home
  "Function called when clicking on the tab bar home button.
The function is passed the mouse event received."
  :group 'tabbar
  :type 'function)

(defcustom tabbar-home-help-function
  'tabbar-buffer-help-on-home
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

;;; Misc.
;;
(eval-and-compile
  (defalias 'tabbar-display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update))
  )

(defsubst tabbar-click-p (event)
  "Return non-nil if EVENT is a mouse click event."
  (memq 'click (event-modifiers event)))

(defun tabbar-shorten (str width)
  "Return a shortened string from STR that fits in the given display WIDTH.
WIDTH is specified in terms of character display width in the current
buffer; see also `char-width'.  If STR display width is greater than
WIDTH, STR is truncated and an ellipsis string \"...\" is inserted at
end or in the middle of the returned string, depending on available
room."
  (let* ((n  (length str))
         (sw (string-width str))
         (el "...")
         (ew (string-width el))
         (w  0)
         (i  0))
    (cond
     ;; STR fit in WIDTH, return it.
     ((<= sw width)
      str)
     ;; There isn't enough room for the ellipsis, STR is just
     ;; truncated to fit in WIDTH.
     ((<= width ew)
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (substring str 0 i))
     ;; There isn't enough room to insert the ellipsis in the middle
     ;; of the truncated string, so put the ellipsis at end.
     ((zerop (setq sw (/ (- width ew) 2)))
      (setq width (- width ew))
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (concat (substring str 0 i) el))
     ;; Put the ellipsis in the middle of the truncated string.
     (t
      (while (< w sw)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (setq w (+ w ew))
      (while (< w width)
        (setq n (1- n)
              w (+ w (char-width (aref str n)))))
      (concat (substring str 0 i) el (substring str n)))
     )))

;;; Tab and tab set
;;
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
(make-variable-buffer-local 'tabbar-current-tabset)

(defvar tabbar-last-selected-tab nil
  "The last selected tab.")

(defsubst tabbar-free-tabsets-store ()
  "Free the tab set store."
  (setq tabbar-tabsets nil
        tabbar-current-tabset nil
        tabbar-last-selected-tab nil))

(defsubst tabbar-init-tabsets-store ()
  "Initialize the tab set store."
  (tabbar-free-tabsets-store)
  (setq tabbar-tabsets (make-vector 31 0)))

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

(defsubst tabbar-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst tabbar-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (tabbar-tab-value (tabbar-selected-tab tabset)))

(defsubst tabbar-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (tabbar-selected-tab tabset)))

(defvar tabbar-show-selected nil)

(defsubst tabbar-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (tabbar-member tab tabset)
    (unless (tabbar-selected-p tab tabset)
      (tabbar-set-template tabset nil)
      (setq tabbar-show-selected tabbar-selected-tab-always-visible))
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

(defun tabbar-delete-tab (tab)
  "Remove TAB from its TABSET."
  (let* ((tabset (tabbar-tab-tabset tab))
         (tabs   (tabbar-tabs tabset)))
    (tabbar-set-template tabset nil)
    (when (eq tab (tabbar-selected-tab tabset))
      (tabbar-select-tab (car (or (cdr (memq tab tabs)) (last tabs)))
                         tabset))
    (set tabset (delq tab tabs))))

(defun tabbar-scroll (tabset count)
  "Scroll the TABSET's view of COUNT tabs.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (tabbar-start tabset) count))
                    (1- (length (tabbar-tabs tabset))))))
    (when (/= start (tabbar-start tabset))
      (tabbar-set-template tabset nil)
      (put tabset 'start start))))

(defun tabbar-tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* (last (tabs (tabbar-tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
            tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

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

;;; Faces
;;
(defface tabbar-default
  '(
    ;;(((class color grayscale) (background light))
    ;; :inherit variable-pitch
    ;; :height 0.8
    ;; :foreground "gray50"
    ;; :background "grey75"
    ;; )
    (((class color grayscale) (background dark))
     :inherit variable-pitch
     :height 0.8
     :foreground "grey75"
     :background "gray50"
     )
    (((class mono) (background light))
     :inherit variable-pitch
     :height 0.8
     :foreground "black"
     :background "white"
     )
    (((class mono) (background dark))
     :inherit variable-pitch
     :height 0.8
     :foreground "white"
     :background "black"
     )
    (t
     :inherit variable-pitch
     :height 0.8
     :foreground "gray50"
     :background "gray75"
     ))
  "Default face used in the tab bar."
  :group 'tabbar)

(defface tabbar-unselected
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-selected
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style pressed-button)
     :foreground "blue"
     ))
  "Face used for the selected tab."
  :group 'tabbar)

(defface tabbar-separator
  '((t
     :inherit tabbar-default
     :height 0.1
     ))
  "Face used for the select mode button."
  :group 'tabbar)

(defface tabbar-button
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     :foreground "dark red"
     ))
  "Face used for the select mode button."
  :group 'tabbar)

(defcustom tabbar-background-color nil
  "*Background color of the tab bar.
By default, use the background color specified for the
`tabbar-default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'tabbar
  :type '(choice (const :tag "Default" nil)
                 (color)))

(defsubst tabbar-background-color ()
  "Return the background color of the tab bar."
  (or tabbar-background-color
      (let* ((face 'tabbar-default)
             (color (face-background face)))
        (while (null color)
          (or (facep (setq face (face-attribute face :inherit)))
              (setq face 'default))
          (setq color (face-background face)))
        color)))

;;; Buttons and separator look and feel
;;
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
  "Widget for editing a tab bar button.
A button is specified as a pair (ENABLED-BUTTON . DISABLED-BUTTON),
where ENABLED-BUTTON and DISABLED-BUTTON specify the value used when
the button is respectively enabled and disabled.  Each button value is
a pair (STRING . IMAGE) where STRING is a string value, and IMAGE a
list of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING.
If only the ENABLED-BUTTON image is provided, a DISABLED-BUTTON image
is derived from it.")

;;; Home button
;;
(defvar tabbar-home-button-value nil
  "Value of the home button.")

(defconst tabbar-home-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0
6 0 255 255 255 255 255 255 255 255 255 255 9 130 9 255 255 255 255
255 255 255 255 255 255 26 130 26 255 255 255 255 255 255 255 0 9 26
41 130 41 26 9 0 255 255 255 255 5 145 140 135 130 125 120 115 5 255
255 255 255 0 9 26 41 130 41 26 9 0 255 255 255 255 255 255 255 26 130
26 255 255 255 255 255 255 255 255 255 255 9 130 9 255 255 255 255 255
255 255 255 255 255 0 6 0 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255
"))
  "Default image for the enabled home button.")

(defconst tabbar-home-button-disabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 0 0 1 2 3 2 1 0 0 255 255 255 255 0 132 128 123 119 114 110
106 0 255 255 255 255 0 0 1 2 3 2 1 0 0 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 255
"))
  "Default image for the disabled home button.")

(defcustom tabbar-home-button
  (cons (cons "[o]" tabbar-home-button-enabled-image)
        (cons "[x]" tabbar-home-button-disabled-image))
  "The home button.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-home-button-value nil)))

;;; Scroll left button
;;
(defvar tabbar-scroll-left-button-value nil
  "Value of the scroll left button.")

(defconst tabbar-scroll-left-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255 128 16 48 255 255 255 255 255 255 255
255 144 28 86 128 0 255 255 255 255 255 255 160 44 92 159 135 113 0
255 255 255 255 160 44 97 165 144 129 120 117 0 255 255 176 44 98 175
174 146 127 126 127 128 0 255 255 0 160 184 156 143 136 134 135 137
138 0 255 255 176 32 67 144 146 144 145 146 148 149 0 255 255 255 255
160 42 75 140 154 158 159 160 0 255 255 255 255 255 255 160 40 74 154
170 171 0 255 255 255 255 255 255 255 255 160 41 82 163 0 255 255 255
255 255 255 255 255 255 255 160 32 48 255 255 255 255 255 255 255 255
255 255 255 255 255 255
"))
  "Default image for the enabled scroll left button.
A disabled button image will be automatically build from it.")

(defcustom tabbar-scroll-left-button
  (cons (cons " <" tabbar-scroll-left-button-enabled-image)
        (cons " =" nil))
  "The scroll left button.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-scroll-left-button-value nil)))

;;; Scroll right button
;;
(defvar tabbar-scroll-right-button-value nil
  "Value of the scroll right button.")

(defconst tabbar-scroll-right-button-enabled-image
  '((:type pbm :data "\
P2 13 13 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
48 32 160 255 255 255 255 255 255 255 255 255 255 44 161 71 32 160 255
255 255 255 255 255 255 255 36 157 163 145 62 32 160 255 255 255 255
255 255 30 128 133 137 142 124 50 32 160 255 255 255 255 29 120 121
124 126 126 124 105 42 32 176 255 255 31 126 127 128 128 128 128 126
124 89 32 255 255 33 134 135 136 137 137 138 119 49 32 176 255 255 34
143 144 145 146 128 54 32 160 255 255 255 255 36 152 153 134 57 32 160
255 255 255 255 255 255 38 141 60 32 160 255 255 255 255 255 255 255
255 48 32 160 255 255 255 255 255 255 255 255 255 255 255 255 255 255
255 255 255 255 255 255 255 255
"))
  "Default image for the enabled scroll right button.
A disabled button image will be automatically build from it.")

(defcustom tabbar-scroll-right-button
  (cons (cons " >" tabbar-scroll-right-button-enabled-image)
        (cons " =" nil))
  "The scroll right button.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-scroll-right-button-value nil)))

;;; Separator
;;
(defconst tabbar-separator-widget
  '(cons (choice (string)
                 (number :tag "Space width" 0.2))
         (repeat :tag "Image"
                 :extra-offset 2
                 (restricted-sexp :tag "Spec"
                                  :match-alternatives (listp))))
  "Widget for editing a tab bar separator.
A separator is specified as a pair (STRING-OR-WIDTH . IMAGE) where
STRING-OR-WIDTH is a string value or a space width, and IMAGE a list
of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING-OR-WIDTH.
The value (\"\"), or (0) hide separators.")

(defvar tabbar-separator-value nil
  "Value of the separator used between tabs.")

(defcustom tabbar-separator (list 0.2)
  "Separator used between tabs.
The variable `tabbar-separator-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-separator-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of separator value.
          (setq tabbar-separator-value nil)))

;;; Images
;;
(defcustom tabbar-use-images t
  "*non-nil means to try to use images in tab bar.
That is for buttons and separators."
  :group 'tabbar
  :type 'boolean
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of all buttons and separator values.
          (setq tabbar-separator-value nil
                tabbar-home-button-value nil
                tabbar-scroll-left-button-value nil
                tabbar-scroll-right-button-value nil)))

(defsubst tabbar-find-image (specs)
  "Find an image, choosing one of a list of image specifications.
SPECS is a list of image specifications.  See also `find-image'."
  (when (and tabbar-use-images (display-images-p))
    (condition-case nil
        (find-image specs)
      (error nil))))

(defsubst tabbar-disable-image (image)
  "Make IMAGE look disabled."
  (setcdr image (plist-put (cdr image) :conversion 'disabled))
  image)

(defsubst tabbar-normalize-image (image &optional margin)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image."
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (setq plist (plist-put plist :mask '(heuristic t))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

;;; Button keymaps and callbacks
;;
(defun tabbar-make-mouse-keymap (callback)
  "Return a keymap that call CALLBACK on mouse events.
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

(defsubst tabbar-make-mouse-event (&optional type)
  "Return a mouse click event.
Optional argument TYPE is a mouse-click event or one of the
symbols `mouse-1', `mouse-2' or `mouse-3'.
The default is `mouse-1'."
  (if (tabbar-click-p type)
      type
    (list (or (memq type '(mouse-2 mouse-3)) 'mouse-1)
          (or (event-start nil) ;; Emacs 21.4
              (list (selected-window) (point) '(0 . 0) 0)))))

;;; Buttons
;;
(defconst tabbar-default-button-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-button-callback)
  "Default keymap of a button.")

(defun tabbar-help-on-button (window object position)
  "Return a help string or nil for none, for the button under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the button label under the mouse.
POSITION is the position in that label.
Call `tabbar-NAME-help-function' where NAME is the button name
associated to OBJECT."
  (let* ((name (get-text-property position 'tabbar-button object))
         (funvar (and name
                      (intern-soft (format "tabbar-%s-help-function"
                                           name)))))
    (and (symbol-value funvar)
         (funcall (symbol-value funvar)))))

(defsubst tabbar-click-on-button (name &optional type)
  "Handle a mouse click event on button NAME.
Call `tabbar-select-NAME-function' with the received, or simulated
mouse click event.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let ((funvar (intern-soft (format "tabbar-%s-function" name))))
    (when (symbol-value funvar)
      (funcall (symbol-value funvar) (tabbar-make-mouse-event type))
      (tabbar-display-update))))

(defun tabbar-select-button-callback (event)
  "Handle a mouse EVENT on a button.
Pass mouse click events on a button to `tabbar-click-on-button'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-button
       (get-text-property (cdr target) 'tabbar-button (car target))
       event))))

(defun tabbar-make-button-keymap (name)
  "Return a keymap to handle mouse click events on button NAME."
  (if (fboundp 'posn-string)
      tabbar-default-button-keymap
    (let ((event (make-symbol "event")))
      (tabbar-make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tabbar-click-p ,event)
               (tabbar-click-on-button ',name ,event)))))))

;;; Button callbacks
;;
(defun tabbar-scroll-left (event)
  "On mouse EVENT, scroll current tab set on left."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) -1)))

(defun tabbar-scroll-left-help ()
  "Help string shown when mouse is over the scroll left button."
  "mouse-1: scroll tabs left.")

(defun tabbar-scroll-right (event)
  "On mouse EVENT, scroll current tab set on right."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) 1)))

(defun tabbar-scroll-right-help ()
  "Help string shown when mouse is over the scroll right button."
  "mouse-1: scroll tabs right.")

;;; Tabs
;;
(defconst tabbar-default-tab-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-tab-callback)
  "Default keymap of a tab.")

(defun tabbar-help-on-tab (window object position)
  "Return a help string or nil for none, for the tab under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the tab label under the mouse.
POSITION is the position in that label.
Call `tabbar-help-on-tab-function' with the associated tab."
  (when tabbar-help-on-tab-function
    (let ((tab (get-text-property position 'tabbar-tab object)))
      (funcall tabbar-help-on-tab-function tab))))

(defsubst tabbar-click-on-tab (tab &optional type)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (when tabbar-select-tab-function
    (setq tabbar-last-selected-tab tab)
    (funcall tabbar-select-tab-function
             (tabbar-make-mouse-event type) tab)))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-tab
       (get-text-property (cdr target) 'tabbar-tab (car target))
       event))))

(defun tabbar-make-tab-keymap (tab)
  "Return a keymap to handle mouse click events on TAB."
  (if (fboundp 'posn-string)
      tabbar-default-tab-keymap
    (let ((event (make-symbol "event")))
      (tabbar-make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tabbar-click-p ,event)
               (tabbar-click-on-tab ',tab ,event)))))))

;;; Tab bar construction
;;
(defun tabbar-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearence of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-NAME-button'."
  (let* ((btn (symbol-value
               (intern-soft (format "tabbar-%s-button" name))))
         (on  (tabbar-find-image (cdar btn)))
         (off (and on (tabbar-find-image (cddr btn)))))
    (when on
      (tabbar-normalize-image on 1)
      (if off
          (tabbar-normalize-image off 1)
        ;; If there is no disabled button image, derive one from the
        ;; button enabled image.
        (setq off (copy-sequence on))
        (tabbar-disable-image off)))
    (cons
     (propertize (or (caar btn) " ") 'display on)
     (propertize (or (cadr btn) " ") 'display off))))

(defun tabbar-line-button (name)
  "Return the display representation of button NAME.
That is, a propertized string used as an `header-line-format' template
element."
  (let ((label (funcall tabbar-button-label-function name)))
    ;; Cache the display value of the enabled/disabled buttons in
    ;; variables `tabbar-NAME-button-value'.
    (set (intern (format "tabbar-%s-button-value"  name))
         (cons
          (propertize (car label)
                      'tabbar-button name
                      'face 'tabbar-button
                      'pointer 'hand
                      'local-map (tabbar-make-button-keymap name)
                      'help-echo 'tabbar-help-on-button)
          (propertize (cdr label)
                      'face 'tabbar-button
                      'pointer 'arrow)))))

(defun tabbar-line-separator ()
  "Return the display representation of a tab bar separator.
That is, a propertized string used as an `header-line-format' template
element."
  (let ((image (tabbar-find-image (cdr tabbar-separator))))
    ;; Cache the separator display value in variable
    ;; `tabbar-separator-value'.
    (setq tabbar-separator-value
          (cond
           (image
            (propertize " "
                        'face 'tabbar-separator
                        'pointer 'arrow
                        'display (tabbar-normalize-image image)))
           ((numberp (car tabbar-separator))
            (propertize " "
                        'face 'tabbar-separator
                        'pointer 'arrow
                        'display (list 'space
                                       :width (car tabbar-separator))))
           ((propertize (or (car tabbar-separator) " ")
                        'face 'tabbar-separator
                        'pointer 'arrow))))
    ))

(defsubst tabbar-line-buttons (tabset)
  "Return a propertized string for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (concat
   (if tabbar-home-function
       (car tabbar-home-button-value)
     (cdr tabbar-home-button-value))
   (if (> (tabbar-start tabset) 0)
       (car tabbar-scroll-left-button-value)
     (cdr tabbar-scroll-left-button-value))
   (if (< (tabbar-start tabset)
          (1- (length (tabbar-tabs tabset))))
       (car tabbar-scroll-right-button-value)
     (cdr tabbar-scroll-right-button-value))
   tabbar-separator-value))

(defsubst tabbar-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (concat (propertize
           (if tabbar-tab-label-function
               (funcall tabbar-tab-label-function tab)
             tab)
           'tabbar-tab tab
           'local-map (tabbar-make-tab-keymap tab)
           'help-echo 'tabbar-help-on-tab
           'face (if (tabbar-selected-p tab (tabbar-current-tabset))
                     'tabbar-selected
                   'tabbar-unselected)
           'pointer 'hand)
          tabbar-separator-value))

(defun tabbar-truncated-p (buttons tabs)
  "Return non-nil if the tab bar has a good chance to be truncated.
That is, if the tab bar extends beyond the right edge of the window
when it displays BUTTONS and TABS.
BUTTON is a propertized string and TABS is a list propertized string
to display."
  (with-temp-buffer
    (let ((truncate-partial-width-windows nil))
      (setq truncate-lines nil)
      (apply 'insert buttons tabs)
      (goto-char (point-min))
      (> (vertical-motion (buffer-size)) 0))))

(defun tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  ;; If a cached value of the `header-line-format' exists use it.
  (or (tabbar-template tabset)
      ;; Otherwise recompute a new `header-line-format'.
      (let* ((sel (tabbar-selected-tab tabset))
             (tabs (tabbar-view tabset))
             (padcolor (tabbar-background-color))
             atsel elts)
        ;; Initialize buttons and separator values.
        (or tabbar-separator-value
            (tabbar-line-separator))
        (or tabbar-home-button-value
            (tabbar-line-button 'home))
        (or tabbar-scroll-left-button-value
            (tabbar-line-button 'scroll-left))
        (or tabbar-scroll-right-button-value
            (tabbar-line-button 'scroll-right))
        ;; Track the selected tab to ensure it is always visible.
        (when tabbar-show-selected
          (while (not (memq sel tabs))
            (tabbar-scroll tabset -1)
            (setq tabs (tabbar-view tabset)))
          (while (and tabs (not atsel))
            (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
                  atsel (eq (car tabs) sel)
                  tabs  (cdr tabs)))
          (setq elts (nreverse elts))
          ;; At this point the selected tab is the last elt in ELTS.
          (while (and (cdr elts) ;; Always show the selected tab!
                      (tabbar-truncated-p
                       (tabbar-line-buttons tabset) elts))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))
          (setq elts (nreverse elts))
          (setq tabbar-show-selected nil))
        ;; Format remaining tabs.
        (while tabs
          (setq elts (cons (tabbar-line-tab (car tabs)) elts)
                tabs (cdr tabs)))
        ;; Cache and return the new tab bar.
        (tabbar-set-template
         tabset
         (list (tabbar-line-buttons tabset)
               (nreverse elts)
               (propertize "%-"
                           'face (list :background padcolor
                                       :foreground padcolor)
                           'pointer 'arrow)))
        )))

(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window if any of the
`tabbar-inhibit-functions' return non-nil."
  (if (run-hook-with-args-until-success 'tabbar-inhibit-functions)
      (setq header-line-format nil)
    (let ((tabset (tabbar-current-tabset t)))
      (and tabset (tabbar-line-format tabset)))))

;;; Cyclic navigation through tabs
;;
(defun tabbar-cycle (&optional backward type)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `tabbar-cycling-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead.
Optional argument TYPE is a mouse event type (see the function
`tabbar-make-mouse-event' for details)."
  (let ((tabset (tabbar-current-tabset t))
        selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq tabbar-cycling-scope 'tabs)
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (tabbar-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq tabbar-cycling-scope 'groups)
        (setq tabset (tabbar-get-tabsets-tabset)
              tab (tabbar-tab-next tabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (tabbar-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tabset (tabbar-get-tabsets-tabset)
                tab (tabbar-tab-next tabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (tabbar-tabs tabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (tabbar-tabs (tabbar-tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (tabbar-click-on-tab tab type))))

;;;###autoload
(defun tabbar-backward ()
  "Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'."
  (interactive)
  (tabbar-cycle t))

;;;###autoload
(defun tabbar-forward ()
  "Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'."
  (interactive)
  (tabbar-cycle))

;;;###autoload
(defun tabbar-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((tabbar-cycling-scope 'groups))
    (tabbar-cycle t)))

;;;###autoload
(defun tabbar-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((tabbar-cycling-scope 'groups))
    (tabbar-cycle)))

;;;###autoload
(defun tabbar-backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((tabbar-cycling-scope 'tabs))
    (tabbar-cycle t)))

;;;###autoload
(defun tabbar-forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((tabbar-cycling-scope 'tabs))
    (tabbar-cycle)))

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

;;; Hooks
;;
(defun tabbar-default-inhibit-function ()
  "Inhibit display of the tab bar in specified windows.
That is dedicated windows, and `checkdoc' status windows."
  (or (window-dedicated-p (selected-window))
      (member (buffer-name)
              (list " *Checkdoc Status*"
                    (if (boundp 'ispell-choices-buffer)
                        ispell-choices-buffer
                      "*Choices*")))))

(defun tabbar-buffer-kill-buffer-hook ()
  "Hook run just before actually killing a buffer.
In tab bar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq header-line-format tabbar-header-line-format)
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
         (when (and (setq sibling (or (car bl) sibling))
                    (get-buffer sibling))
           ;; Move sibling buffer in front of the buffer list.
           (save-current-buffer
             (switch-to-buffer sibling))))))

;;; Buffer tabs
;;
(defcustom tabbar-buffer-home-button
  (cons (cons "[-]" tabbar-home-button-disabled-image)
        (cons "[+]" tabbar-home-button-enabled-image))
  "The home button displayed when showing buffer tabs.
The enabled button value is displayed when showing tabs for groups of
buffers, and the disabled button value is disabled when showing buffer
tabs.
Images default to the enabled image specified in `tabbar-home-button'.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-home-button-value nil)))

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
Exclude buffers whose name starts with a space, when they are not
visiting a file."
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
(make-variable-buffer-local 'tabbar-buffer-group-mode)

(defsubst tabbar-buffer-set-group-mode (flag)
  "Set display of tabs for group of buffers to FLAG."
  (setq tabbar-buffer-group-mode flag
        ;; Redisplay the home button.
        tabbar-home-button-value nil))

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

(defun tabbar-buffer-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearence of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-button-label'.
When NAME is 'home, return a different ENABLED button depending on the
value of `tabbar-buffer-group-mode'.  Otherwise just call the function
`tabbar-button-label'."
  (let ((lab (tabbar-button-label name)))
    (when (eq name 'home)
      (let* ((btn tabbar-buffer-home-button)
             (on  (tabbar-find-image (cdar btn)))
             (off (tabbar-find-image (cddr btn))))
        ;; When `tabbar-buffer-home-button' does not provide a value,
        ;; default to the enabled value of `tabbar-home-button'.
        (if on
            (tabbar-normalize-image on 1)
          (setq on (get-text-property 0 'display (car lab))))
        (if off
            (tabbar-normalize-image off 1)
          (setq off (get-text-property 0 'display (car lab))))
      (setcar lab
              (if tabbar-buffer-group-mode
                  (propertize (or (caar btn) (car lab)) 'display on)
                (propertize (or (cadr btn) (car lab)) 'display off)))
      ))
    lab))

(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar-buffer-group-mode
                    (format "[%s]" (tabbar-tab-tabset tab))
                  (format "%s" (tabbar-tab-value tab)))))
    ;; Unless tracking the selected tab, wich auto-hscroll the tab
    ;; bar, shorten the tab label to try to keep all tabs in the
    ;; visible area of the tab bar.
    (if tabbar-show-selected
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar-buffer-group-mode
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (tabbar-tab-value tab) tabset))
    (format "mouse-1: switch to buffer %S\n\
mouse-2: pop to buffer, mouse-3: delete other windows"
            (tabbar-tab-value tab))
    ))

(defun tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (pop-to-buffer buffer t))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Disable group mode.
    (tabbar-buffer-set-group-mode nil)
    ))

(defun tabbar-buffer-click-on-home (event)
  "Handle a mouse click EVENT on the tab bar home button.
mouse-1, toggle the display of tabs for group of buffers.
mouse-3, close the current buffer."
  (let ((mouse-button (event-basic-type event)))
    (cond
     ((eq mouse-button 'mouse-1)
      (tabbar-buffer-set-group-mode (not tabbar-buffer-group-mode)))
     ((eq mouse-button 'mouse-3)
      (kill-buffer nil))
     )))

(defun tabbar-buffer-help-on-home ()
  "Return the help string shown when mouse is onto the toggle button."
  (concat
   (if tabbar-buffer-group-mode
       "mouse-1: show buffers in selected group"
     "mouse-1: show groups of buffers")
   ", mouse-3: close current buffer"))

(provide 'tabbar)

;;; tabbar.el ends here
