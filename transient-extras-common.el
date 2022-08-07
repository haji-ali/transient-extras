;; transient-extras-common.el --- Extra features for transient -*- lexical-binding: t -*-

;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>, Samuel W. Flint <swflint@flintfam.org>
;; URL: https://github.com/haji-ali/lp-transient
;; Version: 0.0.1
;; Package-Requires ((emacs "28.0"))
;; Keywords: transient, gui

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; This package provides a number of additional transient infixes and switches.
;;
;; In particular, the following are defined:
;;
;;  - `transient-extras-file-list-or-buffer' a defined argument that
;;    can be used in a transient.  It contains either the current
;;    buffer, the name of the current file, or the names of the files
;;    currently marked in `dired'.
;;
;;  - The class `transient-extras-exclusive-switch', which allows for
;;    command line switches with defined options to be cycled through.
;;    This is similar to `transient-switches', but the `choices' slot
;;    is a list of cons cells, `(value . label)', with label used for
;;    display.  Ex:
;;      (transient-define-argument lp-transient--orientation ()
;;        :description "Print Orientation"
;;        :class 'transient-extras-exclusive-switch
;;        :key "o"
;;        :argument-format "-oorientation-requested=%s"
;;        :argument-regexp "\\(-oorientation-requested=\\(4\\|5\\|6\\)\\)"
;;        :choices '(("4" . "90°(landscape)")
;;                   ("5" . "-90°")
;;                   ("6" . "180°")))
;;
;; - The class `transient-extras-options-from-command', based off of
;;   `transient-switch'.  This allows completion of options which are
;;   determined by an external command.  It requires `:command-line',
;;   `:filter-function' and `:prompt' slots be filled in addition to
;;   the normal `transient-switch' slots.  A `:cachep' slot may also
;;   be set to `t' to allow the possible options to be cached.  Ex:
;;      (transient-define-argument lp-transient--printer ()
;;        :class 'transient-extras-options-from-command
;;        :description "Printer"
;;        :key "d"
;;        :argument "-d"
;;        :command-line '("lpstat" "-a")
;;        :cachep t
;;        :filter-function (lambda (line)
;;                           (let ((index (string-match "[[:space:]]" line)))
;;                             (if index
;;                                 (substring line nil index)
;;                               line)))
;;        :prompt "Printer? ")

(require 'transient)
(require 'cl)

;;; Code:


;;; Files Lists

(defun transient-extras--get-default-file-list-or-buffer ()
  "Return the default list of files or buffer to print.
In `dired-mode', get the marked files.  In other modes, if a
buffer has a file get the filename, otherwise return the buffer
itself."
  (if (derived-mode-p 'dired-mode)
      (dired-get-marked-files)
    (or (when-let (ff (buffer-file-name))
          (list ff))
        (current-buffer))))

(defclass transient-extras-files-or-buffer (transient-infix)
  ((key         :initform "--")
   (argument    :initform "--")
   (reader      :initform #'transient-extras-read-file)
   (always-read :initform t))
  "A transient class to read list of files.
The slot `value' is either a list of files or a single buffer.")

(cl-defmethod transient-format-value ((obj transient-extras-files-or-buffer))
  "Format OBJ's value for display and return the result."
  (let ((argument (oref obj argument)))
    (if-let ((value (oref obj value)))
        (propertize
         (if (listp value)
             ;; Should be list of files.
             (mapconcat (lambda (x)
                          (file-relative-name
                           (abbreviate-file-name (string-trim x "\"" "\""))))
                        value " ")
           ;; Should be a buffer
           (prin1-to-string value))
         'face 'transient-value)
      (propertize argument 'face 'transient-inactive-value))))

(defun transient-extras-read-file (prompt _initial-input _history)
  "PROMPT for file name.

Returns a list containing the filename. The file must exist."
  (list (file-local-name (expand-file-name
                          (read-file-name prompt nil nil t)))))

(transient-define-argument transient-extras-file-list-or-buffer ()
  :description "Files"
  :init-value (lambda (obj)
                (setf (slot-value obj 'value)
                      (transient-extras--get-default-file-list-or-buffer)))
  :class 'transient-extras-files-or-buffer)


;;; Switches with mutual exclusion

(defclass transient-extras-exclusive-switch (transient-switches) ()
  "Class used for mutually exclusive command-line switches.
Similar to `transient-switches' except it allows choices to
contain different values and labels. In particular, Each element
in `choices' is a cons of (value . \"label\") and label is used
for the display.")

(cl-defmethod transient-infix-read ((obj transient-extras-exclusive-switch))
  "Cycle through the mutually exclusive switches in `choices'."
  (let* ((choices (mapcar
                   (apply-partially #'format (oref obj argument-format))
                   (mapcar
                    (lambda (x)
                      ;; Return car of X if it is a cons, otherwise return X.
                      (if (consp x) (car x) x))
                    (oref obj choices)))))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-format-value ((obj transient-extras-exclusive-switch))
  "Format OBJ's value for display and return the result."
  (with-slots (value argument-format choices) obj
    (format (concat
             (mapconcat
              (lambda (choice)
                (propertize
                 (if (consp choice) (cdr choice) choice)
                 'face
                 (if (equal (format argument-format
                                    (if (consp choice) (car choice) choice))
                            value)
                     'transient-value
                   'transient-inactive-value)))
              choices
              (propertize "|" 'face 'transient-inactive-value))))))

;; TODO: Maybe use `choices' slot to `transient-init-value'


;;; Read from command

(defclass transient-extras-options-from-command (transient-switch)
  ((command-line :initarg :command-line)
   (filter-function :initarg :filter-function :initform #'identity)
   (cache-choices-p :initarg :cachep :initform nil)
   (prompt :initarg :prompt :initform "Prompt? ")
   (cached-choices :initform nil))
  "Class used for command line options which get their arguments
from a command.")

;; TODO: Allow for async-completing read?
(cl-defmethod transient-infix-read ((obj transient-extras-options-from-command))
  (if-let ((choices (oref obj cached-choices)))
      (completing-read (oref obj prompt) choices nil t)
    (with-slots (command-line filter-function cache-choices-p prompt) obj
      (let* ((lines (split-string (with-temp-buffer
                                    (apply (apply-partially #'call-process (first command-line) nil t nil) (rest command-line))
                                    (buffer-string))
                                  "\n" 'omit-nulls))
             (choices (cl-remove-if #'null (mapcar filter-function lines))))
        (when cache-choices-p
          (setf (oref obj cached-choices) choices))
        (completing-read prompt choices nil t)))))

(provide 'transient-extras-common)

;;; transient-extras-common.el ends here
