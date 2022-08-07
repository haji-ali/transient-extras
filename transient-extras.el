;; transient-extras.el --- Extra features for transient -*- lexical-binding: t -*-

;; TODO: Package Header

;; TODO: License information

;;; Commentary:

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
   (filter-function :initarg :filter-function)
   (cache-choices-p :initarg :cachep)
   (prompt :initarg :prompt)
   (cached-choices))
  "Class used for command line options which get their arguments
from a command.")

(cl-defmethod transient-infix-read ((obj transient-extras-options-from-command))
  (if (slot-boundp obj 'cached-choices)
      (completing-read (oref obj prompt) (oref obj cached-choices) nil t)
    (with-slots (command-line filter-function cache-choices-p prompt) obj
      (let* ((lines (split-string (with-temp-buffer
                                    (apply (apply-partially #'call-process (first command-line) nil t nil) (rest command-line))
                                    (buffer-string))
                                  "\n" 'omit-nulls))
             (choices (cl-remove-if #'null (mapcar filter-function lines))))
        (when (and (boundp 'cache-choices-p) cache-choices-p)
          (setf (oref obj cached-choices) choices))
        (completing-read prompt choices nil t)))))

(provide 'transient-extras)

;;; transient-extras.el ends here
