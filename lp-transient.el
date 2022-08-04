;;; lp-transient.el --- A transient interface to lp  -*- lexical-binding:t -*-

;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; URL: https://github.com/haji-ali/lp-transient.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))
;; Keywords: printer, transient, gui

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
;; This package provides a simple transient menu with common options for `lp'.
;;
;; Typical usage:
;;
;; (require 'lp-transient)
;;
;; (with-eval-after-load 'dired
;;   (define-key
;;     dired-mode-map
;;     (kbd "C-c C-p") #'lp-transient))
;; (with-eval-after-load 'pdf-tools
;;   (define-key
;;     pdf-misc-minor-mode-map
;;     (kbd "C-c C-p") #'lp-transient))
;;
;; Or simply call `lp-transient' to print the current buffer or the selected
;; files is selected in `dired'.

;;; Code:


(require 'transient)

(defclass lp-transient-switches (transient-switches) ()
  "Class used for mutually exclusive command-line switches.
Similar to `transient-switches' except it allows choices to
contain different values and labels. In particular, Each element
in `choices' is a cons of (value . \"label\") and label is used
for the display.")

;; TODO: (Planned feature) A way to add/remove files instead of replacing.
;; For example by pressing:
;; `-+': add files
;; `-=': remove files
;; `--': replace files

(defun lp-transient-read-file (prompt _initial-input _history)
  "Read a file name.
Returns a list containing the filename. The file must exist."
  (list (file-local-name (expand-file-name
                          (read-file-name prompt nil nil t)))))

(defclass lp-transient-files-or-buf (transient-infix)
  ((key         :initform "--")
   (argument    :initform "--")
   (reader      :initform #'lp-transient-read-file)
   (always-read :initform t))
  "A transient class to read list of files.
The slot `value' is either a list of files or a single buffer.")

(cl-defmethod transient-infix-read ((obj lp-transient-switches))
  "Cycle through the mutually exclusive switches in `choices'."
  (let* ((choices (mapcar
                   (apply-partially #'format (oref obj argument-format))
                   (mapcar
                    (lambda (x)
                      ; Return car of X if it is a cons, otherwise return X.
                      (if (consp x) (car x) x))
                    (oref obj choices)))))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-format-value ((obj lp-transient-switches))
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

(cl-defmethod transient-format-value ((obj lp-transient-files-or-buf))
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

(transient-define-argument lp-transient--orientation ()
  :description "Print Orientation"
  :class 'lp-transient-switches
  :key "o"
  :argument-format "-oorientation-requested=%s"
  :argument-regexp "\\(-oorientation-requested=\\(4\\|5\\|6\\)\\)"
  :choices '(("4" . "90°(landscape)")
             ("5" . "-90°")
             ("6" . "180°")))

(transient-define-argument lp-transient--quality ()
  :description "Print Quality"
  :class 'lp-transient-switches
  :key "l"
  :argument-format "-oprint-quality=%s"
  :argument-regexp "\\(-oprint-quality=\\(3\\|4\\|5\\)\\)"
  :choices '(("3" . "draft")
             ("4" . "normal")
             ("5" . "best")))

(transient-define-argument lp-transient--per-page ()
  :description "Per page"
  :class 'lp-transient-switches
  :key "C"
  :argument-format "-onumber-up=%s"
  :argument-regexp "\\(-onumber-up=\\(2\\|4\\|6\\|9\\|16\\)\\)"
  :choices '("2" "4" "6" "9" "16"))

(transient-define-argument lp-transient--media ()
  :description "Page size"
  :class 'lp-transient-switches
  :key "m"
  :argument-format "-omedia=%s"
  :argument-regexp "\\(-omedia=\\(a4\\|letter\\|legal\\)\\)"
  :choices '("a4" "letter" "legal"))

(transient-define-argument lp-transient--sides ()
  :description "Sides"
  :class 'lp-transient-switches
  :key "s"
  :argument-format "-osides=%s"
  :argument-regexp "\\(-osides=\\(one-sided\\|two-sided-long-edge\\|two-sided-short-edge\\)\\)"
  :choices '("one-sided" "two-sided-long-edge" "two-sided-short-edge"))

(defun lp-transient--get-default-file-or-buf ()
  "Return the default list of files or buffer to print.
In `dired-mode', get the marked files. In other modes, if a
buffer has a file get the filename, otherwise return the buffer
itself."
  (if (derived-mode-p 'dired-mode)
      (dired-get-marked-files)
    (or (when-let (ff (buffer-file-name))
          (list ff))
        (current-buffer))))

(transient-define-argument lp-transient--files ()
  :description "Files"
  :init-value (lambda (obj)
                (setf
                 (slot-value obj 'value) ; get value
                 (lp-transient--get-default-file-or-buf)))
  :class 'lp-transient-files-or-buf)

(defvar lp-transient-saved-options nil
  "List of options that will be passed by default to `lp'.")

(defun lp-transient--read-printer (prompt initial-input history)
  "Read printer name.
Uses the command `lpstat -a' to show a list of printers. If
`async-completing-read' and `acr-preprocess-lines-from-process'
are defined, use these functions to show the list
asynchronously."
  (let ((preprocess-lines-fun
         (lambda (x)
           ;; Accept only the first word in each line
           (mapcar
            (lambda (y)
              (let ((ind (string-match "[[:space:]]" y)))
                (if ind
                    (substring y nil ind)
                  y)))
            x))))
    (if (fboundp 'async-completing-read)
        (if (fboundp 'acr-preprocess-lines-from-process)
            (async-completing-read
             prompt
             (acr-preprocess-lines-from-process
              preprocess-lines-fun
              "lpstat" "-a")
             nil nil initial-input history)
          (car (funcall preprocess-lines-fun
                        (list (async-completing-read
                               prompt
                               (acr-lines-from-process "lpstat" "-a")
                               nil nil initial-input history)))))
      (completing-read
       prompt
       (funcall preprocess-lines-fun
                (split-string
                 (with-temp-buffer
                   (call-process "lpstat" nil t nil "-a")
                   (buffer-string))
                 "\n" 'omit-nulls))
       nil nil initial-input history))))

(defun lp-transient--read-pages (prompt initial-input history)
  "Read pages that will be printed.
Get pages count from `pdf-info-number-of-pages' when defined and
in `pdf-mode' and display the maximum in the prompt."
  (read-string
   (if (and (fboundp 'pdf-info-number-of-pages)
            (derived-mode-p 'pdf-view-mode))
       (format "%s[max %d]: " prompt (pdf-info-number-of-pages))
     prompt)
   initial-input history))

(defun lp-transient (buf-or-files &optional args)
  "Call `lp' with list of files or a buffer.
BUF-OR-FILES is a buffer or a list of files. ARGS are the
arguments that should be passed to `lp'"
  (interactive (list (lp-transient--get-default-file-or-buf)))
  (unless (or (bufferp buf-or-files)
              (listp buf-or-files))
    (user-error "Wrong first argument to `lp-transient'"))

  (let ((program (executable-find "lp")))
    (unless program
      (error "No print program available"))
    (let* ((cmd (nconc (list program)
                       args
                       (and (listp buf-or-files)
                            buf-or-files)))
           (process (make-process
                     :name "printing"
                     :buffer nil
                     :connection-type 'pipe
                     :command cmd)))
      (when (bufferp buf-or-files)
        ;; Send the buffer content to the process
        (process-send-string process
                             (with-current-buffer buf-or-files
                               (buffer-string)))
        (process-send-eof process))
      (message "Print job started: %s"
               (mapconcat #'identity cmd " ")))))

(defun lp-transient/save-options (args)
  "Save printer options as default.
The options, taken from `transient' by default, are saved so
that the next time the `transient' menu is displayed these
options are automatically selected."
  (interactive (list (cdr (transient-args 'lp-transient-menu))))
  (setq lp-transient-saved-options args)
  (message "Saved"))

(defun lp-transient/do-print (args)
  "Call `lp-transient' with `transient' arguments."
  (interactive (list (transient-args 'lp-transient-menu)))
  ;; NOTE: This is relying on the order. This works with latest `transient'
  ;; but future updates might break this
  (lp-transient (car args) (cdr args)))

(transient-define-prefix lp-transient-menu (filename)
  "Call `lp' with various options"
  :init-value (lambda (obj)
                (oset obj value lp-transient-saved-options))

  [(lp-transient--files)]

  [["Argument"
    ("n" "copies" "-n" :always-read t
     :class transient-option
     :prompt "Number of copies? ")
    ("p" "Pages" "-P" :always-read t
     :class transient-option
     :prompt "Pages? "
     :reader lp-transient--read-pages)
    ("d" "Printer" "-d"
     :prompt "Printer? "
     :class transient-option
     :always-read t :reader lp-transient--read-printer)]

   ["Options"
    (lp-transient--sides)
    (lp-transient--media)
    (lp-transient--per-page)
    (lp-transient--orientation)
    (lp-transient--quality)
    ("f" "Fit to page" "-ofit-to-page")]]

  [["Commands"
    ("C-c C-c" "Print"
     lp-transient/do-print
     :transient nil)]

   ["" ("C-c C-s" "Save options"
        lp-transient/save-options
        :transient t)]])

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables
               'lp-transient-saved-options))

(provide 'lp-transient)
