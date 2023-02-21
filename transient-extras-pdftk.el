;;; transient-extras-pdftk.el --- A transient interface to pdftk -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; URL: https://github.com/haji-ali/transient-extras.git
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") transient-extras)
;; Keywords: Convenience
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; 

;;; Code:

(require 'transient)
(require 'transient-extras)


;; Options

(defun transient-extras-pdftk-read-a-file (prompt initial-input _history)
  "PROMPT for filename with INITIAL-INPUT.

Returns a filepath, possibly non-existent."
  (file-local-name (expand-file-name (read-file-name prompt nil nil nil initial-input))))

(transient-define-argument transient-extras-pdftk-dump ()
  :description "Dump Data"
  :class 'transient-extras-exclusive-switch
  :key "Dd"
  :argument-format "dump_data_%s"
  :argument-regexp "\\(dump_data_\\(utf8\\|fields\\|fields_utf8\\|annots\\)\\)"
  :choices '("utf8" "fields" "fields_utf8" "annots"))

(transient-define-argument transient-extras-pdftk-encrypt ()
  :description "Encryption Mode"
  :class 'transient-extras-exclusive-switch
  :key "Em"
  :argument-format "encrypt_%s"
  :argument-regexp "\\(encrypt_\\(40bit\\|128bit\\|aes128\\)\\)"
  :choices '("40bit" "128bit" "aes128"))

(transient-define-argument transient-extras-pdftk-compression ()
  :description "Compression"
  :class 'transient-extras-exclusive-switch
  :key "oc"
  :argument-format "%s"
  :argument-regexp "\\(\\(compress\\|uncompress\\)\\)"
  :choices '("compress" "uncompress"))

(transient-define-argument transient-extras-pdftk-keep-id ()
  :description "ID Keeping"
  :class 'transient-extras-exclusive-switch
  :key "oi"
  :argument-format "keep_%s_id"
  :argument-regexp "\\(keep_\\(final\\|first\\)_id\\)"
  :choices '("first" "final"))

(transient-define-argument transient-extras-pdftk-ask ()
  :description "Ask Questions?"
  :class 'transient-extras-exclusive-switch
  :key "op"
  :argument-format "%s_ask"
  :argument-regexp "\\(\\(dont\|do\\)_ask\\)"
  :choices '("dont" "do"))


;; Run Program

(defun transient-extras-pdftk-run (files &optional arguments)
  "Run `pdftk' with ARGUMENTS on FILES."
  (interactive (list (transient-extras--get-dedault-file-list-or-buffer)))
  (unless (or (bufferp files)
              (listp files))
    (user-error "`files' must be a buffer or list"))
  (if-let ((program (executable-find "pdftk")))
      (let* ((cmd (nconc (list program)
                         (if (listp files) files (list "-"))
                         arguments))
             (buffer (generate-new-buffer "*pdftk-out-buffer*"))
             (process (make-process :name "pdftk-run"
                                    :buffer buffer
                                    :stderr buffer
                                    :connection-type 'pipe
                                    :command cmd)))
        (message "Started %s" (mapconcat #'identity cmd " "))
        (when (bufferp files)
          (process-send-string process (with-current-buffer files (buffer-string)))
          (process-send-eof process))
        (while (accept-process-output process))
        (let ((output (with-current-buffer buffer (buffer-string))))
          (kill-buffer buffer)
          (message "%s" (string-trim output))))
    (error "No `pdftk' executable available")))

(defun transient-extras-pdftk-do-run (arguments)
  "Call `transient-extras-pdftk-run' with current `transient' ARGUMENTS."
  (interactive (list (transient-args 'transient-extras-pdftk-menu)))
  (transient-extras-pdftk-run (car arguments) (cdr arguments)))

(defun transient-extras-pdftk ()
  "Start the `pdftk' transient menu."
  (interactive)
  (call-interactively #'transient-extras-pdftk-menu))


;; Build the Transient Menu

(transient-define-prefix transient-extras-pdftk-menu (filename)
  "Call `pdftk' with various arguments and options."
  :man-page "pdftk"

  [(transient-extras-file-list-or-buffer)]

  [["Password"
    ("Ep" "Input Password" "input_pw %s"
     :prompt "Password? "
     :class transient-extras-password)]]

  [["Operations"
    ("Dc" "Cat" "cat "
     :class transient-option
     :prompt "Page ranges? ")
    ("Ds" "Shuffle" "shuffle "
     :class transient-option
     :prompt "Page ranges? ")
    ("Db" "Burst" "burst")
    ("Dr" "Rotate" "rotate "
     :class transient-option
     :prompt "Rotate Pages? ")
    ("Dg" "Generate FDF" "generate_fdf")
    ("Df" "Fill Form" "fill_form "
     :class transient-option
     :prompt "Form data file? "
     :reader transient-extras-pdftk-read-a-file)
    ("DB" "Background" "background "
     :class transient-option
     :prompt "Background Filename? "
     :reader transient-extras-pdftk-read-a-file)]
   [""
    ("Dm" "Multibackground" "multibackground "
     :class transient-option
     :prompt "Multibackground filename? "
     :reader transient-extras-pdftk-read-a-file)
    ("DS" "Stamp" "stamp "
     :class transient-option
     :prompt "Stamp filename? "
     :reader #'transient-extras-pdftk-read-a-file)
    ("DM" "Multistamp" "multistamp "
     :class transient-option
     :prompt "Multistamp Filename? "
     :reader transient-extras-pdftk-read-a-file)
    (transient-extras-pdftk-dump)
    ("DU" "Update Info" "update_info_utf8 "
     :class transient-option
     :prompt "Information Filename? "
     :reader transient-extras-pdftk-read-a-file)
    ("Da" "Attach Files" "attach_files "
     :class transient-option
     :prompt "Attach Files? "
     :reader transient-extras-pdftk-read-a-file)
    ("Du" "Unpack Files" "unpack_files")]]


  [["Output"
    ("O" "Output File" "output "
     :prompt "Output Filename? "
     :class transient-option
     :reader transient-extras-pdftk-read-a-file)]]

  [["Encryption"
    (transient-extras-pdftk-encrypt)
    ("Eo" "Owner Password" "owner_pw %s"
     :prompt "Owner Password? "
     :class transient-extras-password)]
   [""
    ("Eu" "User Password" "user_pw %s"
     :prompt "User Password? "
     :class transient-extras-password)
    ("Ea" "Allow" "allow "
     :prompt "Permissions? "
     :multi-value t
     :class transient-option
     :choices ("Printing" "DegradedPrinting" "ModifyContents"
               "Assembly" "CopyContents" "Assembly" "CopyContents"
               "ScreenReaders" "ModifyAnnotations" "FillIn" "AllFeatures"))]]


  [["Options"
    ("of" "Flatten" "flatten")
    ("oa" "Appearances" "need_appearances")
    (transient-extras-pdftk-compression)
    (transient-extras-pdftk-keep-id)
    (transient-extras-pdftk-ask)]
   [""
    ("oda" "Drop XFA Data" "drop_xfa")
    ("odp" "Drop XMP Data" "drop_xmp")
    ("or" "Replace Font" "replacement_font "
     :class transient-option
     :prompt "Replacement Font? ")
    ("ov" "Verbose Mode" "verbose")]]

  [[("C-c C-c" "Run"
     transient-extras-pdftk-do-run
     :transient nil)]])

(provide 'transient-extras-pdftk)

;;; transient-extras-pdftk.el ends here
