;;; transient-extras-find.el --- A transient interface to `find' -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; URL: https://github.com/haji-ali/lp-transient
;; Version: 0.1.0
;; Package-Require: ((emacs "28.0"))
;; Keywords: find, transient

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
;; TODO

(require 'transient)
(require 'transient-extras)

;;; Code:

;; Options


;; Running Find

(defun transient-extras-find--run (directory &rest args)
  "Run `find-dired' in DIRECTORY using ARGS configured with `transient-extras-find-menu'."
  (find-dired directory (string-join args " ")))

(defun transient-extras-find-do-run (arguments)
  "Call `transient-extras-find--run' with `transient' ARGUMENTS."
  (interactive (list (transient-args 'transient-extras-find-menu)))
  (transient-extras-find--run (car arguments) (cdr arguments)))

(defun transient-extras-find ()
  "Start the a2ps transient menu."
  (interactive)
  (call-interactively #'transient-extras-find-menu))


;; Build the Transient

(transient-define-prefix transient-extras-find-menu ()
  "TODO: Doc String"
  :man-page "find"
  :info-manual "find"

  [(transient-extras-directory)]

  ;; -depth
  ;; -mount
  ;; -noleaf

  ;; -amin
  ;; -anewer
  ;; -atime
  ;; -cmin
  ;; -cnewer
  ;; -ctime
  ;; -newer

  ;; -empty
  ;; -executable

  ;; -gid
  ;; -group
  ;; -uid
  ;; -user
  ;; -nogroup
  ;; -nouser

  ;; -ilname
  ;; -iname
  ;; -name
  ;; -lname
  ;; -path
  ;; -perm
  ;; -regex

  ;; -type=bcdpflsD


  ;; -readable
  ;; -writable
  ;; -size

  ;; -links


  ;; Last one!
  [("C-c C-c" "Find"
    transient-extras-find-do-run
    :transient nil)])


(provide 'transient-extras-find)

;; transient-extras-find.el ends here
