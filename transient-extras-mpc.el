;;; transient-extras-mpc-.el --- Transient interface to MPD -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; URL: https://github.com/haji-ali/transient-extras
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, multimedia

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This package provides a transient menu to interface with mpd/mpc.
;; Simply bind `transient-extras-mpc' to a convenient binding.

(require 'transient)

;;; Code:

;;;; Customization

(defgroup transient-extras-mpc ()
  "Transient interface to mpc."
  :group 'multimedia)

(defcustom transient-extras-mpc-status-format "%artist% - %title%"
  "Format for MPC Status."
  :type 'string
  :group 'transient-extras-mpc)


;;;; Run MPC commands

(defun transient-extras-mpc--mpc (args)
  "Call MPC with ARGS."
  (with-temp-buffer
    (apply (apply-partially #'call-process "mpc" nil t nil)
           (if (listp args)
               args
             (list args)))
    (string-trim (buffer-string))))


;;;; Completion data

(defun transient-extras-mpc--list-songs ()
  "Get a list of songs that can be played."
  (split-string (transient-extras-mpc--mpc "listall")
                "\n" 'omit-nulls))

(defun transient-extras-mpc--list-playlists ()
  "Get a list of playlists to add."
  (split-string (transient-extras-mpc--mpc "lsplaylists")
                "\n" 'omit-nulls))


;;;; Read a "position" (integer, more or less)

(defun transient-extras-mpc--read-position (prompt &optional allow-empty-p)
  "PROMPT for position, if ALLOW-EMPTY-P, then empty strings are treated as nil."
  (let* ((backup-prompt (format "Please enter a number%s.\n%s"
                                (if allow-empty-p " or blank" "") prompt))
         (in-string (read-string prompt)))
    (while (not (or (string-match-p (rx bol (? ?-) (or ?0 (and (in (?1 . ?9)) (* (in (?0 . ?9))))) eol) in-string)
                    (and allow-empty-p (string-empty-p in-string))))
      (setf in-string (read-string backup-prompt)))
    (if (string-empty-p in-string)
        nil
      (string-to-number in-string))))


;;;; Suffixes - Autogeneration

(defmacro transient-extras-mpc--define-suffix-command (command)
  "Generate a simple suffix for mpd COMMAND."
  `(defun ,(intern (format "transient-extras-mpc--suffix-%s" command)) ()
     ,(format "Run MPD Command %s." command)
     (interactive)
     (message "%s" (transient-extras-mpc--mpc ,command))))


;;;; Formatted Information - Autogeneration

(defmacro transient-extras-mpc--description-for (param)
  "Generate status getter for PARAM."
  `(defun ,(intern (format "transient-extras-mpc--status-%s" param)) ()
     ,(format "Get status for %s." param)
     (format "%s (%s)"
             ,param
             (transient-extras-mpc--mpc (list "status" ,(format "%%%s%%" param))))))


;; Formatted information - General

(defun transient-extras-mpc--status-general ()
  "Format current status."
  (let ((queue-length (string-to-number (transient-extras-mpc--mpc (list "status" "%length%"))))
        (queue-item (string-to-number (transient-extras-mpc--mpc (list "status" "%songpos%"))))
        (state (transient-extras-mpc--mpc (list "status" "%state%")))
        (play-status (nth 0 (string-split (transient-extras-mpc--mpc (list "status" "-f" transient-extras-mpc-status-format)) "\n"))))
    (format "MPD (%s)"
            (cond
             ((= queue-length 0) "Queue Empty")
             ((string= state "playing") (format "Playing: %s" play-status))
             ((and (string= state "paused")
                   (not (= queue-item 0)))
              (format "Paused: %s" play-status))
             ((string= state "paused")
              (format "Paused, %d items queued." queue-length))))))


;;;; Player Control

(defun transient-extras-mpc--suffix-play (arg)
  "Play.  If ARG, prompt for queue item."
  (interactive "P")
  (message "%s" (transient-extras-mpc--mpc
                 (if arg
                     (list "play" (number-to-string (transient-extras-mpc--read-position "Play position? ")))
                   "play"))))

(transient-extras-mpc--define-suffix-command "toggle")
(transient-extras-mpc--define-suffix-command "prev")
(transient-extras-mpc--define-suffix-command "next")

(defun transient-extras-mpc--suffix-seek (seek)
  "Seek to SEEK."
  (interactive (list (read-string "Seek to: ")))
  (message "%s" (transient-extras-mpc--mpc (list "seek" seek))))

(defun transient-extras-mpc--suffix-seekthrough (seek)
  "Seek through to SEEK."
  (interactive (list (read-string "Seek through: ")))
  (message "%s" (transient-extras-mpc--mpc (list "seekthrough" seek))))


;;;; Queue Management

(defun transient-extras-mpc--suffix-add (files)
  "Add FILES to MPD Queue."
  (interactive (list (completing-read-multiple "File(s): " (transient-extras-mpc--list-songs) nil t)))
  (message "%s" (transient-extras-mpc--mpc (cons "add" files))))

(defun transient-extras-mpc--suffix-insert (files)
  "Add FILES to MPD Queue."
  (interactive (list (completing-read-multiple "File(s): " (transient-extras-mpc--list-songs) nil t)))
  (message "%s" (transient-extras-mpc--mpc (cons "insert" files))))

(transient-extras-mpc--define-suffix-command "clear")
(transient-extras-mpc--define-suffix-command "crop")

(defun transient-extras-mpc--suffix-delete (position)
  "Delete Queue item at POSITION."
  (interactive (list (transient-extras-mpc--read-position "Delete position? ")))
  (message "%s" (transient-extras-mpc--mpc (list "del" (number-to-string position)))))

(defun transient-extras-mpc--suffix-move (from to)
  "Move song FROM position TO position."
  (interactive (list (transient-extras-mpc--read-position "From: ")
                     (transient-extras-mpc--read-position "To: ")))
  (message "%s" (transient-extras-mpc--mpc (list "mv"
                                                 (number-to-string from)
                                                 (number-to-string to)))))

(transient-extras-mpc--define-suffix-command "shuffle")


;;;; Playlists

(defun transient-extras-mpc--suffix-load-playlist (playlist)
  "Load PLAYLIST."
  (interactive (list (completing-read "Playlist: " (transient-extras-mpc--list-playlists))))
  (message "%s" (transient-extras-mpc--mpc (list "load" playlist))))

(defun transient-extras-mpc--suffix-save-playlist (name)
  "Save queue to NAME."
  (interactive (list (read-string "Playlist name: ")))
  (message "%s" (transient-extras-mpc--mpc (list "save" name))))

(defun transient-extras-mpc--suffix-remove-playlist (playlist)
  "Remove PLAYLIST."
  (interactive (list (completing-read "Playlist: " (transient-extras-mpc--list-playlists))))
  (message "%s" (transient-extras-mpc--mpc (list "rm" playlist))))


;;;; Audio

(defun transient-extras-mpc--status-audio ()
  "Get MPC audio status."
  (format "Audio (%s)"
          (let ((volume (transient-extras-mpc--mpc (list "status" "%volume%"))))
            (if (string-match "-1" volume)
                "N/A"
              volume))))

(defun transient-extras-mpc--suffix-dec-volume (vol)
  "Increase volume by VOL."
  (interactive (list (transient-extras-mpc--read-position "Decrease by? ")))
  (message "%s" (transient-extras-mpc--mpc (list "volume" (format "-%d" vol)))))

(defun transient-extras-mpc--suffix-set-volume (vol)
  "Set volume to VOL."
  (interactive (list (transient-extras-mpc--read-position "Volume? ")))
  (message "%s" (transient-extras-mpc--mpc (list "volume" (number-to-string vol)))))

(defun transient-extras-mpc--suffix-inc-volume (vol)
  "Increase volume by VOL."
  (interactive (list (transient-extras-mpc--read-position "Increase by? ")))
  (message "%s" (transient-extras-mpc--mpc (list "volume" (format "+%d" vol)))))


;;;; Player Modes

(transient-extras-mpc--define-suffix-command "repeat")
(transient-extras-mpc--description-for "repeat")

(transient-extras-mpc--define-suffix-command "random")
(transient-extras-mpc--description-for "random")

(transient-extras-mpc--define-suffix-command "single")
(transient-extras-mpc--description-for "single")

(transient-extras-mpc--define-suffix-command "consume")
(transient-extras-mpc--description-for "consume")

(defun transient-extras-mpc--suffix-crossfade (secs)
  "Set crossfade to SECS, disable if 0."
  (interactive (list (transient-extras-mpc--read-position "Crossfade for? ")))
  (message "%s" (transient-extras-mpc--mpc (list "crossfade" (number-to-string secs)))))

(defun transient-extras-mpc--status-crossfade ()
  "Disable crossfade status."
  (let* ((crossfade-string (transient-extras-mpc--mpc "crossfade"))
         (splits (mapcar #'string-trim (split-string crossfade-string ":")))
         (value (string-to-number (nth 1 splits))))
    (format "crossfade (%s)"
            (if (= 0 value)
                "disabled"
              (format "%ds" value)))))


;;;; Miscellaneous Operations

(transient-extras-mpc--define-suffix-command "update")
(transient-extras-mpc--define-suffix-command "rescan")


;;;; Implement the Transient

(transient-define-prefix transient-extras-mpc ()
  "Transient interface to mpc."
  :man-page "mpc"

  [:description transient-extras-mpc--status-general
                ["Player Control"
                 ("p" "play" transient-extras-mpc--suffix-play :transient t)
                 ("t" "toggle play/pause" transient-extras-mpc--suffix-toggle :transient t)
                 ("P" "previous song" transient-extras-mpc--suffix-prev :transient t)
                 ("N" "previous song" transient-extras-mpc--suffix-next :transient t)
                 ("s" "seek" transient-extras-mpc--suffix-seek :transient t)
                 ("S" "seekthrough" transient-extras-mpc--suffix-seekthrough :transient t)]
                ["Queue Management"
                 ("qa" "add" transient-extras-mpc--suffix-add :transient t)
                 ("qi" "insert" transient-extras-mpc--suffix-insert :transient t)
                 ("qc" "clear" transient-extras-mpc--suffix-clear :transient t)
                 ("qC" "crop" transient-extras-mpc--suffix-crop :transient t)
                 ("qk" "delete" transient-extras-mpc--suffix-delete :transient t)
                 ("qM" "move" transient-extras-mpc--suffix-move :transient t)
                 ("qs" "shuffle" transient-extras-mpc--suffix-shuffle :transient t)
                 ]
                ["Playlists"
                 ("ll" "load playlist" transient-extras-mpc--suffix-load-playlist :transient t)
                 ("ls" "save playlist" transient-extras-mpc--suffix-save-playlist :transient t)
                 ("lr" "drop playlist" transient-extras-mpc--suffix-remove-playlist :transient t)]
                [:description transient-extras-mpc--status-audio
                              ("v-" "decrease volume" transient-extras-mpc--suffix-dec-volume :transient t)
                              ("vs" "set volume" transient-extras-mpc--suffix-set-volume :transient t)
                              ("v+" "increase volume" transient-extras-mpc--suffix-inc-volume :transient t)]
                ["Player Modes"
                 ("mr" transient-extras-mpc--suffix-repeat :description transient-extras-mpc--status-repeat :transient t)
                 ("mR" transient-extras-mpc--suffix-random :description transient-extras-mpc--status-random :transient t)
                 ("ms" transient-extras-mpc--suffix-single :description transient-extras-mpc--status-single :transient t)
                 ("mc" transient-extras-mpc--suffix-consume :description transient-extras-mpc--status-consume :transient t)
                 ("mC" transient-extras-mpc--suffix-crossfade :description transient-extras-mpc--status-crossfade :transient t)]
                ["Miscellaneous"
                 ("u" "update" transient-extras-mpc--suffix-update :transient t)
                 ("r" "rescan" transient-extras-mpc--suffix-rescan :transient t)]])

(provide 'transient-extras-mpc)

;;; transient-extras-mpc.el ends here
