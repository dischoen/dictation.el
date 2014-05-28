;;; dication.el --- playing audio files, with single key pausing

;; Copyright (C) 2014 Dieter Schoen

;; Author: Dieter Schoen
;; Keywords: audio, transcription
;; Homepage: https://github.com/dischoen/dictation.el

;; This file is NOT part of GNU Emacs.

;; dictation.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; dictation.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with dication.el.  If not, see <http://www.gnu.org/licenses/>.

;; Requirements:
;; audio players for console
;; mp3: mpg123
;; ogg: mplayer
;;; Code:

(defun dictation-version (&optional here full message)
  "Show the version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
(let ((versionstr "dication version 0.1"))
  (when here (insert versionstr))
  (when message (message "%s" versionstr))
  versionstr))


(defconst dictation-version (dictation-version))

(defvar dictation-proc nil
  "This variable holds the process object of the audio provessor.")

(defvar pause-key (kbd "<f4>")
  "Key which is used in a temporary local binding for the pause function.")

(defvar pause-key-old-command nil
  "Command which was previously assigned to pause-key.
Saved here to enable restoration afterwards.")

(defun dictation-processor (file)
  "Check the extension of file and returns the appropriate
processor. Currently mpg123 for mp3 and mplayer for ogg
are supported. Maybe expand this to a map."
  (let ((extension (car (last (split-string file "[.]" t)))))
    (cond
     ((string= "ogg" extension) "mplayer")
     ((string= "mp3" extension) "mpg123")
     (t nil))))

;; test: (dictation-processor "bla.ogg")
;; test: (dictation-processor "bla.mp3")
;; test: (dictation-processor "bla.ordsdag")
;; test: (local-set-key pause-key 'next-line)

(defun dictation-start (file)
  "Starts a dictation with voice file FILE in mpg123."
  (interactive "fvoice file:")
  (setq dictation-proc (start-process
                        "dictation"
                        "*dictation*"
                        (dictation-processor file)
                        (expand-file-name file)))
  (setq pause-key-old-command (local-key-binding pause-key))
  (local-set-key pause-key 'dictation-pause)
  (set-process-sentinel dictation-proc 'dictation-sentinel))

(defun dictation-pause ()
  "Pauses and continues a audio output."
  (interactive)
  (when (process-live-p dictation-proc)
    (if (eq 'run (process-status dictation-proc))
        (progn
          (message "stopping")
          ;;(stop-process dictation-proc)
          (signal-process (process-id dictation-proc) 'STOP))
      (progn
        (message "continuing")
        (continue-process dictation-proc)))))

(defun dictation-stop ()
  "Terminates a audio output."
  (interactive)
  (when (process-live-p dictation-proc)
    (delete-process dictation-proc))
  (if pause-key-old-command
      (local-set-key pause-key
                     (indirect-variable pause-key-old-command))
    (local-unset-key pause-key))
  (message "dictation ended"))



(defun dictation-sentinel (process event)
  (when (not (process-live-p dictation-proc))
    (dictation-stop)))



