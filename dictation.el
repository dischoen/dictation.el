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

(defvar ds/dictation-proc nil
  "This variable holds the process object of the audio provessor.")

(defvar ds/pause-key (kbd "<f4>")
  "Key which is used in a temporary local binding for the pause function.")

(defvar ds/pause-key-old-command nil
  "Command which was previously assigned to ds/pause-key.
Saved here to enable restoration afterwards.")

(defun ds/dictation-processor (file)
  "Check the extension of file and returns the appropriate
processor. Currently mpg123 for mp3 and mplayer for ogg
are supported. Maybe expand this to a map."
  (let ((extension (car (last (split-string file "[.]" t)))))
    (cond
     ((string= "ogg" extension) "mplayer")
     ((string= "mp3" extension) "mpg123")
     (t nil))))

;; test: (ds/dictation-processor "bla.ogg")
;; test: (ds/dictation-processor "bla.mp3")
;; test: (ds/dictation-processor "bla.ordsdag")
;; test: (local-set-key ds/pause-key 'next-line)

(defun ds/dictation-start (file)
  "Starts a dictation with voice file FILE in mpg123."
  (interactive "fvoice file:")
  (setq ds/dictation-proc (start-process
                        "dictation"
                        "*dictation*"
                        (ds/dictation-processor file)
                        (expand-file-name file)))
  (setq ds/pause-key-old-command (local-key-binding ds/pause-key))
  (local-set-key ds/pause-key 'ds/dictation-pause)
  (set-process-sentinel ds/dictation-proc 'ds/dictation-sentinel))

(defun ds/dictation-pause ()
  "Pauses and continues a audio output."
  (interactive)
  (when (process-live-p ds/dictation-proc)
    (if (eq 'run (process-status ds/dictation-proc))
        (progn
          (message "stopping")
          ;;(stop-process ds/dictation-proc)
          (signal-process (process-id ds/dictation-proc) 'STOP))
      (progn
        (message "continueing")
        (continue-process ds/dictation-proc)))))

(defun ds/dictation-stop ()
  "Terminates a audio output."
  (interactive)
  (when (process-live-p ds/dictation-proc)
    (delete-process ds/dictation-proc))
  (if ds/pause-key-old-command
      (local-set-key ds/pause-key
                     (indirect-variable ds/pause-key-old-command))
    (local-unset-key ds/pause-key))
  (message "dictation ended"))



(defun ds/dictation-sentinel (process event)
  (when (not (process-live-p ds/dictation-proc))
    (ds/dictation-stop)))



