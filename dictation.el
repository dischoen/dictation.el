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
;;
;; Installation:
;;(add-to-list 'load-path (expand-file-name "~/git/dictation.el"))
;;(require 'dictation)


;;; Code:

(defun dictation-version (&optional here full message)
  "Show the version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
(let ((versionstr "dication version 0.2"))
  (when here (insert versionstr))
  (when message (message "%s" versionstr))
  versionstr))


(defconst dictation-version (dictation-version))

(defvar dictation-proc nil
  "This variable holds the process object of the audio provessor.")

(defvar pause-key (kbd "<f4>")
  "Key which is used in a temporary local binding for the pause function.")

(defvar jump-back-key (kbd "<f3>")
  "Key which is used in a temporary local binding for the rewind function.")

(defvar jump-forward-key (kbd "<f5>")
  "Key which is used in a temporary local binding for the forward function.")

(defvar pause-key-old-command nil
  "Command which was previously assigned to pause-key.
Saved here to enable restoration afterwards.")

(defvar jump-back-key-old-command nil
  "Command which was previously assigned to jump-back-key.
Saved here to enable restoration afterwards.")

(defvar jump-forward-key-old-command nil
  "Command which was previously assigned to jump-forward-key.
Saved here to enable restoration afterwards.")

(defun dictation-start (file)
  "Starts a dictation with voice file FILE in mpg123."
  (interactive "fvoice file:")
  (setq dictation-proc (start-process
                        "dictation"
                        "*dictation*"
                        "mplayer"
                        "-slave"
                        (expand-file-name file)))
  (setq pause-key-old-command (local-key-binding pause-key))
  (local-set-key pause-key 'dictation-pause)
  
  (setq jump-back-key-old-command (local-key-binding jump-back-key))
  (local-set-key jump-back-key 'dictation-jump-back)
  
  (setq jump-forward-key-old-command (local-key-binding jump-forward-key))
  (local-set-key jump-forward-key 'dictation-jump-forward)
  
  (set-process-sentinel dictation-proc 'dictation-sentinel))

(defun dictation-pause ()
  "Pauses and continues a audio output."
  (interactive)
  (when (process-live-p dictation-proc)
    (process-send-string dictation-proc "pause\n")))

(defun dictation-jump-back ()
  "Jump back 10 seconds."
  (interactive)
  (when (process-live-p dictation-proc)
    (process-send-string dictation-proc "seek -10\n")))

(defun dictation-jump-forward ()
  "Jump forward 10 seconds."
  (interactive)
  (when (process-live-p dictation-proc)
    (process-send-string dictation-proc "seek 10\n")))

(defun dictation-stop ()
  "Terminates a audio output."
  (interactive)
  (when (process-live-p dictation-proc)
    (delete-process dictation-proc))
  (if pause-key-old-command
      (local-set-key pause-key
                     (indirect-variable pause-key-old-command))
    (local-unset-key pause-key))
  (if jump-back-key-old-command
      (local-set-key jump-back-key
                     (indirect-variable jump-back-key-old-command))
    (local-unset-key jump-back-key))
  (if jump-forward-key-old-command
      (local-set-key jump-forward-key
                     (indirect-variable jump-forward-key-old-command))
    (local-unset-key jump-forward-key))
  (message "dictation ended")
  (kill-buffer (process-buffer dictation-proc)))

(defun dictation-sentinel (process event)
  (when (not (process-live-p dictation-proc))
    (dictation-stop)))

(provide 'dictation)

