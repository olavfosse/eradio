;;; eradio.el --- A simple Internet radio player                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Olav Fosse

;; Author: Olav Fosse <mail@olavfosse.no>
;; Version: 0.1
;; URL: https://github.com/fossegrim/eradio
;; Package-Requires: ((emacs "24.1"))

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

;; A simple Internet radio player

;;; Code:

;;;###autoload
(defcustom eradio-channels '()
  "Eradio's radio channels."
  :type '(repeat (cons (string :tag "Name") (string :tag "URL")))
  :group 'eradio)

(defcustom eradio-player '("vlc" "--no-video" "-I" "rc")
  "Eradio's player.
This is a list of the program and its arguments.  The url will be appended to the list to generate the full command."
  :type '(choice
	  (const :tag "vlc"
		 ("vlc" "--no-video" "-I" "rc"))
	  (const :tag "vlc-mac"
		 ("/Applications/VLC.app/Contents/MacOS/VLC" "--no-video" "-I" "rc"))
	  (const :tag "mpv"
		 ("mpv" "--no-video" "--no-terminal")))
  :group 'eradio)

(defvar eradio--process nil "The process running the radio player.")

(defvar eradio--process-name "eradio--process" "The name of the process running the radio player.")

(defvar eradio-current-channel nil "The currently playing (or paused) channel.")

(defun eradio--alist-keys (alist)
  "Get the keys from an ALIST."
  (mapcar #'car alist))

;;;###autoload
(defun eradio-stop ()
  "Stop the radio player."
  (interactive)
  (mapc (lambda (proc)
	  (when (string-prefix-p eradio--process-name (process-name proc))
	    (delete-process proc)))
	(process-list))
  (setq eradio--process nil))

;;;###autoload
(defun eradio-toggle ()
  "Toggle the radio player."
  (interactive)
  (if eradio--process
    (if (eq (process-status eradio--process) 'stop)
	(continue-process eradio--process)
      (signal-process eradio--process 'STOP))
  ;; If eradio-current-channel is nil, eradio-play will prompt the url
  (eradio-play eradio-current-channel)))

(defun eradio--play-low-level (url old-process old-channel)
  "Play radio channel URL in a new process."
  (setq eradio-current-channel url)
  (setq eradio--process
	(apply #'start-process
	       `(,eradio--process-name nil ,@eradio-player ,url)))
  (set-process-sentinel eradio--process (eradio--make-sentinel url old-process old-channel)))

(defun eradio--get-url ()
  "Get a radio channel URL from the user."
  (let ((eradio-channel (completing-read
			 "Channel: "
			 (eradio--alist-keys eradio-channels)
			 nil nil)))
    (or (cdr (assoc eradio-channel eradio-channels)) eradio-channel)))

(defun eradio--make-sentinel (url old-process old-channel) 
  "Make a process sentinel that restores the old channel when the new one is not playable."
  (lambda (proc event-desc)
    "eradio sentinel"
    (if (process-live-p eradio--process)
	(when (process-live-p old-process) (delete-process old-process))
      (when (process-live-p old-process) (continue-process old-process))
      (setq eradio-current-channel old-channel)
      (setq eradio--process old-process)
      (message "Cannot play URL: %s" url))))

;;;###autoload
(defun eradio-play (&optional url)
  "Play a radio channel, do what I mean."
  (interactive)
  (let ((url (or url (eradio--get-url)))
	(old-channel eradio-current-channel)
	(old-process eradio--process))
    (when (process-live-p old-process) (signal-process old-process 'STOP))
    (eradio--play-low-level url old-process old-channel)))

(provide 'eradio)
;;; eradio.el ends here
