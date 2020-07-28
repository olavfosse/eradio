;;; eradio.el --- A radio player frontend for vlc                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Olav Fosse

;; Author: Olav Fosse <fosseolav@gmail.com>
;; Version: 0.1
;; URL: https://github.com/olav35/eradio
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

;; A radio player frontend for vlc

;;; Code:

;;;###autoload
(defvar eradio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")
                         ("metal - soma fm" . "https://somafm.com/metal130.pls")
                         ("groove salad - soma fm" . "https://somafm.com/groovesalad256.pls")
                         ("secret agent - soma fm" . "https://www.somafm.com/secretagent.pls")
                         ("cyberia - lainon". "https://lainon.life:8000/cyberia.ogg")
                         ("cafe - lainon" . "https://lainon.life:8000/cafe.ogg")
                         ("swing - lainon" . "https://lainon.life:8000/swing.ogg")
                         ("everything - lainon" . "https://lainon.life:8000/everything.ogg"))
  "Eradio's radio channels.")

(defvar eradio-process nil "The process running the radio player.")

(defun eradio-alist-keys (alist)
  "Get the keys from an ALIST."
  (mapcar #'car alist))

(defun eradio-stop ()
  "Stop the radio player."
  (interactive) (when eradio-process (delete-process eradio-process)))

(defun eradio-play-low-level (url)
  "Play radio channel URL in a new process."
  (setq eradio-process (start-process "eradio-process" nil "vlc" "--no-video" "-I" "rc" (shell-quote-argument url))))

(defun eradio-get-url ()
  "Get a radio channel URL from the user."
  (let ((eradio-channel (completing-read
                       "Channel: "
                       (eradio-alist-keys eradio-channels)
                       nil nil)))
  (or (cdr (assoc eradio-channel eradio-channels)) eradio-channel)))

;;;###autoload
(defun eradio-play ()
  "Play a radio channel, do what I mean."
  (interactive)
  (eradio-stop)
  (let ((url (eradio-get-url)))
    (eradio-play-low-level url)))

(provide 'eradio)
;;; eradio.el ends here
