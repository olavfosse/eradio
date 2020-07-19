;;; radio.el --- A radio player                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Olav Fosse

;; Author: Olav Fosse <fosseolav@gmail.com>
;; Version: 0.1
;; URL: https://github.com/olav35/radio
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

;; A radio player

;;; Code:

;;;###autoload
(defvar radio-channels '(("def con - soma fm" . "https://somafm.com/defcon256.pls")
                         ("metal - soma fm" . "https://somafm.com/metal130.pls")
                         ("groove salad - soma fm" . "https://somafm.com/groovesalad256.pls")
                         ("secret agent - soma fm" . "https://www.somafm.com/secretagent.pls")
                         ("cyberia - lainon". "https://lainon.life:8000/cyberia.ogg")
                         ("cafe - lainon" . "https://lainon.life:8000/cafe.ogg")
                         ("swing - lainon" . "https://lainon.life:8000/swing.ogg")
                         ("everything - lainon" . "https://lainon.life:8000/everything.ogg"))
  "Radio's radio channels.")

(defvar radio-process nil "The process running the radio player.")

(defun radio-alist-keys (alist)
  "Get the keys from an ALIST."
  (mapcar #'car alist))

(defun radio-stop ()
  "Stop the radio player."
  (interactive) (unless (eq nil radio-process) (delete-process radio-process)))

(defun radio-play-low-level (URL)
  "Play radio channel URL in a new process."
  (setq radio-process (start-process "radio-process" nil "vlc" "--no-video" "-I" "rc" URL)))

(defun radio-get-url ()
  "Get a radio channel URL from the user."
  (let ((radio-channel (completing-read
                       "Channel: "
                       (radio-alist-keys radio-channels)
                       nil nil)))
  (or (cdr (assoc radio-channel radio-channels)) radio-channel)))

;;;###autoload
(defun radio-play ()
  "Play a radio channel, do what I mean."
  (interactive)
  (radio-stop)
  (let ((url (radio-get-url)))
    (radio-play-low-level url)))

(provide 'radio)
;;; radio.el ends here
