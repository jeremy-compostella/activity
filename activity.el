;;; activity.el --- Activity management.

;; Copyright (C) 2010 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Keywords: emacs activity window configuration

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;;
;;
;;; Code:

(defgroup activity nil
  "Activity management group"
  :group 'comm)

(defcustom available-activities '(("default" 110 nil))
  "Available activities."
  :group 'activity)

(defvar activity-stack '(("default" 110 nil))
  "Current stacked activitities.")

(defun activity-push (&optional name)
  "Push[, start] and display \"name\" activity."
  (when (not name)
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (let ((new-activity (search-activity name)))
    (if (not (eq nil new-activity))
	(progn
	  (activity-save (first activity-stack))
	  (activity-restore new-activity)
	  (delq new-activity activity-stack)
	  (push new-activity activity-stack))
      (message (concat name " activity not found.")))))

(defun activity-pop ()
  "Pop the current activity."
  (interactive)
  (if (> (list-length activity-stack) 1)
      (progn
	(activity-save (pop activity-stack))
	(activity-restore (first activity-stack)))
    (message "No more activity.")))

(defun toggle-activity (&optional name)
  "Push \"name\" activity if not displayed, pop otherwise."
  (interactive)
  (when (not name)
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (if (string= name (car (first activity-stack)))
      (activity-pop)
    (activity-push name)))

(defun activity-reset (&optional name)
  "Reset \"name\" activity, it will be restarted on next push."
  (when (not name)
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (set-register (nth 1 (search-activity name)) nil))

(defun activity-show-stack ()
  (interactive)
  (let ((msg ""))
    (dolist (activity activity-stack)
      (setq msg (concat msg (car activity) " ")))
    (message msg)))

(defun search-activity (name)
  (let ((found-activity))
    (dolist (activity available-activities)
      (when (string= (car activity) name)
	(setq found-activity activity)))
    found-activity))

(defun activity-save (activity)
  (window-configuration-to-register (nth 1 activity)))

(defun activity-restore (activity)
  (let ((val (get-register (nth 1 activity))))
    (if (and (consp val) (window-configuration-p (car val)))
	(jump-to-register (nth 1 activity))
      (let ((func (nth 2 activity)))
	(when (not (eq nil func))
	  (funcall func))))))

(provide 'activity)
