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

;;; Documentation: http://www.jerryland.fr/software/activity.html
;;; Code:

(defgroup activity nil
  "Activity management group"
  :group 'convenience)

(defcustom available-activities '(("default" nil))
  "Available activities."
  :group 'activity)

(defvar activities-wconf  (make-hash-table :test 'equal))

(defvar activity-stack (cons (car available-activities) nil)
  "Current stacked activitities.")

(defvar activity-current-name
  (concat "(" (car (first activity-stack)) ") ")
  "Current activity name, useful for activity-mode-line")

(defun activity-push (&optional name)
  "Push[, start] and display NAME activity."
  (when (not name)
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (let ((new-activity (search-activity name)))
    (when (not (eq nil new-activity))
      (activity-save (first activity-stack))
      (activity-restore new-activity)
      (delq new-activity activity-stack)
      (push new-activity activity-stack)
      (setq activity-current-name (concat "(" (car (first activity-stack)) ") ")))))

(defun activity-pop ()
  "Pop the current activity."
  (interactive)
  (if (> (list-length activity-stack) 1)
      (progn
	(activity-save (pop activity-stack))
	(activity-restore (first activity-stack))
	(setq activity-current-name (concat "(" (car (first activity-stack)) ") ")))
    (message "No more activity.")))

(defun toggle-activity (&optional name)
  "Push NAME activity if not displayed, pop otherwise."
  (interactive)
  (when (not name)
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (if (string= name (car (first activity-stack)))
      (activity-pop)
    (activity-push name)))

(defun toggle-activity-mode-line ()
  "Toggle current activity in mode line"
  (interactive)
  (let ((frame-id-pos (memq 'mode-line-frame-identification mode-line-format)))
    (if (memq 'activity-current-name mode-line-format)
	(setcdr frame-id-pos (cddr frame-id-pos))
      (setcdr frame-id-pos (cons 'activity-current-name (cdr frame-id-pos))))))

(defun search-activity (name)
  (let ((found-activity))
    (dolist (activity available-activities)
      (when (string= (car activity) name)
	(setq found-activity activity)))
    found-activity))

(defun activity-reset (&optional name)
  "Reset NAME activity, it will be restarted on next push."
  (when (not name)
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (remhash name activities-wconf))

(defun activity-save (activity)
  (puthash (car activity) (current-window-configuration) activities-wconf))

(defun activity-restore (activity)
  (let ((wconf (gethash (car activity) activities-wconf)))
    (if (window-configuration-p wconf)
	(set-window-configuration wconf)
      (let ((func (nth 1 activity)))
	(when (not (eq nil func))
	  (funcall func))))))

(provide 'activity)
