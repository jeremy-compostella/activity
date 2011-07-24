;;; activity.el --- Activity management.

;; Copyright (C) 2010-2011 Jérémy Compostella

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

(require 'cl)
(require 'iswitchb)

(defgroup activity nil
  "Activity management group"
  :group 'convenience)

(defstruct activity
  (name :read-only t)
  (open-hook nil :read-only t)		; Called on first activity open
  (enable-hook nil :read-only t)	; Called when switching to this activity
  (disable-hook nil :read-only t)	; Called when exiting this activity
  (buffer-filter-p (lambda (buf) (interactive) t) :read-only t))  	; Predicate for buffer filtering

(defcustom available-activities (list (make-activity :name "Default"))
  "Available activities."
  :group 'activity)

(defvar activities-wconf (make-hash-table :test 'equal))

(defvar activity-stack (cons (car available-activities) nil)
  "Current stacked activitities.")

(defvar activity-current-name
  (concat "(" (activity-name (first activity-stack)) ") ")
  "Current activity name, useful for activity-mode-line")

(defun activity-push (&optional name)
  "Push[, start] and display NAME activity."
  (unless name
    (setq name (completing-read "Activity name: " (mapcar 'activity-name available-activities))))
  (let ((new-activity (search-activity name)))
    (when new-activity
      (activity-save (first activity-stack))
      (activity-restore new-activity)
      (delq new-activity activity-stack)
      (push new-activity activity-stack)
      (setq activity-current-name (concat "(" (activity-name (first activity-stack)) ") ")))))

(defun activity-pop ()
  "Pop the current activity."
  (interactive)
  (if (> (list-length activity-stack) 1)
      (progn
	(activity-save (pop activity-stack))
	(activity-restore (first activity-stack))
	(setq activity-current-name (concat "(" (activity-name (first activity-stack)) ") ")))
    (message "No more activity.")))

(defun toggle-activity (&optional name)
  "Push NAME activity if not displayed, pop otherwise."
  (interactive)
  (unless name
    (setq name (completing-read "Activity name: " (mapcar 'activity-name available-activities))))
  (if (string= name (activity-name (first activity-stack)))
      (activity-pop)
    (activity-push name)))

(defun toggle-activity-mode-line ()
  "Toggle current activity in mode line"
  (interactive)
  (let ((frame-id-pos (memq 'mode-line-frame-identification mode-line-format)))
    (if (memq 'activity-current-name mode-line-format)
	(setcdr frame-id-pos (cddr frame-id-pos))
      (setcdr frame-id-pos (cons 'activity-current-name (cdr frame-id-pos))))))

(defun activity-set-current-as (&optional name)
  "Save current window configuration as NAME activity"
  (interactive)
  (unless name
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (let ((activity (search-activity name)))
    (when activity
      (delq activity activity-stack)
      (push activity activity-stack)
      (activity-call-hook activity 'activity-enable-hook)
      (setq activity-current-name (concat "(" (activity-name (first activity-stack)) ") ")))))

(defun activity-switchb ()
  (interactive)
  (let ((buf-p (activity-buffer-filter-p (first activity-stack))))
    (let ((iswitchb-make-buflist-hook
            (lambda () (setq iswitchb-temp-buflist (delete-if-not buf-p iswitchb-temp-buflist)))))
	(switch-to-buffer (iswitchb-read-buffer "activity-switchb: ")))))

(defun search-activity (name)
  (find name available-activities :test '(lambda (x y) (string= x (activity-name y)))))

(defun activity-reset (&optional name)
  "Reset NAME activity, it will be restarted on next push."
  (unless name
    (setq name (completing-read "Activity name: " (mapcar 'car available-activities))))
  (remhash name activities-wconf))

(defun activity-save (activity)
  (activity-call-hook activity 'activity-disable-hook)
  (puthash (activity-name activity) (current-window-configuration) activities-wconf))

(defun activity-restore (activity)
  (let ((wconf (gethash (activity-name activity) activities-wconf)))
    (if (window-configuration-p wconf)
	(progn (set-window-configuration wconf)
	       (activity-call-hook activity 'activity-enable-hook))
      (progn (activity-call-hook activity 'activity-open-hook)
	     (activity-call-hook activity 'activity-enable-hook)))))

(defun activity-call-hook (activity hook-accessor)
  (let ((func (funcall hook-accessor activity)))
    (when func (funcall func))))

(provide 'activity)
