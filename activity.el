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
  name
  (open-hook 'ignore)		    ; Called on first activity open
  (terminate-hook 'ignore)	    ; Called when closing the activity
  (enable-hook 'ignore)	; Called when switching to this activity (even at open)
  (disable-hook 'ignore)	   ; Called when exiting this activity
  (buffer-filter-p 'ignore)	   ; Predicate for buffer filtering
  ;; Internal data
  (buffer-list '())   ; Complementary buffer list for buffer filtering
  (wconf nil))	      ; Activity saved window configuration

(defcustom available-activities (list (make-activity :name "Default"))
  "Available activities."
  :group 'activity)

(defvar activity-stack (cons (car available-activities) nil)
  "Current stacked activitities.")

(defun current-activity () (first activity-stack))

(defcustom activity-mode-line-format "(%s) "
  "Activity mode-line format")
  
(defvar activity-current-name (format activity-mode-line-format (activity-name (current-activity)))
  "Current activity name, useful for activity-mode-line")

(defun activity-push (&optional name)
  "Push[, start] and display NAME activity."
  (unless name
    (setq name (completing-read "Activity name: " (mapcar 'activity-name available-activities))))
  (let ((new-activity (search-activity name)))
    (when new-activity
      (activity-save (current-activity))
      (delq new-activity activity-stack)
      (push new-activity activity-stack)
      (activity-update-mode-line)
      (activity-restore new-activity))))

(defun activity-pop ()
  "Pop the current activity."
  (interactive)
  (if (> (list-length activity-stack) 1)
      (progn
	(activity-save (pop activity-stack))
	(activity-restore (current-activity))
	(activity-update-mode-line))
    (message "No more activity.")))

(defun toggle-activity (&optional name)
  "Push NAME activity if not displayed, pop otherwise."
  (interactive)
  (unless name
    (setq name (completing-read "Activity name: " (mapcar 'activity-name available-activities))))
  (if (string= name (activity-name (current-activity)))
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
      (funcall (activity-enable-hook activity))
      (activity-update-mode-line))))

(defun activity-update-mode-line ()
  (setq activity-current-name (format activity-mode-line-format (activity-name (current-activity)))))
  
(defun activity-buffer-p (buf-name)
  (let ((activity (current-activity)))
    (or (funcall (activity-buffer-filter-p activity) buf-name)
	(find buf-name (activity-buffer-list activity) :test 'string=))))

(defun activity-switchb ()
  "Switch to another buffer. Buffer list filtered by activity."
  (interactive)
  (let ((buf-p (activity-buffer-filter-p (current-activity))))
    (let ((iswitchb-make-buflist-hook
            (lambda () (setq iswitchb-temp-buflist
			     (delete-if-not 'activity-buffer-p iswitchb-temp-buflist)))))
	(switch-to-buffer (iswitchb-read-buffer "activity-switchb: ")))))

(defun new-activity (name key)
  (interactive (list (read-string "Activity name: ")
		     (read-key-sequence "Key sequence: ")))
  (if (search-activity name)
    (error "\"%s\" activity already exists." name)
    (progn
      (add-to-list 'available-activities (make-activity :name name))
      (global-set-key key (lambda () (interactive) (toggle-activity name)))
      (toggle-activity name))))

(defun search-activity (name)
  (find name available-activities :test '(lambda (x y) (string= x (activity-name y)))))

(defun activity-save (activity)
  (funcall (activity-disable-hook activity))
  (setf (activity-wconf activity) (current-window-configuration)))

(defun activity-restore (activity)
  (let ((wconf (activity-wconf activity)))
    (if (window-configuration-p wconf)
	(progn (set-window-configuration wconf)
	       (funcall (activity-enable-hook activity)))
      (progn (funcall (activity-open-hook activity))
	     (funcall (activity-enable-hook activity))))))

(defun activity-add-buffer ()
  (interactive)
  (flet ((add-buffer (window)
		     (let ((buffer (window-buffer window)))     
		       (unless (activity-buffer-p (buffer-name buffer))
			 (push (buffer-name buffer) (activity-buffer-list (current-activity)))))))
    (walk-windows 'add-buffer)))

(defun activity-remove-buffer ()
  (interactive)
  (delq (buffer-name (current-buffer)) (activity-buffer-list (current-activity))))

(add-to-list 'kill-buffer-hook 'activity-remove-buffer)
(add-to-list 'window-configuration-change-hook 'activity-add-buffer)

(provide 'activity)
