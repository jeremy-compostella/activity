;;; activity.el --- Activity management (named window configuration).

;; Copyright (C) 2010-2012 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 0.1
;; Keywords: activity window configuration

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

;; This package provides an `activity' management. An activity is
;; mainly compound with a name (like "Mail" activity for exemple) and
;; a window configuration. When an activity is opened for the first
;; time its `open-hook' is called. When switching from an activity to
;; another the `enable-hook' and `disable-hook' are respectively
;; called on activity or deactivating the activity.

;; The runned activities are arranged in a stack so you can pop the
;; current activity (see `activity-pop') to go to previous one or push
;; a new one. If you push an activity (see `activity-push') which was
;; in the activities stack it goes to the top of the activities
;; stack. The common way to enable an activity consist in call the
;; `toggle-activity' with the name of the activity as argument.

;; The activity management provides a way to work on multiple project
;; or task easily by offering a simple way to manage the buffer list
;; when switching to another buffer. When the `buffer-filter-p' is not
;; provided for an activity, all the available buffers are assumed to
;; be associated to this activity. When defined, only the buffer which
;; which name is accepeted by the `buffer-filter-p' hook is accepted
;; or the buffer has been previously visited is provided as a
;; potential buffer switch.

;; For exemple I define my linux kernel development activity as
;; follow:

;; (add-to-list 'available-activities
;; 	     (make-activity :name "LinuxDev"
;; 			    :open-hook (lambda ()
;; 					 (delete-other-windows)
;; 					 (find-file linux-kernel-path))
;; 			    :buffer-filter-p (lambda (buf-name)
;; 					       (with-current-buffer buf-name
;; 						 (string-prefix-p linux-kernel-path buffer-file-name)))
;; 			    :enable-hook (lambda ()
;; 					   (setq compile-command linux-kernel-compile-command))
;; 			    :disable-hook (lambda ()
;; 					    (setq compile-command nil))))

;; When I call the `activity-switch-to-buffer' it will only propose me
;; the file which are in the linux kernel path or that have been
;; previoulsy visited while the "LinuxDev" activity was enabled.

;;; Code:

(require 'cl)

(defgroup activity nil
  "Activity management group"
  :group 'convenience)

(defstruct activity
  name
  (open-hook 'ignore)		    ; Called on first activity open
  (enable-hook 'ignore)	; Called when switching to this activity (even at open)
  (disable-hook 'ignore)	   ; Called when exiting this activity
  (buffer-filter-p 'ignore)	   ; Predicate for buffer filtering
  ;; Internal data
  (buffer-list '())   ; Complementary buffer list for buffer filtering
  (wconf nil))	      ; Activity saved window configuration

(defcustom available-activities (list (make-activity :name "Default"))
  "Available activities list.
Add your personal activities in this list. "
  :group 'activity)

(defcustom toggle-activity-hooks nil
  "Hooks run when swithing to another activity."
  :group 'activity)

(defvar activity-stack (cons (car available-activities) nil)
  "Currently stacked activitities.")

(defun current-activity () (first activity-stack))

(defcustom activity-mode-line-format "(%s) "
  "Activity mode-line format"
  :group 'activity)
  
(defvar activity-current-name (format activity-mode-line-format (activity-name (current-activity)))
  "Current activity name, used by `activity-update-mode-line'.")

(defun activity-push (&optional name)
  "Push[, start] and enable NAME activity.
If NAME activity is the current one, it calls `activity-pop'. If
NAME activity is in the `activity-stack', it will enable it and
place it on top of the `activity-stack'.
Otherwise, the NAME activity is started."
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
  (interactive (list (completing-read "Activity name: " (mapcar 'activity-name available-activities))))
  (if (string= name (activity-name (current-activity)))
      (activity-pop)
    (activity-push name))
  (run-hooks 'toggle-activity-hooks))

(defun toggle-activity-mode-line ()
  "Toggle current activity in mode line"
  (interactive)
  (let ((frame-id-pos (memq 'mode-line-frame-identification mode-line-format)))
    (if (memq 'activity-current-name mode-line-format)
	(setcdr frame-id-pos (cddr frame-id-pos))
      (setcdr frame-id-pos (cons 'activity-current-name (cdr frame-id-pos))))))

(defun activity-set-current-as (&optional name)
  "Save current window configuration as NAME activity"
  (interactive (list (completing-read "Activity name: " (mapcar 'activity-name available-activities))))
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
    (or (not (activity-buffer-filter-p activity))
	(funcall (activity-buffer-filter-p activity) buf-name)
	(find buf-name (activity-buffer-list activity) :test 'string=))))

(defun activity-iswitch-to-buffer ()
  (let ((iswitchb-make-buflist-hook
	 (lambda () (setq iswitchb-temp-buflist
			  (delete-if-not 'activity-buffer-p iswitchb-temp-buflist)))))
    (switch-to-buffer (iswitchb-read-buffer "activity-switchb: "))))

(defun activity-ido-switch-to-buffer ()
  (let ((ido-ignore-buffers (delete-if 'activity-buffer-p (mapcar 'buffer-name (buffer-list)))))
    (ido-switch-buffer)))

(defun activity-switch-to-buffer ()
  "Switch to another buffer. Buffer list filtered by activity."
  (interactive)
  (cond (iswitchb-mode (activity-iswitch-to-buffer ))
	((or (eq ido-mode 'buffer) (eq ido-mode 'both)) (activity-ido-switch-to-buffer ))
	(t (completing-read (format "Switch to buffer (default %s): " (other-buffer (current-buffer)))
			    (delete-if-not 'activity-buffer-p (mapcar 'buffer-name (buffer-list)))))))

(defun new-activity (name key)
  (interactive (list (read-string "Activity name: ")
		     (read-key-sequence "Key sequence: ")))
  (if (search-activity name)
    (error "\"%s\" activity already exists." name)
    (when (or (not (global-key-binding key))
	      (y-or-n-p "This key sequence is already in use. Overwrite it ? "))
      (add-to-list 'available-activities (make-activity :name name))
      (global-set-key key `(lambda () (interactive) (toggle-activity ,name)))
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

;;;###autoload
(add-to-list 'kill-buffer-hook 'activity-remove-buffer)
(add-to-list 'window-configuration-change-hook 'activity-add-buffer)

(provide 'activity)
;;; activity.el ends here
