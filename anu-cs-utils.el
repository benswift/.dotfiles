;;; anu-cs-utils.el --- utilities for running an ANU COMP course from Emacs -*- lexical-binding: t -*-

;; Author: Ben Swift
;; Maintainer: Ben Swift
;; Version: 1.0.0
;; Package-Requires: (dependencies)


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; you need to set the following config variables in your .emacs (or .spacemacs, init.el, or wherever)

;; for example, on Ben's machine they are:

;; (defconst anu-cs-jekyll-website-directory  (expand-file-name "~/Documents/teaching/comp1720-2019/website"))
;; (defconst anu-cs-lucy-directory  (expand-file-name "~/Documents/teaching/comp1720-2019/lucy"))

;; then, you should source this file with e.g.

;; (load-file (expand-file-name "~/.dotfiles/anu-cs-utils.el"))

;;; Code:

;;;;;;;;;;;;;;
;; teaching ;;
;;;;;;;;;;;;;;

;; this file includes the data specific to the course I'm currently teaching
;; (not included in dotfiles repo for obvious reasons)
(load-file (expand-file-name "~/Documents/teaching/comp1720-2019/convenors/comp1720.el"))

(defconst anu-cs-uid-regex "[uU][0-9]\\{7\\}")
(defvar anu-cs-uid-input-history nil
  "variable for holding the history of interactive commands which require a UID")

(defun anu-cs-get-uid-at-point ()
  (interactive)
  (let ((tap (thing-at-point 'word :no-properties)))
	(if (and tap (s-matches? anu-cs-uid-regex tap))
		(downcase tap)
	  nil)))

(defun anu-cs-is-uid? (uid)
  (s-matches? anu-cs-uid-regex uid))

(defun anu-cs-validate-uid (uid)
  (if (s-matches? anu-cs-uid-regex uid)
	  uid
	(error "user-error: bad UID %s" uid)))

;; csv header: uid,name,status,mark,grade,degree,group,special,course
(defvar anu-cs-student-data nil)

(defconst anu-cs-student-data-columns
  '("uid" "name" "status" "mark" "grade" "degree" "group" "special" "course")
  "columns in students.csv")

(defun anu-cs-lucy-sync ()
  (interactive)
  (setq anu-cs-student-data
		(cdr (read-csv (format "%s/data/students.csv" anu-cs-lucy-directory) nil)))
  (message "lucy: succesfully synced %d students" (length anu-cs-student-data)))

(defun anu-cs-get-student-data (uid)
  (or (--first (string= uid (car it)) anu-cs-student-data)
	  (error "Error: could not find student with uid %s" uid)))

(defun anu-cs-student-firstname (uid)
  (or (cdr-safe (assoc uid anu-cs-firstname-alist))
	  (car (s-split-words (anu-cs-student-info uid "name")))))

(defun anu-cs-student-info (uid field)
  (if (-contains? anu-cs-student-data-columns field)
	  (nth (-elem-index field anu-cs-student-data-columns)
		   (anu-cs-get-student-data uid))
	(error "Error: no field \"%s\" in anu-cs-student-data" uid)))

(defun anu-cs-tutors-for-student (uid)
  (let ((group (anu-cs-student-info uid "group")))
	(and (not (s-blank? group))
		 (cdr (--first (string= group (car it)) anu-cs-tutor-data)))))

(defun anu-cs-tutor-cc-string-for-student (uid)
  (let ((tutors (anu-cs-tutors-for-student uid)))
	(s-join "," (--map (format "%s@anu.edu.au" (cdr (assoc it anu-cs-tutor-uid-alist))) tutors))))

(defun anu-cs-tutor-cc-string-for-group (group)
  (let ((tutors (cdr (assoc group anu-cs-tutor-data))))
	(s-join "," (--map (format "%s@anu.edu.au" (cdr (assoc it anu-cs-tutor-uid-alist))) tutors))))

(defun anu-cs-pretty-format-student (uid)
  (format
   "uid: %s
firstname: %s
full name: %s
group: %s
course: %s
degree: %s
tutors: %s"
   uid
   (anu-cs-student-firstname uid)
   (anu-cs-student-info uid "name")
   (anu-cs-student-info uid "group")
   (anu-cs-student-info uid "course")
   (anu-cs-student-info uid "degree")
   (s-join " " (anu-cs-tutors-for-student uid))))

;; helpers for interactive use

(defun anu-cs-completing-read-uid ()
  (completing-read "uid: "
				   (-map #'car anu-cs-student-data)
				   nil
				   :require-match
				   (anu-cs-get-uid-at-point)))

(defun anu-cs-completing-read-name ()
  (completing-read "name: "
				   (-map #'cadr anu-cs-student-data)
				   nil
				   :require-match))

(defun anu-cs-completing-read-uid-dwim ()
  (let ((uid-or-name (or (anu-cs-get-uid-at-point)
						 (anu-cs-completing-read-name))))
	(if (s-match anu-cs-uid-regex uid-or-name)
		uid-or-name
	  (car (--first (string= (cadr it) uid-or-name) anu-cs-student-data)))))

(defun anu-cs-completing-read-field ()
  (completing-read "field: "
				   anu-cs-student-data-columns
				   nil
				   :require-match))

(defun anu-cs-completing-read-project ()
  (completing-read "project: "
				   anu-cs-project-list
				   nil
				   :require-match))

(defun anu-cs-completing-read-group ()
  (completing-read "group: "
				   (-map #'car anu-cs-tutor-data)
				   nil
				   :require-match))

(defun anu-cs-print-student (uid)
  (interactive
   (list (anu-cs-completing-read-uid-dwim)))
  ;; also add uid to kill ring
  (kill-new uid)
  (message (anu-cs-pretty-format-student uid)))

(defun fais-visit-student (uid)
  (interactive
   (list (anu-cs-completing-read-uid-dwim)))
  (anu-cs-validate-uid uid)
  (ffap (concat "https://cs.anu.edu.au/fais/staff/Students.php?StudID=" (substring uid 1))))

(defun discourse-visit-student (uid)
  (interactive
   (list (anu-cs-completing-read-uid-dwim)))
  (anu-cs-validate-uid uid)
  (ffap (format "https://discourse.cecs.anu.edu.au/u/%s/" uid)))

(defun anu-cs-visit-gitlab (uid project)
  (interactive
   (list (anu-cs-completing-read-uid-dwim)
		 (anu-cs-completing-read-project)))
  (if (s-suffix? "-marking-origin" project)
	  (ffap (format "https://gitlab.cecs.anu.edu.au/%s/%s-%s/" anu-cs-gitlab-marker-username uid (s-chop-suffix "-marking-origin" project)))
	(ffap (format "https://gitlab.cecs.anu.edu.au/%s/%s/" uid project))))

(defun anu-cs-student-dir (uid project)
  (expand-file-name (format "%s/submissions/%s/%s/%s"
							anu-cs-lucy-directory project
							(anu-cs-student-info uid "group")
							uid)))

(defun anu-cs-visit-submission (uid project)
  (interactive
   (list (anu-cs-completing-read-uid-dwim)
		 (anu-cs-completing-read-project)))
  (let ((submission-dir (anu-cs-student-dir uid project)))
	(if (f-dir? submission-dir)
		(dired submission-dir)
	  (error "no %s submission found for %s" project uid))))

;; all the emails

(defun anu-cs-email-student-copy-tutors (uid)
  (interactive
   (list (anu-cs-completing-read-uid-dwim)))
  (mu4e-compose-new)
  (message-add-header (format "Cc: %s" (anu-cs-tutor-cc-string-for-student uid)))
  (message-add-header (format "To: %s@anu.edu.au" uid)))

(defun anu-cs-cc-tutors (group)
  (interactive
   (list (let ((uid-or-name-or-group
				(or (anu-cs-get-uid-at-point)
					(completing-read "name: "
									 (append (-map #'cadr anu-cs-student-data)
											 (-map #'car anu-cs-tutor-data))
									 nil
									 :require-match))))
		   (cond
			;; if it's a uid
			((s-match anu-cs-uid-regex uid-or-name-or-group)
			 (anu-cs-student-info uid-or-name-or-group "group"))
			;; if it's a group name
			((member uid-or-name-or-group (-map #'car anu-cs-tutor-data))
			 uid-or-name-or-group)
			;; if it's a student name
			(t (anu-cs-student-info
				(car (--first (string= (cadr it) uid-or-name-or-group) anu-cs-student-data))
				"group"))))))
  (message-add-header (format "Cc: %s" (anu-cs-tutor-cc-string-for-group group))))

(defun anu-cs-email-tutors (group)
  (interactive
   (list (anu-cs-completing-read-group)))
  (mu4e-compose-new)
  (message-add-header (format "To: %s" (anu-cs-tutor-cc-string-for-student uid))))

;; keybindings
(spacemacs/set-leader-keys "op" 'anu-cs-print-student)
(spacemacs/set-leader-keys "of" 'fais-visit-student)
(spacemacs/set-leader-keys "og" 'anu-cs-visit-gitlab)

;; now, run the lucy sync...
(anu-cs-lucy-sync)

(provide 'anu-cs-utils)

;;; anu-cs-utils.el ends here
