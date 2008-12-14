;;; impress.el --- group, process, and publish files

;; Copyright (C) 2008  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: tools, files

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

;; DONE groups
;; DONE paths
;; DONE timestamps
;; TODO projects
;; TODO formats
;; TODO blogging
;; TODO processors
;; TODO preamble/postamble div control

;;; Code:

(require 'cl)
(require 'org)

;;; Timestamps

(defvar impress-timestamp-directory "~/.impress-timestamps")

(defun impress-timestamp-filename (filename path)
  (concat (file-name-as-directory impress-timestamp-directory)
	  "impress-timestamp-" (impress-path-name path) "-"
	  (sha1 filename)))

(defun impress-up-to-date-p (filename path)
  (let ((timestamp-file (impress-timestamp-filename filename path)))
    (if (file-exists-p impress-timestamp-directory)
	(file-newer-than-file-p filename timestamp-file)
	;; create the timestamp dir
	(prog1 t
	  (make-directory impress-timestamp-directory)))))

(defun impress-update-timestamp (filename path)
  (let ((timestamp-file (impress-timestamp-filename filename path)))
    (when (not (file-exists-p timestamp-file))
      (with-temp-buffer
	(make-directory (file-name-directory timestamp-file) t)
	(write-file timestamp-file)))
    (set-file-times timestamp-file)))

;;; Groups

;; A group is a set of files that are published together.

(defstruct impress-group 
  name description files base-directory format include exclude)

(defun impress-match-regexp-or-list (filename regexp-or-list)
  (if (null regexp-or-list)
      nil
      (etypecase regexp-or-list
	(string (string-match regexp-or-list filename))
	(list (member filename regexp-or-list)))))

(defun impress-filter-files (files regexp-or-list)
  (labels ((match (filename)
	     (impress-match-regexp-or-list filename regexp-or-list)))
    (remove-if #'match files)))

(defun impress-get-group-files (group)
  (let ((dir (impress-group-base-directory group)))
    (labels ((expand (filename)
	       (expand-file-name filename dir)))
      (let* ((include (impress-group-include group))
	     (files (etypecase include
		      (string (directory-files dir nil include))
		      (list (mapcar #'expand include)))))
	(mapcar #'expand 
		(remove-duplicates 
		 (impress-filter-files files (impress-group-exclude group))
		 :test 'equal))))))

(defun impress-load-group-files (group)
  (setf (impress-group-files group)
	(impress-get-group-files group)))

(defun impress-create-group (&rest forms)
  (let ((group (apply #'make-impress-group forms)))
    (impress-load-group-files group)))

;;; Paths

;; A path transforms files from a group into their destination
;; directory and format.

(defstruct impress-path 
  name description source-format destination-format 
  destination-directory processor publisher options)

(defun impress-create-path (&rest forms)
  (apply #'make-impress-path forms))

;;; Publishing a group to a path

(defun impress-publish-org-to-html (filename destination-dir &optional options)
  (find-file filename)
  (org-export-as-html nil :hidden options nil nil destination-dir))

(defun impress-publish-group-to-path (group path)
  (let ((files (impress-group-files group))
	(destination (impress-path-destination-directory path))
	(options (impress-path-options path))
	(publisher (impress-path-publisher path)))
    (dolist (file files)
      (unless (impress-up-to-date-p file path)
	(save-window-excursion
	  (funcall publisher file destination options)
	  (impress-update-timestamp file path))))))

;;; Projects

;; A project consists of a set of groups and a set of paths used to
;; publish files from the groups. Both sets are hash tables with
;; string keys.

(defstruct impress-project name groups paths)


(provide 'impress)
;;; impress.el ends here
