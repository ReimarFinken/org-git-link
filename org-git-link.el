;;; org-git-link.el --- Provide org links to specific file version

;; Copyright (C) 2009  Reimar Finken

;; Author: Reimar Finken <reimar.finken@gmx.de>
;; Keywords: files, calendar, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distaributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'org)
(defcustom org-git-program "git"
  "Name of the git executable used to follow git links." 
  :type '(string)
  :group 'org)

;; org link functions 
;; bare git link
(org-add-link-type "gitbare" 'org-gitbare-open)
(defun org-gitbare-open (str)
  (let* ((strlist (org-git-split-string str))
         (gitdir (first strlist))
         (object (second strlist))
         (buffer (get-buffer-create (concat "*" object "*"))))
    (set-buffer buffer) 
    (let ((inhibit-read-only t))
      (erase-buffer)
      (org-git-show gitdir object buffer))
    (setq buffer-read-only t)))


;; user friendly link
(org-add-link-type "git" 'org-git-open)

;; extracting the search string


;; Utility functions (file names etc)

;; splitting the link string 

;; Both link open functions are called with a string of
;; consisting of two parts separated by a double colon (::).
(defun org-git-split-string (str)
  "Given a string of the form \"str1::str2\", return a list of
  two substrings \'(\"str1\" \"str2\")" 
  (split-string str "::"))

;; finding the file name part of a commit
(defun org-git-link-filename (str)
  "Given an object description (see the man page of
  git-rev-parse), return the nondirectory part of the referenced
  filename, if it can be extracted. Otherwise, return a valid
  filename."
  (let* ((match (and (string-match "[^:]+$" str)
                     (match-string 0 str)))
         (filename (and match (file-name-nondirectory match)))) ;extract the final part without slash
    filename))

;; calling git
(defun org-git-show (gitdir object buffer)
  "Show the output of git --git-dir=gidir show object in buffer."
  (unless 
      (zerop (call-process org-git-program nil buffer t
                           "--no-pager" (concat "--git-dir=" gitdir) "show" object))
    (error "git error: %s " (save-excursion (set-buffer buffer) 
                                            (buffer-string)))))

(provide 'org-git-link)
;;; org-git-link.el ends here

