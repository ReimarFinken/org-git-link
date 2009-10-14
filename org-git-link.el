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
         (object (second strlist)))
    (org-git-open-file-internal gitdir object)))

(defun org-git-open-file-internal (gitdir object)
  (let* ((tmpdir (make-temp-file "org-git" t))
         (filename (org-git-link-filename object))
         (tmpfile (expand-file-name filename tmpdir)))
    (with-temp-file tmpfile 
      (org-git-show gitdir object (current-buffer)))
    (org-open-file tmpfile)))

;; user friendly link
(org-add-link-type "git" 'org-git-open)
(defun org-git-open (str)
  (let* ((strlist (org-git-split-string str))
         (filepath (first strlist))
         (commit (second strlist))
         (dirlist (org-git-find-gitdir filepath))
         (gitdir (first dirlist))
         (relpath (second dirlist)))
    (org-git-open-file-internal gitdir (concat commit ":" relpath))))


;; Utility functions (file names etc)

(defun org-git-split-dirpath (dirpath)
  "Given a directory name, return '(dirname basname)"
  (let ((dirname (file-name-directory (directory-file-name dirpath)))
        (basename (file-name-nondirectory (directory-file-name dirpath))))
    (list dirname basename)))

;; finding the git directory
(defun org-git-find-gitdir (path)
  "Given a file (not necessarily existing) file path, return the
  a pair (gitdir relpath), where gitdir is the path to the first
  .git subdirectory found updstream and relpath is the rest of
  the path. Example: (org-git-find-gitdir
  \"~/gitrepos/foo/bar.txt\") returns '(\"/home/user/gitrepos/.git\" \"foo/bar.txt\")"
  (let ((dir (file-name-directory path))
        (relpath (file-name-nondirectory path)))
    (while (not (file-exists-p (expand-file-name ".git" dir)))
      (let ((dirlist (org-git-split-dirpath dir)))
        (setq dir (first dirlist)
              relpath (concat (file-name-as-directory (second dirlist)) relpath))))
    (list (expand-file-name ".git" dir) relpath)))

;; splitting the link string 

;; Both link open functions are called with a string of
;; consisting of two parts separated by a double colon (::).
(defun org-git-split-string (str)
  "Given a string of the form \"str1::str2\", return a list of
  two substrings \'(\"str1\" \"str2\"). If the double colon is mising, take str2 to be the empty string." 
  (let ((strlist (split-string str "::")))
    (cond ((= 1 (length strlist))
           (list (car strlist) ""))
          ((= 2 (length strlist))
           strlist)
          (t (error "org-git-split-string: only one :: allowed: %s" str)))))

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

