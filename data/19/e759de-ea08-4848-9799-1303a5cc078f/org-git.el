;;; org-git.el - Support for links to specific git versions in Org
;; 

(require 'org)

(org-add-link-type "git" 'org-git-open)
(add-hook 'org-store-link-functions 'org-git-store-link)

(defcustom org-git-shell-command "git"
  "The git shell command to be used to display a man page."
  :group 'org-link
  :type 'string)


(defun org-git-open (revision filename)
  "Visit the manpage on PATH.
     PATH should be a topic that can be thrown at the man command."
  (funcall org-man-command path))

(defun org-git-store-link ()
  "Store a link to a manpage."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link
    (let* ((page (org-man-get-page-name))
           (link (concat "man:" page))
           (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "git"
       :link link
       :description description))))

(defun org-git-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

(provide 'org-git)

;;; org-git.el ends here
