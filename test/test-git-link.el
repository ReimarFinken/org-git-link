
;; test-git-link.el -- test ../org-git-link.el
(require 'cl)                           ;for flet

;;; stolen from cedet test suite
(defvar git-test-src-dir
  (let ((dir (file-name-directory
              (or load-file-name (buffer-file-name)))))
    (add-to-list 'load-path dir)
    (add-to-list 'load-path (expand-file-name ".." dir))
    dir)
  "Src dir to org-git testing suite.")

(require 'org-git-link)		; after .. has been added to load-path
(require 'el-expectations)	; after . has been added to load-path

(setq inhibit-splash-screen t)		; for batch mode


;; test org link types
;; org-open-link always returns nil, we therefore need to test the side effects. We do this by throwing the string and 
(expectations 
  (desc "org link types")
  (expect (error-message "~/foo/.git::brabranch:baz.txt")
    (flet ((org-gitbare-open (str)
                             (error str)))
      (org-open-link-from-string "[[gitbare:~/foo/.git::brabranch:baz.txt]]")))
  (expect (error-message "~/foo/bar/baz.txt::brabranch")
    (flet ((org-git-open (str)
                         (error str)))
      (org-open-link-from-string "[[git:~/foo/bar/baz.txt::brabranch]]")))
  (expect "baz\n"
    (org-open-link-from-string "[[gitbare:../.git::foobarbaztxt:test/testgitrepos/foo/bar/baz.txt]]")
    (set-buffer "*foobarbaztxt:test/testgitrepos/foo/bar/baz.txt*")
    (buffer-string))
  ;; testing whether two links only loads file once
  (expect "a\n"
    (org-open-link-from-string "[[gitbare:../.git::firstlevelfiles:test/testgitrepos/a.txt]]")
    (org-open-link-from-string "[[gitbare:../.git::firstlevelfiles:test/testgitrepos/a.txt]]")
    (set-buffer "*firstlevelfiles:test/testgitrepos/a.txt*")
    (buffer-string))
  (desc "Utility functions")
  (expect '("str1" "str2")
    (org-git-split-string "str1::str2"))
  (desc "Git functions")
  (expect "a\n" 
    (with-temp-buffer 
      (let ((gitdir (file-name-as-directory (expand-file-name "../.git" git-test-src-dir)))
            (object "firstlevelfiles:test/testgitrepos/a.txt") 
            (buffer (current-buffer))) 
        (org-git-show gitdir object buffer)
        (buffer-string)))) 
  (expect "baz\n" 
    (with-temp-buffer 
      (let ((gitdir (file-name-as-directory (expand-file-name "../.git" git-test-src-dir))) 
            (object "foobarbaztxt:test/testgitrepos/foo/bar/baz.txt") 
            (buffer (current-buffer))) 
        (org-git-show gitdir object buffer)
        (buffer-string))))
  (expect (error) 
    (with-temp-buffer 
      (let ((gitdir (file-name-as-directory (expand-file-name "../.git" git-test-src-dir)))
            (object "deletebaztxt:test/testgitrepos/foo/bar/baz.txt") 
            (buffer (current-buffer))) 
        (org-git-show gitdir object buffer)
        (buffer-string)))))

(expectations-execute)

