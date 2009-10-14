
;; test-git-link.el -- test ../org-git-link.el
(require 'cl)                           ;for flet and loop

;;; stolen from cedet test suite
(defvar git-test-src-dir
  (let ((dir (file-name-directory
              (or load-file-name (buffer-file-name)))))
    (add-to-list 'load-path dir)
    (add-to-list 'load-path (expand-file-name ".." dir))
    dir)
  "Src dir to org-git testing suite.")

(defvar gitdir (expand-file-name "../.git")
  "Path to the git directory")          ; to prevent default-directory to change between calls

(require 'org-git-link)		; after .. has been added to load-path
(require 'el-expectations)	; after . has been added to load-path

(setq inhibit-splash-screen t)		; for batch mode

(defun git-test-count-buffers (filename)
  "Returns the number of buffers visiting a certain filename."
  (loop for buf in (buffer-list)
      for file = (buffer-file-name buf)
      count (and file (string= filename (file-name-nondirectory file)))))

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
  (desc "gitbare link")
  (expect "baz\n"
    (org-open-link-from-string (concat "[[gitbare:" gitdir "::foobarbaztxt:test/testgitrepos/foo/bar/baz.txt]]"))
    (set-buffer "baz.txt")
    (buffer-string))
  (expect t
    (org-open-link-from-string (concat "[[gitbare:" gitdir "::foobarbaztxt:test/testgitrepos/foo/bar/baz.txt]]"))
    (set-buffer "baz.txt")
    buffer-read-only)
  (desc "testing whether two links only loads file once")
  (expect "a\n"
    (org-open-link-from-string (concat "[[gitbare:" gitdir "::firstlevelfiles:test/testgitrepos/a.txt]]"))
    (org-open-link-from-string (concat "[[gitbare:" gitdir "::firstlevelfiles:test/testgitrepos/a.txt]]"))
    (set-buffer "a.txt")
    (buffer-string))
  (desc "git link")
  (expect "baz\n"
    (org-open-link-from-string (concat "[[git:" git-test-src-dir "testgitrepos/foo/bar/baz.txt::foobarbaztxt]]"))
    (set-buffer "baz.txt")
    (buffer-string))
  (expect "b\n"
    (org-open-link-from-string (concat "[[git:" git-test-src-dir "testgitrepos/b.txt]]"))
    (set-buffer "b.txt")
    (buffer-string))
  (desc "testing whether two links only loads file once")
  (expect "a\n"
    (org-open-link-from-string (concat "[[git:" git-test-src-dir "testgitrepos/a.txt::firstlevelfiles]]"))
    (org-open-link-from-string (concat "[[git:" git-test-src-dir "testgitrepos/a.txt::firstlevelfiles]]"))
    (set-buffer "a.txt")
    (buffer-string))
  (expect 1
    (org-open-link-from-string (concat "[[git:" git-test-src-dir "testgitrepos/a.txt::firstlevelfiles]]"))
    (org-open-link-from-string (concat "[[git:" git-test-src-dir "testgitrepos/a.txt::foobarbaztxt]]"))
    (git-test-count-buffers "a.txt"))
  (desc "Utility functions")
  (expect '("str1" "str2")
    (org-git-split-string "str1::str2"))
  (expect '("str1" "")                  ;empty search string
    (org-git-split-string "str1"))
  (desc "extraction of file names")
  (expect "foo.org"
    (org-git-link-filename "firstlevelfiles:test/foo.org"))
  (expect "foo.org"
    (org-git-link-filename ":test/foo.org"))
  (expect "78981922613"
    (org-git-link-filename "78981922613")) ; a.txt
  (desc "finding git directory")
  (expect '("/foo/bar/" "baz")
          (org-git-split-dirpath "/foo/bar/baz/"))
  (expect `(,gitdir "test/testgitrepos/a.txt")
          (org-git-find-gitdir (expand-file-name "testgitrepos/a.txt" git-test-src-dir)))
  (expect `(,gitdir "test/testgitrepos/foo/bar/baz.txt")
          (org-git-find-gitdir (expand-file-name "testgitrepos/foo/bar/baz.txt" git-test-src-dir)))
  (desc "Git functions")
  (expect "a\n" 
    (with-temp-buffer
      (let ((object "firstlevelfiles:test/testgitrepos/a.txt") 
            (buffer (current-buffer))) 
        (org-git-show gitdir object buffer)
        (buffer-string)))) 
  (expect "baz\n" 
    (with-temp-buffer 
      (let ((object "foobarbaztxt:test/testgitrepos/foo/bar/baz.txt") 
            (buffer (current-buffer))) 
        (org-git-show gitdir object buffer)
        (buffer-string))))
  (expect (error) 
    (with-temp-buffer 
      (let ((object "deletebaztxt:test/testgitrepos/foo/bar/baz.txt") 
            (buffer (current-buffer))) 
        (org-git-show gitdir object buffer)
        (buffer-string))))
  (expect "78981922613b2afb6025042ff6bd878ac1994e85"
    (org-git-blob-sha gitdir "firstlevelfiles:test/testgitrepos/a.txt")))

;; (expectations-execute)                 ;  use C-M-x on expectations sexp instead

