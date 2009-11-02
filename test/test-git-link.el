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
  "Path to the git directory") ; to prevent default-directory to change between calls

(defvar branchname (shell-command-to-string "./get-branch.sh"))

(require 'org-git-link)		; after .. has been added to load-path
(require 'el-expectations)	; after . has been added to load-path

(setq inhibit-splash-screen t)		; for batch mode
(setq debug-on-error t)

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
  (expect '("/" "")
    (org-git-split-dirpath "/"))        ; no upper directory
  (expect `(,gitdir "test/testgitrepos/a.txt")
    (org-git-find-gitdir (expand-file-name "testgitrepos/a.txt" git-test-src-dir)))
  (expect `(,gitdir "test/testgitrepos/foo/bar/baz.txt")
    (org-git-find-gitdir (expand-file-name "testgitrepos/foo/bar/baz.txt" git-test-src-dir)))
  (expect nil
    (org-git-find-gitdir "/foo/bar/baz.txt")) ;not existing
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
    (org-git-blob-sha gitdir "firstlevelfiles:test/testgitrepos/a.txt"))
  (desc "Storing link name")
  (expect branchname
    (org-git-get-current-branch gitdir))
  (expect "2009-10-03"
    (org-read-date nil nil "3 oct 2009"))
  (expect "master@{2009-10-03}"
    (org-git-create-searchstring "master" (org-read-date nil nil "3 oct 2009")))
  (expect (concat "git:" git-test-src-dir "foo.txt::" (org-git-get-current-branch gitdir) (format-time-string "@{%Y-%m-%d}" (current-time)))
    (org-git-create-git-link (expand-file-name "foo.txt" git-test-src-dir)))
  (expect (true)
    (org-git-gitrepos-p (expand-file-name "foo.txt" git-test-src-dir)))
  (expect nil
    (org-git-gitrepos-p "/foo/bar.txt"))
  (desc "Storing and retrieving links")
  (expect "git"
    (find-file (concat git-test-src-dir "testgitrepos/b.txt"))
    (org-store-link nil)
    (plist-get org-store-link-plist :type))
  ;; cannot test org-store-link in non-interactive mode
  ;; (call-interactively does not work since interactive-p is nil
  ;; in batch mode)
  (expect (true)
    (find-file (concat git-test-src-dir "testgitrepos/b.txt"))
    (call-interactively 'org-store-link) ; only interactive (non-batch) calls store in org-stored-links
    (kill-buffer)
    (switch-to-buffer (get-buffer-create "*expect org-git test*"))
    (org-mode)
    (with-mock (stub read-string => "")
               (org-insert-link nil (car (car org-stored-links))))
    (goto-char (point-min))
    (org-open-at-point)
    (get-buffer "b.txt"))
  (expect "[[git:foo.txt::master][description]]"
    (with-temp-buffer
      (org-git-insert-link-interactively "foo.txt" "master" "description")
      (buffer-string)))
  (desc "Bugs")
  (expect nil
    (with-current-buffer "*scratch*"
      (org-git-store-link))))

;; idea taken from the CEDET test suite
(defun org-git-execute-tests ()
  "Display a MESSAGE that some test is now finished.
Argument STYLE is the type of build done."
  (let ((res (expectations-execute nil))) ; res = errors + failures
    (get-buffer expectations-result-buffer)
    (when (zerop res)
      (insert "\n\nWaiting 5 seconds before exiting with positive exit status.\n")
      ;; Now wait.
      (sit-for 5)
      ;; 1 means GOOD to the shell script, since any other emacs exit
      ;; mechanism will be 0. (ie - click on the X in the corner.)
      (kill-emacs 1))))


;; (expectations-execute)                 ;  use C-M-x on expectations sexp instead
