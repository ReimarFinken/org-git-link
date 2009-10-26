#!/bin/sh
EMACS=emacs
ORGPATH="/home/finken/site-lisp/org"
OPTIONS="-L . -L $ORGPATH"


# idea taken from the CEDET integration tests
if $EMACS -q --no-site-file $OPTIONS -l test-git-link.el -f org-git-execute-tests; then
    # Reverse the meaning of a 0 exit status, as the user had to quit Emacs
    # but on success, the program kills emacs with 1 (to be different.)
    exit 1;
else
    exit 0;
fi
