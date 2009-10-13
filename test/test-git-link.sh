#!/bin/sh
EMACS=emacs
ORGPATH="/home/finken/site-lisp/org"
OPTIONS="-L . -L $ORGPATH"
OUTPUT=/tmp/.el-expectations
$EMACS -q --no-site-file --batch $OPTIONS -l test-git-link.el -f batch-expectations $OUTPUT "$@"
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $ret
