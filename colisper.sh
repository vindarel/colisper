#!/bin/sh

# Run all rules and return 0 if no rule applied = success.
#
# TODOs:
# - use -review
# - correctly format the output (with trivial-formatter or emacs)
#
# Nice to have:
# - integrate sblint or lisp-critic

ARGCOUNT=1  # expect one argument, a lisp file.

if [ $# -ne "$ARGCOUNT" ]
then
    echo "Usage: `basename $0` somefile.lisp"
    exit 1
fi

PATTERNS_DIR=src/patterns/

returncode=1

# XXX: -config / -templates accept a directory as argument or more than one comma-separated paths to toml files.
# But then how do we check that ONE rule failed?
# idea: separate rules into categories, take the one for code checking.
for file in $(ls $PATTERNS_DIR); do
    echo " * running " $file
    comby -config $PATTERNS_DIR/$file -f $1 > .result.txt && \
        cat .result.txt >> .allresults.txt && \
        cat .result.txt
done

# return code is 0 if there was no output, no rule to apply.
# all this files mess because I can't sum a variable in bash.
if [ -f .allresults.txt ] ; then
    returncode=$(cat .allresults.txt | wc -l)
    # echo $returncode

    rm .result.txt
    rm .allresults.txt
fi

if [ $returncode = 0 ] ; then
    echo "All OK."
fi
exit $returncode
