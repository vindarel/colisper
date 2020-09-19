#!/bin/sh

# Run all rules and return 0 if no rule applied = success.

ARGCOUNT=1  # expect one argument, a lisp file.

if [ $# -ne "$ARGCOUNT" ]
then
    echo "Usage: `basename $0` somefile.lisp"
    exit 1
fi

PATTERNS_DIR=src/patterns/

returncode=0

# XXX: separate rules into categories, take the one for code checking.
for file in $(ls $PATTERNS_DIR); do
    echo " * running " $file
    # XXX: comby accepts -config with more than one comma-separated paths.
    # But then how do we check that ONE rule failed?
    comby -config $PATTERNS_DIR/$file -f $1 > .result.txt && \
        cat .result.txt >> .allresults.txt && \
        cat .result.txt
done

# return code is 0 if there was no output, no rule to apply.
# all this files mess because I can't sum a variable in bash.
returncode=$(cat .allresults.txt | wc -l)
# echo $returncode

# check file exists
rm .result.txt
rm .allresults.txt

if [ $returncode = 0 ] ; then
    echo "All OK."
fi
exit $returncode
