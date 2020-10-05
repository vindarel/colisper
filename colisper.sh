#!/bin/sh

# Run all rules and return 0 if no rule applied = success.
#
# TODOs:
# - use -review
# - correctly format the output (with trivial-formatter or emacs)
#
# Nice to have:
# - integrate sblint or lisp-critic

VERSION=$(cat VERSION)

ARGCOUNT=1  # expect one argument, a lisp file.

INPLACE=1
COMBY_INPLACE=""
EMACS_BATCH_FN="indent-file"

REVIEW=1
COMBY_REVIEW=""

help() {
    echo "colisper v"$VERSION
    echo "Usage: `basename $0` [--in-place] somefile.lisp"
    echo
    echo "  [--in-place]: write changes (correct indentation depends on emacs)."
    echo "  [--review]: interactively accept or reject changes (comby -review)"
}

if [ $# -lt "$ARGCOUNT" ] || [ $1 = "--help" ] || [ $1 = "-h" ]
then
    # echo "Usage: `basename $0` somefile.lisp"
    help
    exit 1
fi

PATTERNS_DIR=src/catalog/lisp/

returncode=1

for arg in "$@" ; do
    if [ "-in-place" = $1 ] || [ $1 = "--in-place" ] ; then
        INPLACE=0
        COMBY_INPLACE="-in-place"
        EMACS_BATCH_FN="indent-and-save"
        shift
    fi

    if [ $1 = "--review" ] || [ $1 = "-review" ] ; then
        REVIEW=0
        COMBY_REVIEW="-review"
        shift
    fi
done


# idea: separate rules into categories, take the one for code checking only.
if [ $REVIEW = 0 ] ; then
        comby -config $PATTERNS_DIR $COMBY_REVIEW -f $1
else
    comby -config $PATTERNS_DIR $COMBY_INPLACE -f $1 > .result.txt && \
        cat .result.txt >> .allresults.txt && \
        cat .result.txt
fi

# return code is 0 if there was no output, no rule to apply.
# Comby always returns 0, so this files mess is to check if there were edits, and exit with an error code.
if [ -f .allresults.txt ] ; then
    returncode=$(cat .allresults.txt | wc -l)
    rm .result.txt
    rm .allresults.txt
fi

if [ $INPLACE ] ; then
    if [ $(which emacs) ] ; then
        emacs -Q --batch $@ -l ~/projets/colisper/emacs-batch-indent.el -f $EMACS_BATCH_FN
    else
        echo "! emacs executable not found. We won't indent the file."
    fi
fi

if [ $returncode = 0 ] ; then
    echo "All OK."
fi
exit $returncode
