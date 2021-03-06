#!/bin/sh

# Run all rules and return 0 if no rule applied = success.
#
# Accept source files as argument, or run on the .lisp files of the current directory.
#
# TODOs:
# - [X] use -review
# - [X] correctly format the output (with trivial-formatter or emacs)
#
# Nice to have:
# - integrate sblint or lisp-critic


# emacs --batch needs a full path.
SCRIPT_HOME=$(dirname $0)
# inside the colisper directory, dirname returns ".", we want to full path...
if [ $SCRIPT_HOME = "." ] ; then
    SCRIPT_HOME=$(pwd)
fi

VERSION=$(cat $SCRIPT_HOME/VERSION)

COMBY_EXEC=comby

INPLACE=0
COMBY_INPLACE=""
EMACS_BATCH_FN="indent-file"

REVIEW=0
COMBY_REVIEW=""

help() {
    echo "colisper v"$VERSION
    echo "Usage: `basename $0` [--in-place] [somefile.lisp]"
    echo
    echo "  [--in-place]: write changes (correct indentation depends on emacs)."
    echo "  [--review]: interactively accept or reject changes (comby -review)"
    echo
    echo "Run on the given file or on all .lisp files of the current directory."
}

if [ $# -gt 1 ] && [ $1 = "--help" -o $1 = "-h" ] ; then
    help
    exit 1
fi

PATTERNS_DIR=$SCRIPT_HOME/src/catalog/lisp/base/

returncode=1

for arg in "$@" ; do
    if [ "-in-place" = $1 ] || [ $1 = "--in-place" ] ; then
        INPLACE=1
        COMBY_INPLACE="-in-place"
        EMACS_BATCH_FN="indent-and-save"
        shift
    fi

    if [ $1 = "--review" ] || [ $1 = "-review" ] ; then
        REVIEW=1
        COMBY_REVIEW="-review"
        shift
    fi
done

# Rest of the CLI arguments: one or many files. If none are passed, assume all .lisp files.
REMAINING_ARGS=$@
if [ $# -eq 0 ] ; then
    REMAINING_ARGS=.lisp
fi

result_txt=$SCRIPT_HOME/.result.txt
all_results_txt=$SCRIPT_HOME/.allresults.txt

# idea: separate rules into categories, take the one for code checking only.
if [ $REVIEW -eq 1 ] ; then
        $COMBY_EXEC -config $PATTERNS_DIR $COMBY_REVIEW -f $REMAINING_ARGS
else
    $COMBY_EXEC -config $PATTERNS_DIR $COMBY_INPLACE -f $REMAINING_ARGS > $result_txt && \
        cat $result_txt >> $all_results_txt && \
        cat $result_txt
fi

# return code is 0 if there was no output, no rule to apply.
# Comby always returns 0, so this files mess is to check if there were edits, and exit with an error code.
if [ -f $all_results_txt ] ; then
    returncode=$(cat $all_results_txt | wc -l)
    rm $result_txt
    rm $all_results_txt
fi

if [ $INPLACE -eq 1 ] && [ $returncode != 0 ] ; then
    if [ $(which emacs) ] ; then
        emacs -Q --batch $REMAINING_ARGS -l $SCRIPT_HOME/emacs-batch-indent.el -f $EMACS_BATCH_FN
    else
        echo "! emacs executable not found. We won't indent the file."
    fi
fi

if [ $returncode -eq 0 ] && [ $INPLACE -eq 0 ] ; then
    echo "All OK."
elif [ $returncode -eq 0 ] && [ $INPLACE -eq 1 ] ; then
    # XXX: Comby doesn't tell if it found rules or not. Because we passed the -in-place flag,
    # it always returns 0 as return code. We don't know if some rules applied.
    # We might want to run Comby twice.
    echo "All files written in-place."
fi
exit $returncode
