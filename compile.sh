#! /usr/bin/env bash

set -e

SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")

all_no_docs () {
    echo "***** ALL-NO-DOCS *****"
    cd "$SCRIPT_DIR"
    make -j1 clean all-no-docs
}

def_use () {
    echo "******* DEF-USE *******"
    cd "$SCRIPT_DIR/mlton"
    make -j1 def-use
}

runtime () {
    echo "******* RUNTIME *******"
    cd "$SCRIPT_DIR"
    make -j1 runtime
}

# Main

if [[ $# = "0" ]] ; then
    all_no_docs
    def_use
    exit 0
else
    case $1 in
	all-no-docs)
	    all_no_docs
	    exit 0
	    ;;

	def-use)
	    def_use
	    exit 0
	    ;;

	runtime)
	    runtime
	    exit 0
	    ;;

	*)
	    echo "$1 is not a valid compilation target" 1>&2
	    exit 1
	    ;;
    esac
fi
