#! /bin/sh

#-------------------------------------------------------------------------------
#
# Description: helper functions, imported by run scripts.
#
# Author: C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 4 Dec. 2015
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# append:
# Appends the second to last arguments of this function to the variable given
# as first argument to the function.
append(){
    var=$1
    shift
    eval "$var=\"\$$var $*\""
}

# rng_set:
# Stores a new random variable according to the available random number
# generators in the RAND variable.
rng_set(){
    case $RNG in
	0) RAND=$RNG_COUNT
	   $((${RNG_COUNT}+1)) ;;
	1) RAND=$(od -An -N4 -i /dev/random | sed -e 's/[ \t]*//g') ;;
	2) RAND=$(od -vAn -N4 -tu4 < /dev/urandom | sed -e 's/[ \t]*//g') ;;
	*)
	    echo "ERROR: messed up random generator."
	    exit 1 ;;
    esac
}

# Initialize random number generator
if [ -c /dev/random ]; then
    RNG=1
fi
if [ -c /dev/urandom ]; then
    RNG=2
fi
if [ -z "$RNG" ]; then
    echo "WARNING: No random generator for node names" 1>&2
    RNG_COUNT=1
    RNG=0
fi
