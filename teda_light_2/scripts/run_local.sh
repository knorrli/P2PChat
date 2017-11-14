#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: runs an Erlang application on the local machine, starting
#              the master on the local machine.
#
# NOTICE
# This script requires the presence of enodes.conf in the application directory.
#
# Usage 
#    run_local.sh  <app_dir>  "<mod:fct>"  <cookie>
#
# where
#    <app_dir>      : name of directory containing app, relative to teda/apps
#    "<mod:fct>"    : Erlang module and function to exectue by master
#    <cookie>       : the same cookie as used for h_depl_enodes_1.sh
#
# Examples
#    ../../scripts/run_local.sh  echo  "echod_nn:master()"  abc
#
# Authors: C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 22 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf in a POSIX compatible way
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"
. "${SCRIPTSDIR}/h_run.sh"

# Non-configurable variables
APPDIR=$1
FCT=$2
COOKIE=$3

terminate(){
    # Remove comments and empty lines from enodes file
    sed -e 's/^\s*%.*//' -e '/^\s*$/d' "${APPSDIR}/${APPDIR}/enodes.conf" > enodes.tmp 2>/dev/null
    
    # Create an Erlang command to shutdown all nodes...
    while read LINE
    do
	append CMD "rpc:call(${LINE%%.},init,stop,[]),"
    done < enodes.tmp

    rm -f enodes.tmp

    # ... as well as the terminator node.
    append CMD "init:stop()."

    # Setup the terminator node and shutdown all nodes
    # This requires a new random variable for the id of the Erlang node as well
    # as the host name for the short name option '-sname' without any domain
    # name.
    echo "Terminating nodes" 1>&2
    rng_set
    # Avoid naming the local Erlang nodes by their machine name. Instead
    # explictely name the host name localhost.
    #erl -sname ${RAND}@${LHOST%%.*} -setcookie $COOKIE -noshell -eval "$CMD"
    erl -sname ${RAND}@localhost -setcookie $COOKIE -noshell -eval "$CMD"

    # Finally shut down epmd on the local machine
    echo "Shutting down Erlang Port Mapper Daemons (epmd)..." 1>&2
    echo -n "  ${LHOST%%.*}... " 1>&2
    epmd -kill 1>&2
    
    exit
}

# Initialize callback to shutdown nodes if interrupted
trap 'terminate' INT

# Set random name for Erlang master node
rng_set

# Launch Erlang master node
# For the short name option '-sname' we have to remove the domain name if
# LHOST conatains a fully qualified host name.
if [ -d "${APPSDIR}/${APPDIR}/bin" ]; then
    cd "${APPSDIR}/${APPDIR}/bin"
else
    cd "${APPSDIR}/$APPDIR"
fi
# Avoid naming the local Erlang nodes by their machine name. Instead explictely
# name the host name localhost.
#LHOST=$(uname -n)
#erl -sname ${RAND}@${LHOST%%.*} -setcookie $COOKIE -noshell -eval "${FCT},init:stop()."
erl -sname ${RAND}@localhost -setcookie $COOKIE -noshell -eval "${FCT},init:stop()."

# Shutdown Erlang nodes
terminate
