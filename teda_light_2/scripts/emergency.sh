#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: in case of emergency run this script to shutdown all remaining
#              Erlang nodes on a machine.
#
# NOTICE
# If no arguments are passed to this script, it will destroy all Erlang nodes
# on the local machine.
#
# Usage
#    emergency.sh  [<remote_host>  <remote_user>]
#
# where
#    <remote_host> : DNS or IP of the remote host (optional)
#    <remote_user> : username to connect on remote host (optional)
#
# Example
#    ../../scripts/emergency.sh  diufmac33.unifr.ch  evequozf
#
# Authors: C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 1 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Non-configurable variables
REMOTEHOST=$1
REMOTEUSER=$2

# The pipe of commands does the following (POSIX compatible):
# 1) list all processes in the format: PID USER CMD
# 2) filter out any processes that do not belong to the user
# 3) keep only beam.smp processes
# 4) extract the PID of the beam.smp process with regexps

if [ $# -ge 2 ]; then
    SSH_ARGS="-i $RSAKEY"
    MACHINE=$REMOTEHOST
    
    ssh $SSH_ARGS ${REMOTEUSER}@$MACHINE "for P in \$(ps -A -o pid,user,args | grep \$USER | grep beam.smp | sed -e 's/.\+grep.\+//' -e '/^\s*$/d' -e 's/^\s*\([0-9]\+\)\s*.*/\1/'); do kill \$P; done"
else
    PIDS=$(ps -A -o pid,user,args | grep \$USER | grep beam.smp | sed -e 's/.\+grep.\+//' -e '/^\s*$/d' -e 's/^\s*\([0-9]\+\)\s*.*/\1/')
    for P in $PIDS; do
	kill $P
    done
fi
