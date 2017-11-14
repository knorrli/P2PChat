#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: helper script for depl_enodes.sh: creates n Erlang nodes on exactly one
#              machine.
#
# NOTICE
# This script writes the enode entries to STDOUT.
# If you want, you can also use this script directly, as follows:
#
# Usage
#    h_depl_enodes_1.sh  <app_dir>  <num_enodes>  <num_procs>  <cookie> [<remote_user>  <remote_host>]
#
# where
#    <app_dir>      : name of directory containing app, relative to teda/apps
#    <num_enodes>   : number of enodes to create
#    <num_procs>    : limit number of Erlang processes
#    <cookie>       : unique cookie shared among Erlang nodes
#    <remote_user>  : username to connect on remote host (optional)
#    <remote_host>  : DNS or IP of the remote host (optional, default: local machine)
#
# Example
#    ../../scripts/h_depl_enodes_1.sh  echo  2  abc  evequozf  diufmac33.unifr.ch
#
# Authors: F. Evéquoz, C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 3 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf + h_run in a POSIX compatible way
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"
. "${SCRIPTSDIR}/h_run.sh"

# Script parameters
APPDIR=$1
NB_ENODES=$2
NB_PROCS=$3
COOKIE=$4
REMOTEUSER=$5       # optional (do not set if local)
REMOTEHOST=$6       # optional (do not set if local)          

# Initialize MACHINE variable
MACHINE=$REMOTEHOST

# Create enodes iteratively, start them, and write each created enode id to the
# console.
# Erlang nodes have to be started in the application directory in order to find
# the compiled module. If the Erlang nodes are started remotely they will use
# the full name option '-name' with a fully qualified hostname or IP address.
# When the Erlang nodes are started locally it is sufficient to use the short
# name option '-sname' that makes only use of the host name and avoids the
# domain name.
# Note: all Erlang nodes are created in detached mode, i.e. there is no
# interactive way to access them, all interaction has to take place via IPC,
# e.g. via another Erlang nodes with an interactive Erlang shell.
I=0
while [ $I -lt $NB_ENODES ]; do
    # Assign a random number to variable RAND.
    # rng_set is defined in run_helper.sh.
	rng_set
	
	# If more than 4 parameters are passed to this script, then create Erlang
	# nodes remotely through ssh
    if [ $# -gt 4 ]; then
    	ssh -n -i ${RSAKEY} \
        	${REMOTEUSER}@$MACHINE "if [ -d \"${REMOTEAPPSDIR}/${APPDIR}/bin\" ]; then cd ${REMOTEAPPSDIR}/${APPDIR}/bin; else cd ${REMOTEAPPSDIR}/${APPDIR}; fi && erl +P $NB_PROCS -name ${RAND}@${REMOTEHOST} -setcookie $COOKIE -detached"

   	# otherwise, create nodes locally
	# The uname program returns the host name which can be a fully qualified
	# hostname. In this case we have to remove the domain name which is done
	# by the next line.
    else
	if [ -d "${APPSDIR}/${APPDIR}/bin" ]; then
    	    cd "${APPSDIR}/${APPDIR}/bin"
	else
	    cd "${APPSDIR}/${APPDIR}"
	fi
	# Avoid naming the local Erlang nodes by their machine name. Instead
	# explictely name the host name localhost.
    	#LHOST=$(uname -n)
    	#REMOTEHOST=${LHOST%%.*}
	REMOTEHOST=localhost
    	erl +P $NB_PROCS -sname ${RAND}@${REMOTEHOST} -setcookie $COOKIE -detached
    fi

    # Increment counter
    I=$((${I}+1))
    
    # Write name of created node to the console in a form that can be read by Erlang
    # code
    echo \'${RAND}@${REMOTEHOST}\'.
done 
