#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: runs an Erlang application on remote machines, starting
#              the master on a remote machine. This script requires the 
#              presence of enodes.conf in the application directory.
#
# Usage
#    run.sh  <app_dir>  "<mod:fct>"  <hosts.conf>  <master_host>  <master_user>
#
# where
#    <app_dir>     : name of application directory, relative to teda/apps
#    "<mod:fct>"   : Erlang module and function to exectue by master
#    <hosts.conf>  : file containing a list of hosts and other parameters
#    <master_host> : DNS or IP of the machine where a new Erlang is created that 
#                    plays the role of master node, i.e. the node where the application 
#                    is launched
#    <master_user> : username to connect on <master_host>
#
# Example
#    run.sh  echo  "echod_nn:master()"  hosts_alive.conf  diufmac33.unifr.ch  evequozf
#
# Authors: C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 26 September 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------
#
# Note on how to start the master node:
# The current implementation of teDA consideres all machines and Erlang nodes in
# the hosts*.conf and enodes.conf file to be worker nodes. An additional Erlang
# node is started in this script on the specified remote machine given in the
# script arguments.
# The reason for this implementation is related to the fact that we want the
# scripts to be as simple and atomic as possible and the fact that the Erlang
# nodes should better be started in detached mode. The enodes.sh script's job
# is to create Erlang nodes in detached mode, the master node however has to
# be started last and in interactive mode.
# In order to start the master Erlang node with the worker nodes one has to do
# the following changes:
# 1) Introduce a convention for deciding which node will be the master (e.g.
#    first node in hosts*.conf)
# 2) Extract code for starting the master node and place it in enodes*.sh. The
#    master node needs to be started last and one needs to introduce an
#    artificial delay of at least 1sec for the machines to start up their Erlang
#    nodes, i.e. synchronize.
# 3) In run.sh start an auxiliary Erlang node in interactive mode to tell the
#    master node by rpc to run its initial function to start the application.
#

# Import configuration teda.conf and h_run.sh
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"
. "${SCRIPTSDIR}/h_run.sh"

# Non-configurable variables
APPDIR=$1
FCT=$2
HOSTS_ALIVE=$3
REMOTEHOST=$4
REMOTEUSER=$5

# Read the cookie from the enodes.conf file. The cookie is by convention the
# first line of the configuration file.
COOKIE=$(cat "${APPSDIR}/${APPDIR}/enodes.conf" | head -n 1 | sed -e 's/\.//')

SSH_ARGS="-i $RSAKEY"
SCP_ARGS="-i $RSAKEY"
MACHINE=$REMOTEHOST
RUSER=$REMOTEUSER

terminate(){
    # Remove the cookie, the comments and empty lines from enodes file and store
    # it in a temporary file needed to shutdown the nodes.
    cat "${APPSDIR}/${APPDIR}/enodes.conf" | tail -n +2 | sed -e 's/^\s*%.*//' -e '/^\s*$/d' > enodes.tmp
    
    # Create an Erlang command to shutdown all nodes...
    while read LINE
    do
	append CMD "rpc:call(${LINE%%.},init,stop,[]),"
    done < enodes.tmp

    rm -f enodes.tmp
    
    # ... as well as the terminator node.
    append CMD "init:stop()."

    # Setup the terminator node and shutdown all nodes
    echo "Terminating nodes" 1>&2
    rng_set
    ssh $SSH_ARGS ${RUSER}@$MACHINE "erl -name ${RAND}@$REMOTEHOST -setcookie $COOKIE -noshell -eval \"$CMD\""

    # Finally shut down epmd on each host
    echo "Shutting down Erlang Port Mapper Daemons (epmd)..." 1>&2
    while read REMOTE_MACHINE USERNAME NB_ENODES NB_PROCS; do	
	echo -n "  ${REMOTE_MACHINE}... " 1>&2
	ssh -n -i $RSAKEY ${USERNAME}@$REMOTE_MACHINE "epmd -kill 1>&2"
    done < "$HOSTS_ALIVE"

    echo -n "  ${REMOTEHOST}... " 1>&2
    ssh $SSH_ARGS ${RUSER}@$MACHINE "epmd -kill 1>&2"

    exit
}

# Copy enodes.conf file to master
scp $SCP_ARGS "${APPSDIR}/${APPDIR}/enodes.conf" ${RUSER}@${MACHINE}:${REMOTEAPPSDIR}/${APPDIR}/

# Initialize callback to shutdown nodes if interrupted
trap 'terminate' INT

# Set random name for Erlang master node
rng_set

# Launch Erlang master node
ssh $SSH_ARGS ${RUSER}@$MACHINE "if [ -d \"${REMOTEAPPSDIR}/${APPDIR}/bin\" ]; then cd ${REMOTEAPPSDIR}/${APPDIR}/bin; else cd ${REMOTEAPPSDIR}/$APPDIR; fi && erl -name ${RAND}@$REMOTEHOST -setcookie $COOKIE -noshell -eval \"${FCT},init:stop().\""

# Shutdown Erlang nodes
terminate
