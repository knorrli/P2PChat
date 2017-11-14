#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: A script to test the availability of remote machines.
#
# NOTICE
# This script reads the list of hosts to test from a config file and writes
# the list of 'live' hosts to <app_dir>/hosts_alive.conf by removing comments
# and empty lines.
#
# Usage 
#    ping.sh  <app_dir>  <hosts.conf> 
#
# where
#    <app_dir>    : name of directory containing app, relative to teda/apps
#    <hosts.conf> : file containing a list of hosts and other parameters to ping (inside teda/conf)
#    
# Examples
# 	../../scripts/ping.sh   echo  hosts.conf
#
# Author: C. Göttel, Florian Evéquoz University of Fribourg, Switzerland, © 2015
# Last update: 1 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Non-configurable variables
APPDIR=$1
HOSTS_FILE=$2
HOSTS_ALIVE="${APPSDIR}/${APPDIR}/hosts_alive.conf"

# Remove any previously created hosts file and create new one
rm -f "$HOSTS_ALIVE"
touch "$HOSTS_ALIVE"

# Remove any comments and empty lines from the hosts file and safe it as
# temporary file.
sed -e 's/^\s*#.*//' -e '/^\s*$/d' "${CONFDIR}/$HOSTS_FILE" > hosts.tmp

# Ping for availability of remote hosts and store result in a hosts_alive.conf
# file. Also make sure ssh runs and credentials are correct.
while read MACHINE USERNAME NB_ENODES NB_PROCS; do
    # Machine is in the same subnet and can be pinged from the local machine
    ping -c 1 $MACHINE > /dev/null
    PINGRES=$?

    # Check the ssh credentials for the remote machine
    if [ $PINGRES -eq 0 ]; then
	ssh -n -i $RSAKEY ${USERNAME}@$MACHINE "exit 0" > /dev/null 2>&1
	SSHRES=$?
    fi

    # Write machine into hosts file if it is alive otherwise print a warning
    if [ $PINGRES -eq 0 ] && [ $SSHRES -eq 0 ]; then
		echo "$MACHINE $USERNAME $NB_ENODES $NB_PROCS $TUNNEL" >> "$HOSTS_ALIVE"
    else
		echo "$(basename $0) warning: connection to $MACHINE failed"
    fi    
done < hosts.tmp

rm -rf hosts.tmp
