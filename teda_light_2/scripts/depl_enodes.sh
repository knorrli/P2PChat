#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: creates ni Erlang nodes on host i, where the addresses of all hosts i and
#              and the number ni of nodes to create are specified in a config file.
#
# NOTICE
# This script produces a 'enodes.conf' file containing, by convention, the cookie
# in the first line, and the created node ids, each on one line. The cookie is
# also displayed on the command line.
#
# Usage 
#    depl_enodes.sh  <app_dir>  [<hosts.conf> [<enodes.conf>]] 
#
# where
#    <app_dir>      : name of directory containing app, relative to teda/apps
#    <hosts.conf>   : file containing a list of hosts and parameters (optional, default: <app_dir>/hosts_alive.conf)
#    <enodes.conf>  : name of the file which is created by this script and 
#                     will contain the ids of created nodes and the cookie
#                     (optional, default: <app_dir>/enodes.conf)
#
# Example
#    ../../scripts/depl_enodes.sh  echo
#
# Authors: F. Evéquoz, C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 1 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf and h_run.sh
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"
. "${SCRIPTSDIR}/h_run.sh"

# Non-configurable variables
APPDIR=$1                                                                              
HOSTS_FILE=${2:-$APPSDIR/$APPDIR/hosts_alive.conf}   # hosts file (optional, defaults to <app_dir>/hosts_alive.conf)
ENODES_FILE=${3:-$APPSDIR/$APPDIR/enodes.conf}  # enodes.conf file (optional, defaults to <app_dir>/enodes.conf)

# Delete old and create new enodes.conf file
rm -rf "${ENODES_FILE}" 
touch "${ENODES_FILE}"

# Create a random cookie and write it to enodes.conf so that it can be read by
# run.sh and can also be accessed by an Erlang application (if needed)
rng_set
COOKIE=${RAND}
echo "${COOKIE}." >> "${ENODES_FILE}"

# Remove comments and empty lines from hosts file
sed -e 's/^\s*#.*//' -e '/^\s*$/d' "$HOSTS_FILE" > hosts.tmp

# For each host in HOSTS_FILE call h_depl_enodes_1.sh 
while read MACHINE USERNAME NB_ENODES NB_PROCS; do
    "${SCRIPTSDIR}/h_depl_enodes_1.sh" $APPDIR $NB_ENODES $NB_PROCS $COOKIE $USERNAME $MACHINE \
				       >> "${ENODES_FILE}"  
done < hosts.tmp

rm -f hosts.tmp

# Write info to console
echo "Erlang nodes and cookie created and written to ${ENODES_FILE}."
echo "Cookie: ${COOKIE}"
