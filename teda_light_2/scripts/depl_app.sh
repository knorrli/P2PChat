#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: deploys a local application directory to the hosts listed in a
#              config file. Options are 'make' (compiles in addition all *.erl files),
#              and 'clean' (removes directories that have been deployed previously).
#              
# NOTICE
# This script supports the deployment to machines being in different subdomains,
# including machines located somewhere on the internet.
#
# Usage
#    depl_app.sh  <app_dir>  [<cmd> [<hosts.conf>]] 
#
# where
#    <app_dir>    : name of directory containing app, relative to teda/apps
#    <cmd>        : copy, make or clean (optional, default: copy)
#    <hosts.conf> : file containing a list of hosts and parameters (optional, default: <app_dir>/hosts_alive.conf)
# 
# Examples
#    ../../scripts/depl_app.sh  echo               ### same as: ../../scripts/depl_app.sh  echo  copy  hosts_alive.conf
#    ../../scripts/depl_app.sh  echo  clean
#    ../../scripts/depl_app.sh  echo  make  ./my_hosts_alive.conf
#
# Author: F. Evéquoz, University of Fribourg, Switzerland, © 2015
# Last update: 31 Jan. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Non-configurable variables
APPDIR=$1                                                                              
COMMAND=${2:-copy}                                    # command to send to 'h_depl_1.sh' (optional, defaults to 'copy')                                                                                                                            
HOSTS_FILE=${3:-"$APPSDIR/$APPDIR/hosts_alive.conf"}  # hosts file (optional, defaults to '<app_dir>/hosts_alive.conf')

# Remove any comments and empty lines from the hosts file and save it as temporary file
sed -e 's/^\s*#.*//' -e '/^\s*$/d' "$HOSTS_FILE" > hosts.tmp

# For each host in HOSTS_FILE call h_depl_1.sh
while read MACHINE USERNAME NB_ENODES NB_PROCS; do
    "${SCRIPTSDIR}/h_depl_1.sh" ${APPDIR} ${USERNAME} ${MACHINE} ${COMMAND}; 
done < hosts.tmp

rm -f hosts.tmp
