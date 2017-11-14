#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: helper script for depl_app.sh: deploys an application directory 
#              to exactly one machine.
#
# NOTICE
# If you want, you can also use this script directly, as follows:
#
# Usage
#    h_depl_1.sh  <app_dir>  <remote_user>  <remote_host>  [<cmd>]
#
# where
#    <app_dir>     : name of directory containing app, relative to teda/apps
#    <remote_user> : username to connect on remote host
#    <remote_host> : DNS or IP of the remote host
#    <cmd>         : copy, make or clean (optional, default: copy)
#
# Examples
#    ../../scripts/h_depl_1.sh  echo  evequozf  diufmac33.unifr.ch
#
# Authors: F. Evéquoz, C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 3 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Script parameters
APPDIR=$1
REMOTEUSER=$2               
REMOTEHOST=$3   
COMMAND=${4:-copy}                # type of command to execute (optional 4th argument, defaults to 'copy')       

# Which command: copy, make or clean ?
COPY=false
MAKE=false
CLEAN=false

# Set boolean values according to the operation that is supposed to be executed
case $COMMAND in
clean)
  CLEAN=true 
  ;;
copy)
  COPY=true 
  ;;
make)
  COPY=true
  MAKE=true 
  ;;
*)                # other argument -> set to default. 
  COPY=true       # !! Should eventually exit with an error 'Invalid Argument' / FIXME
  ;;      
esac

#################################

echo "  - deploy '${COMMAND}' ${APPDIR} on ${REMOTEHOST}"

# Copy both app and lib directories to remote host.
# Note: the lib directory is currently outcommented because is has no real use
# yet.
if $COPY ; then
    "${SCRIPTSDIR}/h_copy.sh" ${REMOTEHOST} ${REMOTEUSER} "${APPSDIR}/${APPDIR}" "${REMOTEAPPSDIR}"
    # If teda.erl is required uncomment.
    # "${SCRIPTSDIR}/h_copy.sh" ${REMOTEHOST} ${REMOTEUSER} "${LIBDIR}" "${REMOTEDIR}" # copy lib -> removed
fi

# Build (make) copied directory on remote host
if $MAKE ; then
    ssh -n -i ${RSAKEY} -o NoHostAuthenticationForLocalhost=yes \
      ${REMOTEUSER}@${REMOTEHOST} "cd \"${REMOTEAPPSDIR}/${APPDIR}\" && if [ -f Makefile ]; then make; else erl -make; fi"
fi

# Clean on remote host = remove remotely deployed app directory (do not remove lib)
# Note: if the lib directory is used it might be a good idea to also remove the
# lib directory, because it might contain the compiled code, e.g. teda.beam.
if $CLEAN ; then
  ssh -n -i ${RSAKEY} -o NoHostAuthenticationForLocalhost=yes \
    ${REMOTEUSER}@${REMOTEHOST} "rm -rf \"${REMOTEAPPSDIR}/${APPDIR}\""
fi

echo "  - done"
