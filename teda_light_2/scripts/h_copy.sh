#!/bin/sh

#-------------------------------------------------------------------------------

# Description: helper script for h_depl_1.sh: copy a directory on a remote machine,
#              possibly behind the router
#
# NOTICE
# If you want, you can also use this script directly, as follows:
#
# Usage
#    h_copy.sh  <remote_host>  <remote_user>  <local_dir>  <remote_dir>
#
#  where
#    <remote_host>  : DNS or IP of the remote host
#    <remote_user>  : username to connect on remote host
#    <local_dir>    : local directory (can be any local directory)
#    <remote_dir>   : directory, relative to ~ on remote host, into which local_dir will be copied 
#
# Examples
#    ../../scripts/h_copy.sh  diufmac33.unifr.ch  evequozf  .  mpe/erl/teda/apps
#    ../../scripts/h_copy.sh  10.42.0.10  pi15  .  mpe/erl/teda/apps  10000
#
# Authors: F. Evéquoz, C. Göttel, University of Fribourg, Switzerland, © 2015
# Last update: 3 Feb. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf in a POSIX compatible way
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Script parameters
REMOTEHOST=$1 
REMOTEUSER=$2               
LOCALDIR=$3
REMOTEDIR=$4                                                                       

# Variables for zip archive and temporary teDA files to exclude from it
ARCHIVE=archive.zip
EXCLUDED_FILES="$(basename "$LOCALDIR")/teda-socket \
                                $(basename "$LOCALDIR")/cdc-socket \
                                $(basename "$LOCALDIR")/enodes.conf \
				$(basename "$LOCALDIR")/hosts_alive.conf \
				$(basename "$LOCALDIR")/hosts.tmp"     # exclude temporary files from copy

# Arguments for SSH and SCP
# The '-n' option is necessary for ssh to forbid it to read from STDIN,
# otherwise the STDIN of the loop in depl_app.sh (i.e. while read ...) is
# redirected to STDIN of ssh.
SCP_ARGS="-i ${RSAKEY}"
SSH_ARGS="-n -i ${RSAKEY}"

echo "  - copy ${LOCALDIR} on ${REMOTEHOST}:${REMOTEDIR}"

# Compress the local directory (last-level directory structure only) and
# exclude any temporary files defined at the beginning of the script.
cd "${LOCALDIR%/*}"
zip -rq $ARCHIVE $(basename "$LOCALDIR") -x ${EXCLUDED_FILES}

# Create the remote directory on the remote host. This command has no effect if
# the directory already exists.
ssh ${SSH_ARGS} ${REMOTEUSER}@${REMOTEHOST} "mkdir -p ${REMOTEDIR#~}"

# Copy and unzip the archive to the remote host
scp ${SCP_ARGS} ${ARCHIVE} ${REMOTEUSER}@${REMOTEHOST}:${REMOTEDIR#~}/
ssh ${SSH_ARGS} ${REMOTEUSER}@${REMOTEHOST} "cd ${REMOTEDIR#~} && unzip -oq $ARCHIVE > /dev/null 2>&1"

# Delete remote and local zip files
ssh ${SSH_ARGS} ${REMOTEUSER}@${REMOTEHOST} "rm ${REMOTEDIR#~}/${ARCHIVE}"
rm -f $ARCHIVE
