#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: destroys a previously installed RSA key authentication.
#
# NOTICE
# This script does not remove the public and private RSA keys on the local machine.
#
# Usage
#    RSA_destroy.sh  <hosts.conf>
#
# where
#    <hosts.conf>: file containing a list of hosts and other parameters, relative to teda/conf
#
# Example
#    RSA_destroy.sh  hosts_all.conf
#
# Author: F. Evéquoz, C. Göttel, University of Fribourg, Switzerland,  © 2015
# Last update: 31 Jan. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration teda.conf
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Non-configurable variables
HOSTS_FILE=$1

if [ -f ${RSAKEY}.pub ]; then
  # Extract the key by escaping forward slashes by adding a backslash.
  # The shell otherwise assumes that the key is a path to a file on the
  # local system.
  # Example substitution: abc/xyz -> abc\/xyz
  KEY=$(sed -e 's/\//\\\//g' ${RSAKEY}.pub)

  # Remove comments and empty lines from hosts file
  sed -e 's/^\s*#.*//' -e '/^\s*$/d' "${CONFDIR}"/$HOSTS_FILE > hosts.tmp
  
  # Remove public ssh key from remote hosts. This can be achieved by a manual
  # operation that requires to manipulate the authorized_keys file. We extracted
  # the public key previously and remove any entry we can find in an
  # authorized_keys file using regular expressions with sed.
  while read MACHINE USERNAME NB_ENODES NB_PROCS; do
      ssh -n -i $RSAKEY ${USERNAME}@$MACHINE "if [ -f \
~/.ssh/authorized_keys ]; then sed -e 's/${KEY}//g' -e '/^\s*$/d' \
~/.ssh/authorized_keys > ~/.ssh/ak; mv ~/.ssh/ak ~/.ssh/authorized_keys; fi"
  done < hosts.tmp

  rm -f hosts.tmp
else
  echo "Error: ${RSAKEY}.pub not found. Keys need to"
  echo "be removed manually in ~/.ssh/authorized_keys on all remote machines."
fi
