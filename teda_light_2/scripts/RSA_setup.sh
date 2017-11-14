#!/bin/sh

#-------------------------------------------------------------------------------
#
# Description: sets up RSA key authentication for ssh and scp, so that we don't
#              need to enter the password anymore.
#
# Usage
#    RSA_setup.sh  <hosts.conf>
#
# where
#    <hosts.conf>: file containing a list of hosts and other parameters (inside teda/conf)
#
# Example
#    ../../scripts/RSA_setup.sh  hosts_all.conf
#
# Author: C. Goettel, F. Evéquoz, University of Fribourg, Switzerland,  © 2015
# Last update: 31 Jan. 2016
# Reference: B. Hirsbrunner: "PAI's CDC Lab – Memento", September 2015
#
#-------------------------------------------------------------------------------

# Import configuration file teda.conf
SCRIPTSDIR=$(dirname $0)
. "${SCRIPTSDIR}/../conf/teda.conf"

# Non-configurable variables
HOSTS_FILE=$1

# Test for ssh-copy-id
# Note: MacOS does not have ssh-copy-id command.
if [ -f /usr/bin/ssh-copy-id ] || [ -f /usr/local/bin/ssh-copy-id ]; then
    HAVE_SSHCOPY=true
else
    HAVE_SSHCOPY=false
fi

# Generate RSA key only if identity file does not exist
if [ ! -f $RSAKEY ]; then
    # Generete RSA key pair
    ssh-keygen -t rsa -N "" -f $RSAKEY > /dev/null

    # Make sure public and private RSA keys were correctly generated
    if [ ! -f $RSAKEY ] || [ ! -f ${RSAKEY}.pub ]; then
	echo "$(basename $0): Keys were not or incorrectly generated."
	exit 1
    fi
fi

# Remove comments and empty lines from hosts file
sed -e 's/^\s*#.*//' -e '/^\s*$/d' "${CONFDIR}/$HOSTS_FILE" > hosts.tmp

# Distribute the public ssh key to the remote hosts
while read REMOTE_MACHINE USERNAME NB_ENODES NB_PROCS TUNNEL; do
    echo "${USERNAME}@$REMOTE_MACHINE"
    SSH_ARGS="-n"
    SCP_ARGS="-i $RSAKEY"
    MACHINE=$REMOTE_MACHINE
	
    if $HAVE_SSHCOPY; then
	   ssh-copy-id $SCP_ARGS ${USERNAME}@$MACHINE > /dev/null 2>&1
    else
	# Extract the key by escaping forward slashes by adding a backslash.
        # The shell otherwise assumes that the key is a path to a file on the
        # local system.
        # Example substitution: abc/xyz -> abc\/xyz
	KEY=$(sed -e 's/\//\\\//g' ${RSAKEY}.pub)
	# Install the public RSA key.
	# 1.) make sure the .ssh directory exists
	# 2.) append the public RSA key to the authorized_keys file
    ssh $SSH_ARGS ${USERNAME}@$MACHINE "if [ ! -d ~/.ssh ]; then mkdir \
~/.ssh; fi && echo $KEY >> ~/.ssh/authorized_keys"
    fi
    
    # Check if the RSA key installation was successful by comparing the return
    # value of the last command executed to 0, i.e. no error occurred (POSIX
    # standard return value).
    if [ ! $? -eq 0 ]; then
	echo "Remote setup for host $REMOTE_MACHINE failed."
    else
	echo "Remote setup for host $REMOTE_MACHINE succeeded."
    fi
    
done < hosts.tmp

rm -f hosts.tmp
