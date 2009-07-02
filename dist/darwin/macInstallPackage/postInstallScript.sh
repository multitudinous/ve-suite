#!/bin/sh
#

# The goal is to update the users vr directory without overwriting any
# of the users files that are not part of the install.

# Try to create a vr directory - if one exists, nothing bad happens
sudo mkdir ~/vr

# Move files that are temporarily at root level to user $HOME/vr
sudo mv /opt/ves/vr/* ~/vr
sudo rm -rf /opt/ves/vr

# Correct owner and group of the files moved to user $HOME
# following did not work because administator sign-in overwrote the users name and group
#sudo chown -R `id -un`:`id -ug` ~/vr

# Copy user and group from that of the Desktop directory:
VES_OWNER=`ls -l ~ | grep Desktop | awk '{print $3}'`
#echo "VES_OWNER =" $VES_OWNER

VES_GROUP=`ls -l ~ | grep Desktop | awk '{print $4}'`
#echo "VES_GROUP =" $VES_GROUP

sudo chown -R ${VES_OWNER}:${VES_GROUP} ~/vr

