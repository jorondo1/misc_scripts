# Each time you log in
newgrp def-ilafores

# Once in a lifetime
echo umask 007 >> ~/.bashrc
echo export MUGQIC_INSTALL_HOME=/cvmfs/soft.mugqic/CentOS6 >> ~/.bashrc
echo module use $MUGQIC_INSTALL_HOME/modulefiles  >> ~/.bashrc
echo export ILAFORES=/nfs3_ib/nfs-ip34/home/def-ilafores >> ~/.bashrc

# Locally
# Windows: 
Host remote_server_ip
    ServerAliveInterval 60

# Mac
echo ServerAliveInterval 60 >> ~/.ssh/config 