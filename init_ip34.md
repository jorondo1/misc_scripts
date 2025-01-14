# Log onto ip34
Using your shell, log onto ip34:

	ssh your_digital_alliance_username@ip34.ccs.usherbrooke.ca
	
Each time you log onto ip34, *you must* run the following command.

	newgrp def-ilafores
Otherwise, any file you create will only be readable and writable by you.

# Initial setup 
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