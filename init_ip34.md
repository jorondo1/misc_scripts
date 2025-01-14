# Create a DRAC account
To work on ip34, you need a [Digital Research Alliance of Canada](https://ccdb.alliancecan.ca/security/login) account. Ask your director for their sponsorship code.

# Setup your shell locally
This will periodically send a signal to ip34 to make sure you don't spontaneously get logged out.

## Mac 
Open the Terminal app and run the following: 

	echo "Host *" >>  ~/.ssh/config 
	echo ServerAliveInterval 60 >> ~/.ssh/config 

## Windows
1. Navigate to your home folder, typically under `C:\Users:\<username>\.ssh
2. Open the config file; if there is none, right-click inside the .ssh folder and select New > Text Document
3. Add the following lines to the file :

```
Host remote_server_ip
	ServerAliveInterval 60
 ```	
Save it and quit. 
Then, open the PowerShell app and proceed with the next steps.
# Log onto ip34
To access ip34, run this command with your DRAC username (will prompt for password; nothing shows when you'll write your password, that's normal!)

	ssh <username>@ip34.ccs.usherbrooke.ca
	
Once you are logged in, *you must* run the following command, each and every time.

	newgrp def-ilafores
	
Otherwise, any file you create will only be readable and writable by you.

# Initial setup 
You'll only need to do this once, ever. It will add a few lines to your `.bashrc` file, which is executed every time you login. 

	echo umask 007 >> ~/.bashrc
	echo export MUGQIC_INSTALL_HOME=/cvmfs/soft.mugqic/CentOS6 >> ~/.bashrc
	echo module use $MUGQIC_INSTALL_HOME/modulefiles  >> ~/.bashrc
	echo export ILAFORES=/nfs3_ib/nfs-ip34/home/def-ilafores >> ~/.bashrc
	. ~/.bashrc 
	
This will allow others within your group (def-ilafores) to write/execute files you've created, allow you to access certain programs on mugqic, and finally will create a simple variable you can use to navigate to the def-ilafores directory, which is where you should generally work from. To get there and for example create a new project under the analysis folder, I suggest adding another variable to your bashrc so it's always accessible (choose a name you'll remember):

	echo "export PROJECT1=$ILAFORES/analysis/your_project_name" >> ~/.bashrc
	. ~/.bashrc

Again, these last two commands only need to be done once (the second one simply re-executes the bashrc so your PROJECT1 variable gets created. Then, whenever you log back onto ip34, you can just do (using the project directory variable name you've chosen, here PROJECT1) :

	mkdir $PROJECT1
	cd $PROJECT1
	
