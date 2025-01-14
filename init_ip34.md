A quick guide to access ip34 (aka Mammouth) ! Steps 0, 1 and 3 only need to be done once. If you need a quick primer on the bash language syntax, which what the code blocks below are written in, checkout Mike Lee's [awesome tutorial](https://astrobiomike.github.io/unix/unix-intro). 

# 0. Create a DRAC account
To work on ip34, you need a [Digital Research Alliance of Canada](https://ccdb.alliancecan.ca/security/login) account. Ask your director for their sponsorship code.

# 1. Initial setup (locally)
This will periodically send a signal to ip34 to make sure you don't spontaneously get logged out. _You only need to do this once._

### Mac 
Open the Terminal app and run the following: 

	echo "Host *" >>  ~/.ssh/config 
	echo ServerAliveInterval 60 >> ~/.ssh/config 

### Windows
1. Navigate to your home folder, typically under `C:\Users:\<username>\.ssh
2. Open the config file; if there is none, right-click inside the .ssh folder and select New > Text Document
3. Add the following lines to the file :

```
Host remote_server_ip
	ServerAliveInterval 60
 ```	
Save it and quit. 
Then, open the PowerShell app and proceed with the next steps.

# 2. Log onto ip34
To access ip34, run this command with your DRAC username (will prompt for password; nothing shows when you'll write your password, that's normal!)

	ssh <username>@ip34.ccs.usherbrooke.ca
	
Once you are logged in, *you must* run the following command, each and every time.

	newgrp def-ilafores
	
Otherwise, any file you create will only be readable and writable by you.

# 3. Initial setup (ip34)
_You'll only need to do this once, ever._ It will add a few lines to your `.bashrc` file, which is executed every time you login. 

	echo umask 007 >> ~/.bashrc
	echo export MUGQIC_INSTALL_HOME=/cvmfs/soft.mugqic/CentOS6 >> ~/.bashrc
	echo module use $MUGQIC_INSTALL_HOME/modulefiles  >> ~/.bashrc
	echo export ILAFORES=/nfs3_ib/nfs-ip34/home/def-ilafores >> ~/.bashrc
	. ~/.bashrc        # This simply executes the contents of the .bashrc file
	
This setup will allow others within your group (def-ilafores) to write/execute files you've created, allow you to access certain programs on mugqic, and finally will create a simple variable you can use to navigate to the def-ilafores directory, which is where you should generally work from. To get there and, for example, create a new project (under the analysis folder would be the best place), I suggest adding another variable to your `.bashrc` that will let you use the path to your project directory without having to type it in its entirety every time.

	echo "export PROJECT1=$ILAFORES/analysis/your_project_name" >> ~/.bashrc
	. ~/.bashrc
	mkdir $PROJECT1

Again, these last two commands only need to be done once. Then, whenever you log in to ip34, using the project directory variable name you've chosen (here PROJECT1), all you have to do is

	newgrp def-ilafores # don't forget ! ;-)
	cd $PROJECT1
	
And you can start doing science!

# Loading the R environment 

If you need extra juice to run a massive DADA2 pipeline your laptop can't handle, for example, you can load the R environment within ip34.

	module load StdEnv/2023 r/4.4.0 mugqic/cutadapt/2.10
	R

Once you run this, you are now in the equivalent of an R console, which accepts R commands. You'll need to install the packages you need only once, as you once did on your computer. By default, your current directory will be the one you were in when you activated R. 

### Multithread support of ip34
The cool thing about ip34 is that there are 96 cores available. So whenever the function your run has a `threads` or `cores` argument or something like that, you can choose how many to use. *However, keep in mind that you are not alone on ip34!* It is generally frowned upon to huddle all the cores. Try to limit yourself to 24 for most purposes. If you need more, there are 1500 computing nodes that can be used, but this requires a little extra legwork in terms of scripting (but you're having fun so that shouldn't be an issue!).

# Having stuff run in the background
Imagine you want to run DADA2's sample inference on hundreds of samples... that might take a while! If you want to go home and let it run while you binge-watch Friends, you can either let your laptop running (not recommended), or use a cool little tool called `tmux` directly on ip34. Essentially, tmux creates a virtual shell that runs in the background. If you close your ip34 session (or just log out), it keeps running. Within that tmux shell, you can run anything like you would on your normal shell. To get it running, log in to ip34, get where you need to be, then run:

	tmux
	
The text will disappear on your shell and a green line will appear at the bottom. That's it, you are in a tmux session! You can do anything you would normally have, like load your R environment and execute some commands. Once your code is running, you can log out by using the shortcut `Ctrl+B` on your keyboard, then pressing `D`. `Ctrl+B` lets you send one command to control tmux from within a tmux session, and here we use `D`, which means to Detach from the tmux session. You'll be back to where you were before creating the tmux session. You can check open sessions by doing

	tmux ls
	
You first session will be session 0, and you can reattach to it by doing 

	tmux a -t 0
	
And you'll see your code is still running.








	
