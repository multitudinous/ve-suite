Installation instructions of launching the cluster

unpack and install the psexec.exe executible and place it in the C:\Windows\System32 directory on the master node.

Place the run.osg.vep.clusterLauncher.bat files into the VE_SUITE_HOME/bin directory of each node.

Edit this file to denote the correct location of the Juggler config files.

Place the runWindowsLauncher.bat file to a convenient location on the master node.

Edit the runWindowsLauncher.bat file to reflect the names of the slave node locations (should be denoted within
the file)

This should be all that is needed for file manipulation and editing.



TO RUN

Start the naming service and VE_Conductor like normal.

Open a command prompt on the master node.  Go to the directory where the runWindowsLauncher.bat file is located.

Type in the following:
	
	runWindowsLauncher.bat *****.param

Currently, the Working directory is set in your setup.bat file on each machine, and the ***.param is the parameter
file needed to start whatever app is in the working directory.  I wanted to do this right now for testing.  later
we can have the working directory passed in as an argument.