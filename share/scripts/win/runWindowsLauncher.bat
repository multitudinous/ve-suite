REM Double click this to run the Windows Launching script

REM This bat file will start VE-Xplorer on the main node and then it will recursively go to the other nodes.
REM The syntax is to type in the name of this bat file and pass in the paramfile.

REM For this to work, psexec needs to be installed on the master machine, and the user needs admin priveleges on the machines.
REM Copy this psexec into the the default location of C:\WINDOWS\System32

REM add the run.osg.vep.clusterLauncher.bat file to the VE_SUITE_HOME/bin directory on each of the nodes

REM start the naming service and VE_Conductor on the master node like usual

REM REPLACE THE MACHINE NAMES DENOTED BELOW WITH THE CORRECT NAMES OF THE MACHINES

C:\VE_Suite.0.9.3\bin\run.osg.vep.clusterLauncher.bat

sleep 5

REM REPLACE THE MACHINE NAMES DENOTED BELOW WITH THE CORRECT NAMES OF THE MACHINE
psexec \\machine2 C:\VE_Suite.0.9.3\bin\run.osg.vep.clusterLauncher.bat

sleep 5

REM REPLACE THE MACHINE NAMES DENOTED BELOW WITH THE CORRECT NAMES OF THE MACHINE
psexec \\machine3 C:\VE_Suite.0.9.3\bin\run.osg.vep.clusterLauncher.bat

sleep 5 

REM REPLACE THE MACHINE NAMES DENOTED BELOW WITH THE CORRECT NAMES OF THE MACHINE
psexec \\machine4 C:\VE_Suite.0.9.3\bin\run.osg.vep.clusterLauncher.bat


cmd
