========================================================================
Before attempting to run VE-Suite, be sure to install the dependencies.
Versions of some of the pre-compiled dependencies are available via
an installer on our website. The others are obtainable from the respective 
sites.

See www.vesuite.org
for information on obtaining VE-Suites dependendcies.
=========================================================================
VE-Suite_0.9.0

This windows installer places components of VE-Suite on your computer.
Three optional components are available from this installer:

Name Server -- Communications Engine
VE-Xplorer -- Graphics Engine
VE-Conductor -- Graphical User Interface

A sample dataset is also optionally installed.

After running the installer, you must edit your setup file 
./setup.bat

to reflect your environment. The variables defined in the
setup.bat file to tell VE-Suite where its dependencies are located,as well
as setting up the working directory.  

For all installs set:
VE_INSTALL_DIR ==> the location of the install (example: C:\VE_Suite.9.0)

For VE-Xplorer make sure to set:
VE_WORKING_DIR ==> the location of the parameter file containing the model 
information

For VE-Conductor an optional variable may be used:
VE_USER_PLUGIN_DIR ==> the location of any GUI plugins you may have developed

After setting the enviroment, run the installed components from the start menu,
under the VE-Suite.0.9.0 program folder.

To view the sample dataset, type sample.param in VE-Xplorer cmd window when 
prompted.
========================================================================
NOTES:

========================================================================
Visit: 

www.vesuite.org

for more detailed information on setting environment variables and
getting started using VE-Suite.
=========================================================================
