This file contains info about building and maintaining rpms for VE-Suite.

Source RPMS by default dump the spec files in /usr/src/packages/SPECS and the 
sources in /usr/src/packages/SOURCES. If you have the SuSE build package 
installed, you can run the command unrpm and they will be dumped into the current 
directory instead. A source RPM contains everything necessary to build the binary 
RPM, including the sources and patches used, and the .spec file. A binary RPM 
only contains the built binaries as specified in the .spec file; multiple binary 
RPMs can have a single source RPM, if they are included in the same .spec file. 

VE-Suite builds in a chroot environment using the SuSE build script.

You can rpm -Uvh a .src.rpm to place the .spec file in /usr/src/packages/SPECS OR 
if you have SuSE build installed, you can run /usr/lib/build/pac_unpack or 
unrpm (depending on your version of build).  build is included in the SLE SDK.  
You can also get the latest version at:
 http://software.opensuse.org/download/openSUSE:/Tools/ 

It is recommended using build to build the .spec files as it creates a chroot 
environment that allows you to build independent of the packages installed on the 
system.

The current spec files are only tested on SuSE Linux Enterprise Desktop. These 
files need to be tested on other platforms and will require finding rpms for 
some of our dependencies.
