========================================================================
Before attempting to run VE-Suite, be sure to install the dependencies.
Compliant versions of the pre-compiled dependencies are available via
a tarball on our website. The others are obtainable from the respective 
sites.

See www.vesuite.org
for information on obtaining VE-Suites dependendcies.
=========================================================================
                          VE-Suite_1.0.*
=========================================================================
The tarball creates a directory structure containing the binaries and exe
needed for the core VE-Suite application and utilities.

The directory structure should look like:
./{Platform}/bin
./{Platform}/images
./{Platform}/exampleDatasets
./{Platform}/images
./{Platform}/README.txt
./{Platform}/setup.tsh

where {Platform} corresponds to the platform that that VE-Suite was build on.


Before running, you must edit your setup file: 
./{Platform}/setup.tsh

to reflect your environment. The variables defined in the
setup.tsh file to tell VE-Suite where its dependencies are located.

The important variables are:
VE_INSTALL_DIR ==> the location of the install (example: /home/user/VE_Suite.9.0/{Platform)
VE_DEPS_DIR ==> the location of the dependencies contained in the tarball(example: /home/user/VE_Suite_deps.9.0/{Platform)

=========================================================
Running VE-Suite
=========================================================
1)Set the environment by sourcing the setup.tsh file, for example:

/home/user/joeengineer/VE_Suite.0.9.0/{Platform} >source setup.tsh

This will enable you to run VE-Suite and it's utilities from any directory.
Included in the install is a shell script to help launch the various utilities and
components of VE-Suite. After the environment is set, type:

>VES

at the prompt to see the various options and uses for this script.

========================================================================
NOTES:
- A Name Service must be started before running VE-Xplorer or attempting to
connect from VE-Conductor. 

- VE-Xplorer, currently must be run from the directory that contains your
model data.

========================================================================
Visit: 

www.vesuite.org

for more detailed information on setting environment variables and
getting started using VE-Suite.
=========================================================================
