========================================================================
Before attempting to run VE-Suite, be sure to install the dependencies.
Versions of some of the pre-compiled dependencies are available via
an installer on our website. The others are obtainable from the respective 
sites.

See www.vesuite.org
for information on obtaining VE-Suites dependendcies.
=========================================================================
VE-Suite_0.9.3

This is the beta of the VE-Suite Launcher. Current features include:
-Setting the Dependencies directory.
-Setting the working directory from the Launcher.
-Choosing which mode to run Xplorer in.
-Launching NameServer, Xplorer and Conductor (and any combination of the three)
from one window.
-Environmental variables set externally will override the Launcher's variable
defaults, letting it work with custom builds as well.

It is currently in beta testing for Windows and Unix (especially Unix).


========================================================================
Installing VE-Suite Launcher

VE-Suite Launcher runs off Python (python.org) and wxPython (wxpython.org).
First, install the latest versions of Python and wxPython on your computer.
(Currently, there's difficulties installing wxPython on the lab computers
without administrator access.) Then move the launcherBeta.py to your VE-Suite
folder. It should be in the same location as the setup.bat or .tsh file.


========================================================================
Using VE-Suite Launcher, the Basics

Double-click launcherBeta.py or type
    python launcherBeta.py
in the command line to launch the Launcher. Since this is your first time
activating it, it will ask you to find VE-Suite Dependencies directory for it.

Once you've chosen the VE-Suite Dependencies directory, you'll see the main
Launcher window. From here, you can:
-Choose your working directory.
-Select which programs (NameServer, Conductor, Xplorer) to run.
-Select which mode to run Xplorer in.

Once you've chosen your settings, click the Launch button and wait. The Launcher
will automatically close once all the programs you've specified have launched.


========================================================================
Using VE-Suite Launcher, Custom Builds

VE-Suite Launcher can also be used with custom builds by externally overriding
some of the environmental variables it sets. The Launcher only modifies
non-existant variables (except when it appends the PATH variables). If you set
them outside the Launcher, they'll be passed to the VE-Suite programs untouched.
I recommend making a batch/shell file that sets all of the variables you want
changed, then call:
    python betaLauncher.py
as its last line.


========================================================================
The Launcher's Current Status

The VE-Suite Launcher runs on Windows machines with the last public release of
VE-Suite and the latest versions of Python and wxPython. It should run with the
latest build of VE-Suite on Windows.

It will PROBABLY run with the latest build of VE-Suite on Unix systems.  I've only tested
the underlying execute commands, not the GUI or the variable settings. Its
performance on Unix won't be known until someone runs it on Unix.


========================================================================
Launcher Code Documentation

All of the launcher's classes and functions should have docstring documentation.


========================================================================
Reporting Bugs/Suggestions:
Bugzilla:
http://www.vrac.iastate.edu/Bugzilla/

or email:
vesuite-support@iastate.edu

