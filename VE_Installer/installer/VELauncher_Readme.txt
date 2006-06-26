========================================================================
Before attempting to run VE-Suite, be sure to install the dependencies.
Versions of some of the pre-compiled dependencies are available via
an installer on our website. The others are obtainable from the respective 
sites.

See www.vesuite.org
for information on obtaining VE-Suites dependendcies.
=========================================================================
VE-Suite_0.9.3 Launcher

This is the beta of the VE-Suite Launcher. Current features include:
-Setting the Dependencies directory.
-Setting the working directory from the Launcher.
-Choosing which mode to run Xplorer in.
-Choosing which Jconf to use for Xplorer.
-Launching NameServer, Xplorer and Conductor (and any combination of the three)
from one window.
-Environmental variables set externally will override the Launcher's variable
defaults, letting it work with custom builds as well.

It is currently in beta testing for Windows and Unix.

=========================================================================
What's new:

Revision 4586: Cluster GUI added. It doesn't do anything on Windows yet, but
when launched on Unix with Xplorer's OSG Cluster selected, it sends test commands to
every checked computer on the cluster list.

=========================================================================
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
in the command line to launch the Launcher.

NOTE: Double-clicking it might not work on Unix systems. More testing needed.

Since this is your first time activating it, it will ask you to find
the VE-Suite Dependencies directory for it.

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
Using VE_Suite Launcher, Command Line Launch

You can also give arguments to the Launcher from the command line to
immediately launch it. The options are:

<none>: Start the velauncher GUI.

-c, --conductor: Launch VE Conductor.
-n, --nameserver: Launch VE NameServer.
-x <xplorer type>, --xplorer=<xplorer type>: Launch VE Xplorer in <xplorer type> mode. You can choose OSG, OSG-VEP, or OSG-VEPC.

-k, --desktop: Set VE Conductor and VE Xplorer to Desktop mode.
-j <filepath>, --jconf=<filepath>: Use <filepath> as VE Xplorer's Juggler configuration.
-w <dir>, --dir=<dir>: Set the Working directory to <dir>.

-t <name>, --taomachine=<name>: Set TAOMACHINE to <name>.
-p <port>, --port=<port>: Set TAOPORT to <port>.
-e <dir>, --dep=<dir>: Set the Dependencies directory to <dir>.
-m <name>, --master=<name>: Set VEXMASTER to <name>.

If you leave out -c, -n, -x, or -k, the Launcher will set those options to
False. If you leave out -j, -t, -p, -w, or -e, the Launcher will use their
saved custom values. (This will cause errors if you try to use a command line
launch without setting a Dependencies directory in GUI mode, for example.)

========================================================================
The Launcher's Current Status

The VE-Suite Launcher runs on Windows & Unix machines with the last public
release of VE-Suite and the latest versions of Python and wxPython. It should
run with the latest build of VE-Suite on Windows and Unix systems.

Since using different Jconf files for Xplorer is still in the works, the
Launcher comes with a JCONF_STANDARD override. When it's set to True, the program
will use the standard Jconf files to run Xplorer instead of the ones you chose.
To use your chosen Jconf files, edit velauncher.py, go to line 27, and set
JCONF_STANDARD to False.

There is currently a minor error in the Unix version where the Launcher window
will remain after the launch. This is because the Xplorer takes over the Launcher's
terminal to get the user's input. It won't affect VE-Suite (unless you try to
Force Quit it, which will quit VE-Suite as well) and will be fixed once Xplorer
doesn't require the user to type in the .param file.

---Cluster Testing---

Cluster testing functions have been added to the Unix launch code. To test them,
first set CLUSTER_TEST (line 33) to True. Then set CLUSTER_PACKAGE (line 34) to
the names of every other computer in the test cluster. Finally, change the command
variable (line 1823) to the commands you want to use. When you send command line
arguments to VE Launcher in Unix (see above), the Cluster test will automatically
run.

========================================================================
Launcher Code Documentation

All of the launcher's classes and functions should have docstring documentation.


========================================================================
Reporting Bugs/Suggestions:
Bugzilla:
http://www.vrac.iastate.edu/Bugzilla/

or email:
vesuite-support@iastate.edu

