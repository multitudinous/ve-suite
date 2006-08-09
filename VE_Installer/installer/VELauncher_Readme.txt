========================================================================
Before attempting to run VE-Suite, be sure to install the dependencies.
Versions of some of the pre-compiled dependencies are available via
an installer on our website. The others are obtainable from the respective 
sites.

See www.vesuite.org
for information on obtaining VE-Suites dependendcies.
=========================================================================
VE-Suite1.0.2 Launcher

This is the beta of the VE-Suite Launcher. Current features include:
-Setting the Dependencies directory.
-Setting the working directory from the Launcher.
-Choosing which mode to run Xplorer in.
-Choosing configuration files for Xplorer.
-Launching NameServer, Xplorer and Conductor (and any combination of the three)
from one window.
-Choosing preset modes to run in.
-Can run the Launcher from the command line for quick launches.
-Can run the Launcher in Dev mode for developer builds.
-Start up Xplorer on multiple clustered computers.
-Start up shells for VE-Builder.

=========================================================================
Version List

1.0.2:
-Can launch shells with VE-Suite environmental variables.
-Can launch VE-Builder shells.
-Can save, load, and delete configurations of VE-Launcher.
-Cluster launching enabled for Unix.

1.0.1:
-Can now start the Launcher in Dev mode.
-Cluster launching disabled for reworking.
-Change Dependencies Folder button added.

1.0.0: Initial release.

=========================================================================
Installing VE-Suite Launcher

VE-Suite Launcher requires Python (python.org) and wxPython (wxpython.org).
The default VE Suite installation will install VE Launcher in the proper place.
Moving it out of VE Suite's directory or renaming it will cause problems.

=========================================================================
Upgrading VE-Suite Launcher

If you're upgrading from a previous version of VE-Suite, you'll need to update
which Dependencies folder VE-Launcher uses as well. Start up the Launcher,
click the Change Dependencies button, and select your latest VE-Suite Dependencies
directory.

Also, all versions of VE-Launcher use the same configuration file; you'll have to
change the Dependencies folder each time you use a different version of VE-Suite.

=========================================================================
Using VE-Suite Launcher, the Basics

Double-click velauncher.py or type
    python velauncher.py
in the command line to launch the Launcher.

NOTE: Double-clicking it might not work on Unix systems.

On your first time activating it, it will ask you to find the VE-Suite
Dependencies directory. On Windows, this is the folder named
VE_Suite.#.#.#_Dependencies. On Unix, the Dependencies directory is your system's
corresponding OS folder, tucked within the VE_Suite.#.#.#_Dependencies folder.

Once you've chosen the VE-Suite Dependencies directory, you'll see the main
Launcher window. From here, you can:
-Change your Dependencies directory.
-Choose your working directory.
-Choose the Computing Engine's name and port.
-Choose which mode to run in.

The Mode Settings button lets you view each mode's settings and, in the case
of the Visualization & Custom modes, change them. The Mode Settings window
shows Xplorer's current configuration, format, and which programs will
be launched. You can also set up cluster nodes to call if you're on a Unix cluster.

Once you've chosen your settings, click the Launch button and wait. The Launcher
will automatically close once all the programs have launched.

========================================================================
Using VE-Suite Launcher, Custom Builds

VE-Suite Launcher can also be used with custom builds by externally overriding
some of the environmental variables it sets. The Launcher only modifies
non-existant variables (except when it appends the PATH variables). If you set
them outside the Launcher, they'll be passed to the VE-Suite programs untouched.
I recommend making a batch/shell file that sets all of the variables you want
changed, then call:
    python velauncher.py --dev
as its last line. Run the batch/shell whenever you want to use the custom build.

========================================================================
Using VE_Suite Launcher, Command Line Launch

You can also give arguments to the Launcher from the command line to
immediately launch it. The options are:

<none>: Start the velauncher GUI.
--dev: Start the velauncher GUI in developer mode. Doesn't work with any
other arguments.

-c, --conductor: Launch VE Conductor.
-n, --nameserver: Launch VE NameServer.
-x <xplorer type>, --xplorer=<xplorer type>: Launch VE Xplorer in
<xplorer type> mode. You can choose OSG, OSG-VEP, or OSG-VEPC.

-s, --shell: Launches a subshell with the VE-Suite environmental
variables set. Overrides -c, -n, and -x.
-b <builder dir>, --builder=<builder dir>: Launches a subshell
with the VE-Suite environmental variables set, including a path
to the VE-Builder directory. Overrides -c, -n, -x and -s.

-k, --desktop: Set VE Conductor and VE Xplorer to Desktop mode.
-j <filepath>, --jconf=<filepath>: Use <filepath> as VE Xplorer's
Juggler configuration.
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

The VE-Suite Launcher runs on Windows as a standalone executable.
It runs on Unix machines with Python 2.3 and the last public releases of
VE-Suite and wxPython. Mac compatibility and Windows cluster functionality
are slated for future versions.

========================================================================
Launcher Code Documentation

The launcher's classes and functions have docstring documentation.

========================================================================
Reporting Bugs/Suggestions:
Bugzilla:
http://www.vrac.iastate.edu/Bugzilla/

or email:
vesuite-support@iastate.edu

