========================================================================
Before attempting to run VE-Suite, be sure to install the dependencies.
Versions of some of the pre-compiled dependencies are available via
an installer on our website. The others are obtainable from the respective 
sites.

See www.vesuite.org
for information on obtaining VE-Suite's dependendcies.
=========================================================================
VE-Suite Launcher

The VE-Suite Launcher lets you launch VE-Suite from multiple saved configurations.
Features include:-Setting Dependencies, Working, and Builder directories.
-Preset modes for quick launching.
-Can run the Launcher from the command line for quicker launches.
-Cluster mode lets you start Xplorer on multiple computers from one.

=========================================================================
Version List

1.0.2:
-Can launch shells with VE-Suite environmental variables.
-Can launch VE-Builder shells.
-Can save, load, and delete configurations of VE-Launcher.
-Cluster launching.
-Can set debug levels.
-Can load .ves, .bat, .sh, and .tsh files for running.

1.0.1:
-Can now start the Launcher in Dev mode.
-Cluster launching disabled for reworking.
-Change Dependencies Folder button added.

1.0.0: Initial release.

=========================================================================
Installing VE-Suite Launcher

VE-Suite Launcher requires Python 2.3+ (python.org) and wxPython 2.6.3.3 (wxpython.org).
The default VE Suite installation will install VE Launcher in the proper place.
Moving it out of VE Suite's directory or renaming it will cause problems.

=========================================================================
Upgrading VE-Suite Launcher

If you're upgrading from a previous version of VE-Suite, you'll need to update
which Dependencies folder VE-Launcher uses as well. Start up the Launcher,
click the Change Dependencies button, and select your latest VE-Suite Dependencies
directory.

=========================================================================
Using VE-Suite Launcher, the Basics

Double-click velauncher.py or type
    python velauncher.py
in the command line to launch the Launcher.

On your first time activating it, it will ask you to find the VE-Suite
Dependencies directory. On Windows, this is the folder named
VE_Suite.#.#.#_Dependencies. On Unix, the Dependencies directory is your system's
corresponding OS folder, tucked within the VE_Suite.#.#.#_Dependencies folder.

Once you've chosen the VE-Suite Dependencies directory, you'll see the main
Launcher window. From here, you can:
-Choose your working directory.
-Choose the Computing Engine's name and port.
-Choose which mode to run in.

More advanced option, such as the Dependencies directory, Builder directory,
and debug options are available in the menu.

The Mode Settings button lets you view each mode's settings and, in the case
of the Visualization & Custom modes, change them. The Mode Settings window
shows Xplorer's current configuration, format, and which programs will
be launched. You can also set up cluster nodes to call if you're running in
cluster mode.

Once you've chosen your settings, click the Launch button and wait. The Launcher
will launch all the programs you chose and exit.
========================================================================
Using VE-Suite Launcher, Clusters

VE-Launcher versions 1.0.2+ have extra features for automatically starting up
VE-Suite on other computers in a cluster. (Cluster functions are slated
for Windows in the future.) VE-Launcher writes an automated script during
launch to connect to each node, set up a duplicate environment, and start up Xplorer.

--UNIX Clusters--
To use the cluster functions of VE-Launcher, your cluster must satisfy
these requirements:
1. VE-Launcher's user has permission to ssh to each node.
2. The file structure of each node is the same.
3. The user has manually sshed into each node and authenticated its name.

The last requirement needs some explanation. The first time you ssh into an
unknown host, you need to manually authenticate its name. If it isn't
authenticated and VE-Launcher tries to connect to it, it will hang the program.
Therefore, you must manually ssh into each node on the cluster (including your
current computer) and authenticate it before you can start the cluster using
VE-Launcher. Furthermore, you must use the exact name you'll use in VE-Launcher;
ssh treats partially-qualified (francis) and fully-qualified (francis.iastate.edu)
names as separate authentications. Luckily, once you authenticate the other nodes,
the authentication stays in the user's profile permentantly.

Once you have all the nodes authenticated, you can run the cluster from VE-Launcher:

1. If you're running NameServer or Conductor on a certain node, open VE-Launcher
from that node. (VE-Launcher runs NameServer and Conductor from the current computer.)

2. Choose the settings. When you select the Xplorer type OSG Patented Cluster,
the Set Cluster Computers button will be enabled.

3. Click Set Cluster Computers. A window will pop up showing the master and
slaves listed. Put the master's name in, then add each slave's name to the list.
Click OK when you're done.

4. Launch VE-Suite by clicking the Launch button. The cluster computers
will start up VE-Suite.

Note there's a slight delay between each node launched. That's because
simultaneous launching causes erratic behavior in the cluster. The delays can be
modified under the Options->Cluster Wait Times menu. If you're encountering problems
with sync or unconnected nodes using VE-Launcher, try lengthening the delays.
If it's taking too long to launch the cluster, try shortening them.

--Windows Clusters--
The Windows cluster mode uses psexec to connect to the other nodes. Some clusters might
require you to connect as a certain user; VE-Suite can automatically pass a username but
not a password (for security reasons). You can also enter code into the clusterTemplate.txt
file if you need to insert your own code into the cluster launch script; see clusterTemplate.txt
for more info.

To run the cluster from VE-Launcher:

1. If you're running NameServer or Conductor on a certain node, open VE-Launcher
from that node. (VE-Launcher runs NameServer and Conductor from the current computer.)

2. Choose the settings. When you select the Xplorer type OSG Patented Cluster,
the Set Cluster Computers button will be enabled.

3. Click Set Cluster Computers. A window will pop up showing the master and
slaves listed, plus an optional username field. Put the master's name in,
then add each slave's name to the list. If you need to connect to the other nodes as
a particular user, enter the username as well. Click OK when you're done.

4. Launch VE-Suite by clicking the Launch button. The cluster computers
will start up VE-Suite.

Note there's a slight delay between each node launched. That's because
simultaneous launching causes erratic behavior in the cluster. The delays can be
modified under the Options->Cluster Wait Times menu. If you're encountering problems
with sync or unconnected nodes using VE-Launcher, try lengthening the delays.
If it's taking too long to launch the cluster, try shortening them.

========================================================================
Using VE-Suite Launcher, Custom Builds

VE-Suite Launcher can also be used with custom builds by externally overriding
some of the environmental variables it sets. If you set them outside the Launcher,
they'll be passed to the VE-Suite programs untouched.
I recommend making a batch/shell file that sets all of the variables you want
changed, then call:
    python velauncher.py --dev
as its last line. Run the batch/shell whenever you want to use the custom build.

========================================================================
Using VE_Suite Launcher, Command Line Launch

You can also give arguments to the Launcher from the command line to
immediately launch it. The options are:

<none>: Start the velauncher GUI.
--dev: If passed alone, starts the velauncher GUI in developer mode.
If passed with other options, sets the launch environment to developer mode.

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
-v <file>, --ves=<file>: Sets the VES file to pass in. Overrides -w.

-t <name>, --taomachine=<name>: Set TAOMACHINE to <name>.
-p <port>, --port=<port>: Set TAOPORT to <port>.
-e <dir>, --dep=<dir>: Set the Dependencies directory to <dir>.
-m <name>, --master=<name>: Set VEXMASTER to <name>.

<filepath> argument: Passes the file to VE-Launcher for loading.

If you leave out -c, -n, -x, -s, -b or -k, the Launcher will set those options
to False. If you leave out -j, -t, -p, -w, or -e, the Launcher will use their
saved configuration values. (This will cause errors if you try to use a command
line launch without setting a Dependencies directory in GUI mode, for example.)

========================================================================
The Launcher's Current Status

The VE-Suite Launcher runs on Windows as a standalone executable.
It runs on Unix machines with Python 2.3 or higher and wxPython 2.6.3.3.

========================================================================
Launcher Code Documentation

The launcher's classes and functions have docstring documentation.

========================================================================
Reporting Bugs/Suggestions:
Bugzilla:
http://www.vrac.iastate.edu/Bugzilla/

or email:
vesuite-support@iastate.edu

