"""Contains constant variables and basic shared functions for VE-Launcher."""
from os import getenv, name ##Used for getting system values
from os.path import join ##Used for building paths.
import wx ##Used for GUI
import sys

##Set up the system's ID.
windows = (name == "nt")
unix = (name == "posix" or name == "mac")
##Shell variables.
UNIX_SHELL = getenv("SHELL", "/bin/sh") ##Shell program for the Shell mode
##Cluster variables.
CLUSTER_ENABLED = True
##MASTER_WAIT = 7 ##Seconds to wait after starting Master to start Slaves.
##SLAVE_WAIT = 5 ##Seconds to wait between each Slave.
##File/Folder settings.
JUGGLER_FOLDER = "vrJuggler2.0.1"
VELAUNCHER_DIR  = sys.path[0] ##The directory velauncher.py is in.
DIRECTORY_DEFAULT = join(VELAUNCHER_DIR, "exampleDatasets")
LOGO_LOCATION = join(VELAUNCHER_DIR, "installerImages", "ve_logo.xpm")
SPLASH_IMAGE = join(VELAUNCHER_DIR, "installerImages", "ve_banner_1.0.bmp")
SPLASH_TIME = 7000 ##milliseconds before auto-close
if windows:
    CLUSTER_FILE_PATH = "C:\\" + join("WINDOWS", "Temp", "cluster.bat")
else:
    CLUSTER_FILE_PATH = join(VELAUNCHER_DIR, "cluster.tsh")
TEMPLATE_PATH = join(VELAUNCHER_DIR, "clusterTemplate.txt")
##Config settings.
CONFIG_FILE = "VE-Suite-Launcher"
DEFAULT_CONFIG = "previous"
JCONF_CONFIG = "JconfList"
CLUSTER_CONFIG = "Cluster"
##Default setting values.
DEFAULT_TAO_MACHINE = "localhost"
DEFAULT_TAO_PORT = "1239"
RADIO_XPLORER_LIST = ["OpenSceneGraph", "OSG Patented",
                      "OSG Patented Cluster"]
XPLORER_TYPE_LIST = ["OSG", "OSG-VEP", "OSG-VEPC"]
##Settings for launcher's GUI layout.
INITIAL_WINDOW_SIZE = (500, -1)
INITIAL_JCONF_WINDOW_SIZE = (250, 250)
INITIAL_DIALOG_SIZE = INITIAL_JCONF_WINDOW_SIZE
KILL_WINDOW_SIZE = (200, 100)
JCONF_LIST_DISPLAY_MIN_SIZE = (100, 50)
XPLORER_SHELL_NAME = "VE-Xplorer Shell"
CONDUCTOR_SHELL_NAME = "VE-Conductor Shell"
BUILDER_SHELL_NAME = "VE-Builder Shell"
BACKGROUND_COLOR = wx.Colour(200, 200, 200)
TOP_SPACE = (75, 75)
BORDER = 5
VERTICAL_SPACE = (-1, BORDER)
HORIZONTAL_SPACE = (BORDER, -1)
LEFT_MARGIN = HORIZONTAL_SPACE
NULL_SPACE = (0, 0)
##Covers for the CoveredConfig.
##Higher-numbered covers "cover up" lower-numbered covers.
UNAVAILABLE_LAYER = 0
DEV_LAYER = UNAVAILABLE_LAYER + 1
VES_LAYER = DEV_LAYER + 1
MODE_LAYER = VES_LAYER + 1
TOTAL_LAYERS = MODE_LAYER + 1

def Style(window):
    """The uniform style of each window in VE Launcher."""
    ##Set the background color.
    window.SetBackgroundColour(BACKGROUND_COLOR)
    return

def usage():
    """Prints a list of acceptable arguments for command line velauncher.py."""
    print """
LEGAL ARGUMENTS FOR VELAUNCHER.PY
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
-m <name>, --master=<name>: Set VEXMASTER to <name>."""
    return
