"""Contains constant variables and shared functions for VE-Launcher."""
from os import getenv, name ##Used for getting system values
from os.path import join, dirname ##Used for building paths.
import wx ##Used for GUI
import sys
import getopt

UNIX_SHELL = getenv("SHELL", "/bin/sh") ##Shell program for the Shell mode
##Cluster features
CLUSTER_ENABLED = True
MASTER_WAIT = 7 ##Seconds to wait after starting Master to start Slaves
SLAVE_WAIT = 5 ##Seconds to wait between each Slave start
##Miscellaneous values for launcher's UI
XPLORER_SHELL_NAME = "VE-Xplorer Shell"
CONDUCTOR_SHELL_NAME = "VE-Conductor Shell"
BUILDER_SHELL_NAME = "VE-Builder Shell"
##File/Folder settings.
##Note: The HOME_BASE variable will be the one the installer needs to modify.
JUGGLER_FOLDER = "vrJuggler2.0.1"
##The directory velauncher.py is in.
VELAUNCHER_DIR  = sys.path[0]
DIRECTORY_DEFAULT = join(VELAUNCHER_DIR, "exampleDatasets")
LOGO_LOCATION = join(VELAUNCHER_DIR, "installerImages", "ve_logo.xpm")
CONFIG_FILE = "VE-Suite-Launcher"
DEFAULT_CONFIG = "previous"
DEFAULT_TAO_MACHINE = "localhost"
DEFAULT_TAO_PORT = "1239"
RADIO_XPLORER_LIST = ["OpenSceneGraph", "OSG Patented",
                      "OSG Patented Cluster"]
XPLORER_TYPE_LIST = ["OSG", "OSG-VEP", "OSG-VEPC"]
JCONF_CONFIG = "JconfList"
CLUSTER_CONFIG = "Cluster"
##Values for launcher's GUI layout
MENU_DEP_ID = 500
INITIAL_WINDOW_SIZE = (500, -1)
INITIAL_JCONF_WINDOW_SIZE = (250, 250)
BACKGROUND_COLOR = wx.Colour(200, 200, 200)
JCONF_LIST_DISPLAY_MIN_SIZE = (100, 50)
TOP_SPACE = (75, 75)
BORDER = 5
VERTICAL_SPACE = (-1, BORDER)
HORIZONTAL_SPACE = (BORDER, -1)
LEFT_MARGIN = HORIZONTAL_SPACE
NULL_SPACE = (0, 0)
KILL_WINDOW_SIZE = (200, 100)
##Set up the system ID
windows = (name == "nt")
unix = (name == "posix" or name == "mac")

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
-m <name>, --master=<name>: Set VEXMASTER to <name>."""
    return

UNAVAILABLE_COVER = 0
DEV_COVER = UNAVAILABLE_COVER + 1
VES_COVER = DEV_COVER + 1
MODE_COVER = VES_COVER + 1
TOTAL_COVERS = MODE_COVER + 1
