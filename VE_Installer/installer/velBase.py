"""Contains constant variables and shared functions for VE-Launcher."""
from os import getenv, getcwd, name ##Used for getting system values
from os.path import join ##Used for building paths.
import wx ##Used for GUI

UNIX_SHELL = getenv("SHELL", "/bin/sh") ##Shell program for the Shell mode
##Cluster features
CLUSTER_ENABLED = True
MASTER_WAIT = 10 ##Seconds to wait after starting Master to start Slaves
SLAVE_WAIT = 3 ##Seconds to wait between each Slave start
##Miscellaneous values for launcher's UI
XPLORER_SHELL_NAME = "VE-Xplorer Shell"
CONDUCTOR_SHELL_NAME = "VE-Conductor Shell"
BUILDER_SHELL_NAME = "VE-Builder Shell"
FIXED = False ##Constant var used in MODE_DICT
##File/Folder settings.
##Note: The HOME_BASE variable will be the one the installer needs to modify.
JUGGLER_FOLDER = "vrJuggler2.0.1"
VELAUNCHER_DIR = getcwd()
DIRECTORY_DEFAULT = join(getcwd(), "exampleDatasets")
LOGO_LOCATION = join(getcwd(), "installerImages", "ve_logo.xpm")
CONFIG_FILE = "VE-Suite-Launcher"
DEFAULT_CONFIG = "previous"
DEFAULT_TAO_MACHINE = "localhost"
DEFAULT_TAO_PORT = "1239"
RADIO_XPLORER_LIST = ["OpenSceneGraph", "OSG Patented",
                      "OSG Patented Cluster"]
XPLORER_TYPE_LIST = ["OSG", "OSG-VEP", "OSG-VEPC"]
MODE_LIST = ["Desktop", "Tablet", "Computation", "Visualization",
             "Shell", "Custom"]
MODE_DICT = {"Desktop": {"conductor": [FIXED, True],
                         "nameServer": [FIXED, True],
                         "xplorer": [FIXED, True],
                         "xplorerType": [FIXED, 0],
                         "desktop": [FIXED, True],
                         "jconf": [FIXED, "Desktop",
                                   join(getenv("VE_INSTALL_DIR",
                                               getcwd()),
                                               "stereo_desktop",
                                               "desktop.jconf")],
                         "taoMachine": [FIXED, "localhost"]},
             "Tablet": {"conductor": [FIXED, True],
                        "nameServer": [FIXED, False],
                        "xplorer": [FIXED, False],
                        "desktop": [FIXED, False]},
             "Computation": {"conductor": [FIXED, False],
                             "nameServer": [FIXED, True],
                             "xplorer": [FIXED, False]},
             "Visualization": {"conductor": [FIXED, False],
                               "nameServer": [FIXED, False],
                               "xplorer": [FIXED, True],
                               "desktop": [FIXED, False]},
             "Shell": {"conductor": [FIXED, False],
                       "nameServer": [FIXED, False],
                       "xplorer": [FIXED, False],
                       "shell": [FIXED, True]},
             "Custom": {}}
JCONF_CONFIG = "JconfList"
CLUSTER_CONFIG = "Cluster"
DEFAULT_JCONF = join(getcwd(), "stereo_desktop", "desktop.jconf")
##Values for launcher's GUI layout
INITIAL_WINDOW_SIZE = (500, -1)
INITIAL_JCONF_WINDOW_SIZE = (250, 250)
##BACKGROUND_COLOR = wx.Colour(236, 233, 216)
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

class VelDict:
    """Parent class of the Jconf & Cluster dictionary data classes."""
    def __init__(self):
        self.dictionary = self.ReadEntries()

    def Add(self, name, value):
        name = self.UniqueName(name)
        self.dictionary[name] = value
        return name

    def Delete(self, name):
        del self.dictionary[name]

    def Length(self):
        """Returns the length of self.dictionary."""
        return len(self.dictionary)

    def __len__(self):
        """Returns the length of self.dictionary."""
        return self.Length()

    def GetNames(self):
        """Returns a sorted list of the entries' names."""
        nList = []
        for name in self.dictionary:
            nList.append(name)
        nList.sort(lambda x, y: cmp(x.lower(), y.lower()))
        return nList

    def ReadEntries(self):
        return {}      

    def UniqueName(self, name):
        return

    def WriteConfig(self, name, value):
        return
