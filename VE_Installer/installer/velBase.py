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

##NOTE: Currently under testing. Not implemented in actual code yet.
class CoveredState:
    """Base class of the configuration. Saves base & cover vars.

    Base vars are the only ones saved; cover vars are temporary vars that
    override the base vars."""
    def __init__(self, dictionary = {}, coverLayers = 1):
        self.base = dictionary
        self.coverSheet = []
        for height in range(coverLayers):
            self.coverSheet.append({})

    def BuildRange(self, layer = None):
        """Returns an array of layers to check. Private function.

        int -> Just that layer.
        None -> All layers."""
        if layer == None:
            fields = range(len(self.coverSheet))
        else:
            fields = [layer]
        return fields
        
    def Edit(self, name, value):
        """Add/edit a variable in the base."""
        self.base[name] = value

    def Cover(self, name, value, layer = 0):
        """Covers [name] var with [value] in cover layer [layer]."""
        if name not in self.base:
            self.Edit(name, None)
        self.coverSheet[layer][name] = value

    def ImportCover(self, cover, layer = 0):
        """Replaces the old cover in [layer] with the new [cover]."""
        for name in cover:
            if name not in self.base:
                self.Edit(name, None)
        self.coverSheet[layer] = cover

    def Uncover(self, name, layer = None):
        fields = self.BuildRange(layer)
        for field in fields:
            if name in self.coverSheet[field]:
                del self.coverSheet[field][name]

    def UncoverAll(self, layer = None):
        fields = self.BuildRange(layer)
        for field in fields:
            self.coverSheet[field] = {}

    def IsCovered(self, name, value = None):
        covered = False
        for cover in self.coverSheet:
            if name in cover:
                covered = True
        return covered

    def GetBase(self):
        return self.base

    def GetCover(self, layer = None):
        cover = {}
        fields = self.BuildRange(layer)
        fields.reverse()
        for field in fields:
            for name in self.coverSheet[field]:
                if name not in cover:
                    cover[name] = self.coverSheet[field][name]
        return cover

    def GetSurface(self):
        surface = self.GetCover()
        for name in self.base:
            if name not in surface: 
                surface[name] = self.base[name]
        return surface

BASE_CONFIG = {"DependenciesDir": None,
               "BuilderDir": getcwd(),
               "WorkingDir": getcwd(),
               "NameServer": True,
               "Conductor": True,
               "Xplorer": True,
               "XplorerType": 0,
               "TaoMachine": DEFAULT_TAO_MACHINE,
               "TaoPort": DEFAULT_TAO_PORT,
               "DesktopMode": False,
               "Mode": MODE_LIST[0],
               "ClusterMaster": None,
               "JconfSelection": 0,
               "JconfDict": None,
               "ClusterDict": None}

UNAVAILABLE_COVER = 0
DEV_COVER = UNAVAILABLE_COVER + 1
VES_COVER = DEV_COVER + 1
##velCoveredConfig assumes MODE_COVER's the highest one.
MODE_COVER = VES_COVER + 1

ALT_MODE_DICT = {"Desktop": {"Conductor": True,
                             "NameServer": True,
                             "Xplorer": True,
                             "XplorerType": 0,
                             "DesktopMode": True,
                             "jconf": [FIXED, "Desktop",
                                       join(getenv("VE_INSTALL_DIR",
                                                   getcwd()),
                                                   "stereo_desktop",
                                                   "desktop.jconf")],
                             "TaoMachine": "localhost"},
             "Tablet": {"Conductor": True,
                        "NameServer": False,
                        "Xplorer": False,
                        "DesktopMode": False},
             "Computation": {"Conductor": False,
                             "NameServer": True,
                             "Xplorer": False},
             "Visualization": {"Conductor": False,
                               "NameServer": False,
                               "Xplorer": True,
                               "DesktopMode": False},
             "Shell": {"Conductor": False,
                       "NameServer": False,
                       "Xplorer": False,
                       "Shell": True},
             "Custom": {}}
               
