"""Contains constant variables and basic shared functions for VE-Launcher."""
from os import getenv, name ##Used for getting system values
import os.path ##Used for building paths.
import wx ##Used for GUI
import sys
join = os.path.join

##Set up the system's ID.
windows = (name == "nt")
unix = (name == "posix" or name == "mac")
##CFD_HOST_TYPE: Set below.
##Shell variables.
UNIX_SHELL = getenv("SHELL", "/bin/sh") ##Shell program for the Shell mode
##Cluster variables.
CLUSTER_ENABLED = True
##File/Folder settings.
JUGGLER_FOLDER = "vrJuggler2.0.1"
VELAUNCHER_DIR  = sys.path[0] ##The directory velauncher.py is in.
##Fixes directory passed for velauncher to vesuite.
if os.path.basename(VELAUNCHER_DIR) == "velauncher":
    VELAUNCHER_DIR = os.path.dirname(VELAUNCHER_DIR)
DIRECTORY_DEFAULT = join(os.path.dirname(VELAUNCHER_DIR), "exampleDatasets")
##Image settings.
IMAGES_DIR = join(VELAUNCHER_DIR, "..", "share", "installerImages")
if not os.path.isdir(IMAGES_DIR):
    IMAGES_DIR = join(VELAUNCHER_DIR, "..", "..", "share", "installerImages")
print IMAGES_DIR ##TESTER
LOGO_LOCATION = join(IMAGES_DIR, "ve_logo.xpm")
SPLASH_IMAGE = join(IMAGES_DIR, "velauncher_banner.bmp")
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
DEPS_CONFIG = "Dependencies"
RECENTFILES_CONFIG = "RecentFiles"
RECENT_COUNT = 10
RECENT_MENU_ID = 600
##Default setting values.
DEFAULT_TAO_MACHINE = "localhost"
DEFAULT_TAO_PORT = "1239"
RADIO_XPLORER_LIST = ["Non-Cluster", "Cluster"]
XPLORER_TYPE_LIST = ["OSG-VEP", "OSG-VEPC"]
DEFAULT_SOLO_XPLORER = XPLORER_TYPE_LIST[0]
DEFAULT_CLUSTER_XPLORER = XPLORER_TYPE_LIST[1]
##Settings for launcher's GUI layout.
INITIAL_WINDOW_SIZE = (500, -1)
INITIAL_JCONF_WINDOW_SIZE = (250, 250)
INITIAL_DIALOG_SIZE = INITIAL_JCONF_WINDOW_SIZE
KILL_WINDOW_SIZE = (200, 100)
JCONF_LIST_DISPLAY_MIN_SIZE = (100, 50)
DEPS_LIST_DISPLAY_MIN_SIZE = (200, 50)
XPLORER_SHELL_NAME = "VE-Xplorer Shell"
CONDUCTOR_SHELL_NAME = "VE-Conductor Shell"
LAUNCHER_SHELL_NAME = "VE-Launcher Shell"
BACKGROUND_COLOR = wx.Colour(200, 200, 200)
READONLY_COLOR = wx.LIGHT_GREY
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
SCRIPT_LAYER = VES_LAYER + 1
MODE_LAYER = SCRIPT_LAYER + 1
TOTAL_LAYERS = MODE_LAYER + 1

def CFDHostType():
    ##Set CFDHOSTNAME
    if windows:
        cfdHostType = "WIN32"
    elif unix:
        if (os.path.exists("/etc/redhat-release")):
            piped = os.popen("""cat /etc/redhat-release """ +
                             """| awk -F" " '{print $1}'""", 'r')
            firstWord = piped.read()[:-1]
            ##NOTE: [:-1] is to remove the line break from the read()
            piped.close()
            if firstWord == "Red":
                piped = os.popen("""cat /etc/redhat-release """ +
                                 """| awk -F" " '{print $3}'""", 'r')
                thirdWord = piped.read()[:-1]
                piped.close()
                if thirdWord == "Enterprise":
                    ##Extract words from file to create similar to RHEL_3
                    piped= os.popen("""cat /etc/redhat-release """ +
                                    """| awk -F" " '{print "RHEL_" $7}'""",
                                    'r')
                    cfdHostType = piped.read()[:-1]
                    piped.close()
                else:
                    ##Extract words from file to create
                    ##something like RedHat_8.0
                    piped = os.popen("""cat /etc/redhat-release """ +
                                     """| awk -F" " '""" +
                                     """{print $1 $2 "_" $5}'""",
                                     'r')
                    cfdHostType = piped.read()[:-1]
                    piped.close()
            elif firstWord == "Fedora":
                ##Extract words from file to create something like Fedora_1
                piped= os.popen("""cat /etc/redhat-release """ +
                                """| awk -F" " '{print $1 "_" $4}'""", 'r')
                cfdHostType = piped.read()[:-1]
                piped.close()
            else:
                ##NOTE: If the program couldn't identify this type of
                ##Redhat, just use uname.
                piped = os.popen("uname")
                cfdHostType = piped.read()[:-1]
                piped.close()
        elif os.path.exists("/etc/SuSE-release"):
            ##Extract words from file to create
            ##something like SuSE_9.2_x86-64
            piped = os.popen("""head -1 /etc/SuSE-release """ +
                             """| awk -F" " '{print $1 "_" $3 "_" $4}'""",
                             'r')
            cfdHostType = piped.read()[:-1]
            piped.close()
        else:
            piped = os.popen("uname")
            cfdHostType = piped.read()[:-1]
            piped.close()
        ##If CFDHOSTTYPE has parentheses, remove them.
        piped = os.popen("""echo \"%s\" """ %cfdHostType+
                         """| sed -e 's/(//g' | sed -e 's/)//g' """ + 
                         """| sed -e 's/"//g'""", 'r')
        cfdHostType = piped.read()[:-1]
        piped.close()
    return cfdHostType
CFD_HOST_TYPE = CFDHostType()
##print CFD_HOST_TYPE ##TESTER

def Style(window):
    """The uniform style of each window in VE Launcher."""
    ##Set the background color.
    window.SetBackgroundColour(BACKGROUND_COLOR)
    return

def EnvVarEmpty(var):
    """Returns whether var is empty or not.
    
    True if var == None, var's length is 0, or var is all whitespace.
    Otherwise False."""
    if os.getenv(var) == None or len(str(os.getenv(var))) == 0 \
       or str(os.getenv(var)).isspace():
        return True
    else:
        return False

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
