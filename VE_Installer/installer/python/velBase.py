# /*************** <auto-copyright.pl BEGIN do not edit this line> *************
# *
# * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
# *
# * Original Development Team:
# *   - ISU's Thermal Systems Virtual Engineering Group,
# *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
# *   - Reaction Engineering International, www.reaction-eng.com
# *
# * This library is free software; you can redistribute it and/or
# * modify it under the terms of the GNU Library General Public
# * License as published by the Free Software Foundation; either
# * version 2 of the License, or (at your option) any later version.
# *
# * This library is distributed in the hope that it will be useful,
# * but WITHOUT ANY WARRANTY; without even the implied warranty of
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# * Library General Public License for more details.
# *
# * You should have received a copy of the GNU Library General Public
# * License along with this library; if not, write to the
# * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# * Boston, MA 02111-1307, USA.
# *
# * -----------------------------------------------------------------
# * Date modified: $Date$
# * Version:       $Rev$
# * Author:        $Author$
# * Id:            $Id$
# * -----------------------------------------------------------------
# *
# *************** <auto-copyright.pl END do not edit this line> **************
"""Contains constant variables and basic shared functions for VE-Launcher."""
from os import getenv, name ##Used for getting system values
import os.path ##Used for building paths.
import wx ##Used for GUI
import sys
join = os.path.join

##Set up the system's ID.
windows = (name == "nt")
unix = (name == "posix" or name == "mac")
posix = (name == "posix")
##CFD_HOST_TYPE: Set below.
##Shell variables.
UNIX_SHELL = getenv("SHELL", "/bin/sh") ##Shell program for the Shell mode
##Cluster variables.
CLUSTER_ENABLED = True
##File/Folder settings.
JUGGLER_FOLDER = "VRJuggler2.0.3-1"
##Set the directory velauncher.py is in.
##Method differs based on whether it's python or frozen executable.
if hasattr(sys, 'frozen') and sys.frozen == 1:
    VELAUNCHER_DIR = os.path.realpath(os.path.abspath(sys.executable))
else:
    VELAUNCHER_DIR = sys.path[0]

##Fixes directory passed for velauncher to vesuite.
if "velauncher" in os.path.basename(VELAUNCHER_DIR):
    VELAUNCHER_DIR = os.path.dirname(VELAUNCHER_DIR)
DIRECTORY_DEFAULT = join(os.path.dirname(VELAUNCHER_DIR),
                         "share", "vesuite", "examples")

##Image settings.
IMAGES_DIR = join(os.path.dirname(VELAUNCHER_DIR), "bin", "installerImages")
if not os.path.isdir(IMAGES_DIR):
    IMAGES_DIR = join(VELAUNCHER_DIR, "installerImages")
LOGO_LOCATION = join(IMAGES_DIR, "ve_logo.xpm")
SPLASH_IMAGE = join(IMAGES_DIR, "velauncher_banner.xpm")
SPLASH_TIME = 7000 ##milliseconds before auto-close
if windows:
    ##Change this if cluster.bat can't write to its directory.
    CLUSTER_FILE_PATH = "C:\\" + join("WINDOWS", "Temp", "cluster.bat")
else:
    from os import getuid ##Used for grabbing user id
    from pwd import getpwuid ##Used for grabbing user id
    userName = getpwuid(os.getuid())[0]
    CLUSTER_FILE_PATH = join('/', 'var', 'tmp', "cluster.%s.tsh" %(userName))
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
INITIAL_DEBUG_WINDOW_SIZE = (500, 400)
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
  ##Used in text input lists we need to separate.
SEPARATOR_CHARS = [',', ':', ';']

##Covers for the CoveredConfig.
##Higher-numbered covers "cover up" lower-numbered covers.
UNAVAILABLE_LAYER = 0
SETTINGS_LAYER = UNAVAILABLE_LAYER + 1
FILE_LAYER = SETTINGS_LAYER + 1
MODE_LAYER = FILE_LAYER + 1
COMMAND_LINE_LAYER = MODE_LAYER + 1
TOTAL_LAYERS = COMMAND_LINE_LAYER + 1

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

class QuitLaunchError(Exception):
    """Defines error raised if user manually quits launch."""
    def __init__(self, value = "User quit launch"):
        self.value = value
    def __str__(self):
        return repr(self.value)

class NonexistantConfigError(Exception):
    """Defines error raised if VE-Launcher config 'value' doesn't exist."""
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

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


def CreateListFromText(textBlock):
    """Turns a text list into an array list.
    
    First, replaces all chars in SEPARATOR_CHARS with spaces.
    Then, splits the textBlock based on whitespace."""
    for charToReplace in SEPARATOR_CHARS:
        textBlock = textBlock.replace(charToReplace, ' ')
    return textBlock.split()


def usage():
    """Prints a list of acceptable arguments for command line velauncher.py."""
    print """
LEGAL OPTIONS FOR VELAUNCHER:

<none>: Start the velauncher GUI.

<file>: Passes a .ves or shell file to velauncher. Will automatically run it
        if the Auto-Run Passes File menu choice is checked.

-d, --dev: If passed alone, starts the velauncher GUI in developer mode.
           If passed with other options, sets the launch environment to
           developer mode.

-q, --quick: Immediately runs VE-Suite with its last settings.

-g <config>, --config=<config>: Immediately runs VE-Suite using the named
                                VE-Launcher configuration.

-w <dir>, --dir=<dir>: Set the Working directory to <dir>.

-t <name>, --taomachine=<name>: Set TAOMACHINE to <name>.

-p <port>, --port=<port>: Set TAOPORT to <port>.

-j <filepath>, --jconf=<filepath>: Use <filepath> as VE Xplorer's
                                   Juggler configuration.

-b, --debug: Debug VE-Launcher's launch sequence.

-l <boolean>, --cluster=<boolean>: Set whether VE-Suite runs
                                   in Cluster mode.

PROGRAM-LAUNCHING OPTIONS:
If any of these are used, only the VE-Suite programs specified with options
will launch, despite previous settings.
(Example: If you pass "-c -x", only Conductor and Xplorer will launch.)

-c, --conductor: Launch VE Conductor.

-n, --nameserver: Launch VE NameServer.

-x, --xplorer: Launch VE Xplorer.

-s, --shell: Launches a subshell with the VE-Suite environmental
             variables set, including the VE-Builder directory.
"""
    return

def version():
    """Prints VE-Suite's version."""
    print """VE-Suite v1.0.6"""
    return
