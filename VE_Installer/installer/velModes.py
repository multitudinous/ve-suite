"""Constants defining VE-Launcher's modes."""
from velBase import *
from velClusterDict import *
from velJconfDict import *
from velRecentFiles import *

DEFAULT_JCONF = join(VELAUNCHER_DIR, "stereo_desktop", "desktop.jconf")
DEFAULT_DEV_JCONF = join(VELAUNCHER_DIR, "..", "..", "VE_Xplorer",
                         "stereo_desktop", "desktop.jconf")
MODE_LIST = ["Desktop", "Tablet", "Computation", "Visualization",
             "Shell", "Custom"]
MODE_DICT = {"Desktop": {"Conductor": True,
                         "NameServer": True,
                         "Xplorer": True,
                         "XplorerType": 1,
                         "DesktopMode": True,
                         "JconfDict": JconfDict({"Desktop": DEFAULT_JCONF}),
                         "JconfSelection": "Desktop",
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

COMMAND_CONFIG = {"Conductor": False,
                  "NameServer": False,
                  "Xplorer": False,
                  "DesktopMode": False,
                  "Shell": False,
                  "Debug": False}

BASE_CONFIG = {"DependenciesDir": None,
               "BuilderDir": None,
               "BuilderShell": None, ##Auto-Launch Builder?
               "Shell": False,
               "Directory": DIRECTORY_DEFAULT,
               "NameServer": True,
               "Conductor": True,
               "Xplorer": True,
               "XplorerType": 0,
               "TaoMachine": DEFAULT_TAO_MACHINE,
               "TaoPort": DEFAULT_TAO_PORT,
               "DesktopMode": False,
               "Mode": 0,
               "ClusterMaster": "",
               "JconfDict": JconfDict({"Desktop": DEFAULT_JCONF}),
               "JconfSelection": "Desktop",
               "ClusterDict": ClusterDict({}),
               "VESFile": None,
               "VPRDebug": -1,
               "OSGNotifyLevel": "None",
               "MasterWait": 7,
               "SlaveWait": 1,
               "User": "",
               "ShellScript": None,
               "DevMode": False,
               "Debug": False,
               "RecentFiles": RecentFiles([])}
##               "FileDir": DIRECTORY_DEFAULT}
