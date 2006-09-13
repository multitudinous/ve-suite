"""Constants defining VE-Launcher's modes."""
from velBase import *
from velClusterDict import *
from velJconfDict import *

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

COMMAND_COVER = {"Conductor": False,
                 "NameServer": False,
                 "Xplorer": False,
                 "DesktopMode": False,
                 "Shell": False}

BASE_CONFIG = {"DependenciesDir": None,
               "BuilderDir": None,
               "Shell": False,
               "Directory": DIRECTORY_DEFAULT,
               "NameServer": True,
               "Conductor": True,
               "Xplorer": True,
               "XplorerType": 0,
               "TaoMachine": DEFAULT_TAO_MACHINE,
               "TaoPort": DEFAULT_TAO_PORT,
               "DesktopMode": False,
               "Mode": 1,
               "ClusterMaster": None,
               "JconfDict": JconfDict({"Desktop": DEFAULT_JCONF}),
               "JconfSelection": "Desktop",
               "ClusterDict": ClusterDict({}),
               "VESFile": None}
