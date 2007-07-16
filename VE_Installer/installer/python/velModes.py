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
"""Constants defining VE-Launcher's modes."""
from velBase import *
from velClusterDict import *
from velJconfDict import *
from velRecentFiles import *
from velDepsArray import *

##JCONF_BASE_PATH is a workaround so velauncher.py can be used
##in both the base VE-Suite directory & the VE-Suite/bin directory.
JCONF_BASE_PATH = VELAUNCHER_DIR
if os.path.basename(JCONF_BASE_PATH) == "bin":
    JCONF_BASE_PATH = os.path.dirname(JCONF_BASE_PATH)
##Set default Jconf files.
DEFAULT_JCONF = join(JCONF_BASE_PATH, "share", "vesuite","vrj_configs", "stereo_desktop", "desktop.jconf")
DEFAULT_DEV_JCONF = join(JCONF_BASE_PATH, "..", "..", "share",
                         "stereo_desktop", "desktop.jconf")
MODE_LIST = ["Desktop", "Tablet", "Computation", "Visualization", "Shell", "Custom"]
COMP_ADD_MODE_LIST = ["Manual", "From File"]
#COMP_ADD_MODE_LIST = ["Manual", "Auto Fill", "From File"]

MODE_DICT = {"Desktop": {"Conductor": True,
                         "NameServer": True,
                         "Xplorer": True,
                         "XplorerType": DEFAULT_SOLO_XPLORER,
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
                  "Shell": False}

DEV_CONFIG = {"DependenciesDir": None,
              "BuilderDir": None,
              "BuilderShell": False,
              "DevMode": True}

BASE_CONFIG = {"DependenciesDir": None,
               "Dependencies": DepsArray([]),
               "JugglerDep": None,
               "BuilderDir": None,
               "BuilderShell": None, ##Auto-Launch Builder?
               "Shell": False,
               "Directory": DIRECTORY_DEFAULT,
               "NameServer": True,
               "Conductor": True,
               "Xplorer": True,
               "XplorerType": DEFAULT_SOLO_XPLORER,
               "TaoMachine": DEFAULT_TAO_MACHINE,
               "TaoPort": DEFAULT_TAO_PORT,
               "DesktopMode": False,
               "AutoShutDown": False,
               "Mode": 0,
               "AddMode": 0, ##Adding computer name mode in cluster mode
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
               "RecentFiles": RecentFiles([]),
               "AutoRunVes": False,
               "ExtraVariables": "",
               "RunDebugPrograms": False}
##               "FileDir": DIRECTORY_DEFAULT}
