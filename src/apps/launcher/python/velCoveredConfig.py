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
"""Data structure for VE-Launcher's config data."""
import os
from velCoveredState import *
from velBase import *
from velModes import *

class CoveredConfig(CoveredState):
    """Data structure for VE-Launcher's config data.

    Contains data in multiple layers that can cover each other up.
    The Base layer contains raw user input.
    The other layers contain changes to it made by modes, devMode,
    VES files, or as reactions to the user input.
    The Base layer is saved, while the Surface view is sent to Launch.

    Functions:
        __init__([modifications])
        DevMode
        CommandLineMode
        VesArgument(vesFile)
        ChangeMode(mode)
        IsEnabled(var)
        React(check, name, value)
        GetLaunchSurface
        *BuildRange([layer])
        *Edit(name, value)
        *Cover(name, value, [layer])
        *ImportCover(cover, [layer])
        *Uncover(name, [layer])
        *UncoverAll([layer])
        *IsCovered(name)
        *GetBase([var])
        *GetCover([layer, var])
        *GetSurface([var])
    * Defined in CoveredState."""
    def __init__(self, modifications = {}):
        """Initializes a CoveredConfig for velauncher."""
        CoveredState.__init__(self, BASE_CONFIG, coverLayers = TOTAL_LAYERS)
        for name in modifications:
            self.Edit(name, modifications[name])
        if unix:
            self.Cover("BuilderDir", VELAUNCHER_DIR, layer = UNAVAILABLE_LAYER)

    def DevMode(self):
        """Applies developer mode to the launcher."""
        for var in DEV_CONFIG:
            self.Cover(var, DEV_CONFIG[var], layer = SETTINGS_LAYER)

    def CommandLineMode(self):
        """Applies changes to base for Command Line mode."""
        for var in COMMAND_CONFIG:
            self.Cover(var, COMMAND_CONFIG[var], layer = COMMAND_LINE_LAYER)

    def InterpretArgument(self, argument):
        """Takes a filename argument, removes other loaded files, loads argument."""
        vesFile = None
	scriptFile = None
        if argument:
            if argument[-4:] == '.ves':
                vesFile = argument
            else:
                scriptFile = argument
        self.SetVesFile(vesFile)
	self.SetScript(scriptFile)
        return

    def SetVesFile(self, vesFile):
        """Applies a .ves argument to the launcher."""
        if vesFile:
##            self.Cover("Directory", os.path.dirname(os.path.abspath(vesFile)),
##                       layer = VES_LAYER)
            self.Edit("Directory", os.path.dirname(os.path.abspath(vesFile)))
            self.Edit("VESFile", vesFile)
            ##Ensure only one file loaded at a time.
            self.SetScript(None)
        else:
##            self.UncoverAll(VES_LAYER)
            self.Edit("VESFile", None)
        self.ChangeMode(MODE_LIST[self.GetSurface("Mode")])
        return

    def SetScript(self, scriptFile):
        """Applies a script (.bat, .tsh, .sh) argument to the launcher."""
        if scriptFile:
##            self.Cover("Directory",
##                       os.path.dirname(os.path.abspath(scriptFile)),
##                       layer = SCRIPT_LAYER)
            self.Edit("Directory", os.path.dirname(os.path.abspath(scriptFile)))
            self.Edit("ShellScript", scriptFile)
            self.Cover("Shell", True, layer = FILE_LAYER)
            self.Cover("BuilderDir", None, layer = FILE_LAYER)
            self.Cover("BuilderShell", False, layer = FILE_LAYER)
            self.Cover("Mode", 4, layer = FILE_LAYER)
            ##Ensure only one file loaded at a time.
            self.SetVesFile(None)
        else:
            self.UncoverAll(FILE_LAYER)
            self.Edit("ShellScript", None)
        self.ChangeMode(MODE_LIST[self.GetSurface("Mode")])
        return

    def ChangeMode(self, mode):
        """Changes the launcher's mode."""
        if mode in MODE_DICT:
            self.ImportCover(MODE_DICT[mode], layer = MODE_LAYER)

    def IsEnabled(self, var):
        """Returns whether a var's uncovered and, thus, enabled."""
        return not self.IsCovered(var)

    def React(self, check, name, value):
        """If [check] is true, sets a cover for [name]:[value]."""
        if check:
            self.Cover(name, value, layer = UNAVAILABLE_LAYER)
        else:
            self.Uncover(name, layer = UNAVAILABLE_LAYER)

    def JconfPath(self):
        """Returns the path of the currently-selected Juggler configuration."""
        return self.GetSurface("JconfDict").GetPath(self.GetSurface("JconfSelection"))

    def FileLoaded(self):
        """Returns True if a file's loaded, False if one isn't."""
        return self.GetSurface("VESFile") or self.GetSurface("ShellScript")

    def GetLaunchSurface(self):
        """Specialized GetSurface for the Launch.

        Replaces all None in surface with "None" to avoid errors in
        path joinings.
        Appends JconfPath and ClusterNodes to the dictionary."""
        surface = self.GetSurface()
##        for var in surface:
##            if surface[var] == None:
##                surface[var] = "None"
        ##Determine the JconfPath.
        surface["JconfPath"] = self.JconfPath()
        ##Set ClusterNodes to list of cluster nodes if a cluster is launched.
        if surface["Xplorer"] and surface["XplorerType"] == "OSG-VEPC" \
           and surface["ClusterMaster"] and surface["ClusterDict"]:
            surface["ClusterSlaves"] = surface["ClusterDict"].GetNames()
            surface["Cluster"] = True
##            surface["ClusterNodes"].append(surface["ClusterMaster"])
        else:
##            print "No slaves found!" ##TESTER
            surface["ClusterSlaves"] = None
            surface["Cluster"] = False
        ##If OSGNotifyLevel == "None", change it to None
        if surface["OSGNotifyLevel"] == "None":
            surface["OSGNotifyLevel"] = None
        ##Change JugglerDep
        if surface["JugglerDep"] == None:
            surface["JugglerDep"] = "None"
        ##Split ExtraVars
        surface["ExtraVariables"] = CreateListFromText(surface["ExtraVariables"])
        return surface
