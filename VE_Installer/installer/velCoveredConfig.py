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
        self.Cover("DependenciesDir", None, layer = DEV_LAYER)
        self.Cover("BuilderDir", None, layer = DEV_LAYER)
        self.Cover("BuilderShell", False, layer = DEV_LAYER)
        self.Cover("DevMode", True, layer = DEV_LAYER)

    def CommandLineMode(self):
        """Applies changes to base for Command Line mode."""
        for var in COMMAND_CONFIG:
            self.Edit(var, COMMAND_CONFIG[var])

    def InterpretArgument(self, argument):
        if argument:
##            self.Edit("FileDir", os.path.dirname(os.path.abspath(argument)))
            if argument[-4:] == '.ves':
                self.SetVesFile(argument)
            else:
                self.SetScript(argument)
        else:
            self.SetVesFile(None)
            self.SetScript(None)
        return

    def SetVesFile(self, vesFile):
        """Applies a .ves argument to the launcher."""
        if vesFile:
##            self.Cover("Directory", os.path.dirname(os.path.abspath(vesFile)),
##                       layer = VES_LAYER)
            self.Edit("Directory", os.path.dirname(os.path.abspath(vesFile)))
            self.Cover("VESFile", vesFile, layer = VES_LAYER)
            ##Ensure only one file loaded at a time.
            self.SetScript(None)
        else:
            self.UncoverAll(VES_LAYER)
        self.ChangeMode(MODE_LIST[self.GetSurface("Mode")])
        return

    def SetScript(self, scriptFile):
        """Applies a script (.bat, .tsh, .sh) argument to the launcher."""
        if scriptFile:
##            self.Cover("Directory",
##                       os.path.dirname(os.path.abspath(scriptFile)),
##                       layer = SCRIPT_LAYER)
            self.Edit("Directory", os.path.dirname(os.path.abspath(scriptFile)))
            self.Cover("ShellScript", scriptFile, layer = SCRIPT_LAYER)
            self.Cover("Shell", True, layer = SCRIPT_LAYER)
            self.Cover("BuilderDir", None, layer = SCRIPT_LAYER)
            self.Cover("BuilderShell", False, layer = DEV_LAYER)
            self.Cover("Mode", 4, layer = SCRIPT_LAYER)
            ##Ensure only one file loaded at a time.
            self.SetVesFile(None)
        else:
            self.UncoverAll(SCRIPT_LAYER)
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
        if surface["Xplorer"] and surface["XplorerType"] == 2 \
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
        return surface
