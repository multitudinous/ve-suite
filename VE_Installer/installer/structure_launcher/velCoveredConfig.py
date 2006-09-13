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

    def DevMode(self):
        """Applies developer mode to the launcher."""
        self.Cover("DependenciesDir", None, layer = DEV_LAYER)
        self.Cover("BuilderDir", None, layer = DEV_LAYER)
        self.Cover("BuilderShell", False, layer = DEV_LAYER)
        self.Cover("DevMode", True, layer = DEV_LAYER)

    def CommandLineMode(self):
        """Applies changes to base for Command Line mode."""
        for var in COMMAND_LAYER:
            self.state.Edit(var, COMMAND_LAYER[var])

    def VesArgument(self, vesFile):
        """Applies a .ves argument to the launcher."""
        self.Cover("Directory", os.path.dirname(vesFile), layer = VES_LAYER)
        self.Cover("VESFile", vesFile)

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

    def GetLaunchSurface(self):
        """Specialized GetSurface for the Launch.

        Replaces all None in surface with "None" to avoid errors in
        path joinings."""
        surface = self.GetSurface()
        for var in surface:
            if surface[var] == None:
                surface[var] = "None"
        return surface
