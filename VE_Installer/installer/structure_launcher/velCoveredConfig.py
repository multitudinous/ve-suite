import os
from velCoveredState import *
from velBase import *
from velModes import *

class CoveredConfig(CoveredState):
    def __init__(self, modifications = {}):
        CoveredState.__init__(self, BASE_CONFIG, coverLayers = MODE_COVER + 1)
        for name in modifications:
            self.Edit(name, modifications[name])

    def DevMode(self):
        self.Cover("DependenciesDir", None, layer = DEV_COVER)
        self.Cover("BuilderDir", None, layer = DEV_COVER)
        self.Cover("BuilderShell", False, layer = DEV_COVER)
        self.Cover("DevMode", True, layer = DEV_COVER)

    def VesArgument(self, vesFile = VELAUNCHER_DIR):
        self.Cover("Directory", os.path.dirname(vesFile), layer = VES_COVER)
        self.Cover("VESFile", vesFile)

    def ChangeMode(self, mode):
        if mode in MODE_DICT:
            self.ImportCover(MODE_DICT[mode], layer = MODE_COVER)

    def IsEnabled(self, var):
        """Returns whether a var's uncovered and, thus, enabled."""
        return not self.IsCovered(var)

    def React(self, check, name, value):
        """If [check] is true, sets a cover for [name]:[value]."""
        if check:
            self.Cover(name, value, layer = UNAVAILABLE_COVER)
        else:
            self.Uncover(name, layer = UNAVAILABLE_COVER)
