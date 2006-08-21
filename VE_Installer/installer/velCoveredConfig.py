import os
from velBase import *

class CoveredConfig(CoveredState):
    def __init__(self, modifications = {}):
        CoveredState.__init__(self, BASE_CONFIG, coverLayers = MODE_COVER + 1)
        for name in modifications:
            self.Edit(name, modifications[name])

    def DevMode(self):
        self.Cover("DependenciesDir", None, layer = DEV_COVER)
        self.Cover("BuilderDir", None, layer = DEV_COVER)
        self.Cover("BuilderShell", False, layer = DEV_COVER)

    def VesArgument(self, vesFile = VELAUNCHER_DIR):
        self.Cover("WorkingDir", os.path.dirname(vesFile), layer = VES_COVER)

    def ChangeMode(self, mode):
        if mode in ALT_MODE_DICT:
            self.ImportCover(ALT_MODE_DICT[mode], layer = MODE_COVER)

    def IsEnabled(self, var):
        """Returns whether a var's uncovered and, thus, enabled."""
        return not var.IsCovered()

    def React(self, check, name, value):
        """If [check] is true, sets a cover for [name]:[value]."""
        if check:
            self.Cover(name, value, layer = UNAVAILABLE_COVER)
        else:
            self.Uncover(name, layer = UNAVAILABLE_COVER)
