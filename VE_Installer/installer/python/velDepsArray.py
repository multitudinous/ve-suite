"""Data structure for VE-Launcher's Dependencies."""
import wx
from velBase import *

class DepsArray:
    """"""
    def __init__(self, defArray = None):
        if defArray != None:
            self.array = defArray
        else:
            self.array = self.ReadEntries()

    def __len__(self):
        """Returns the length of the array."""
        return len(self.array)

    def Add(self, entry):
        self.array.append(entry)

    def Delete(self, entry):
        self.array.remove(entry)

    def GetNames(self):
        return self.array

    def ReadEntries(self):
        """Reads the list of dependencies from the config's Deps folder."""
        entries = []
        config = wx.Config.Get()
        config.SetPath(DEPS_CONFIG)
        step = config.GetFirstEntry()
        while (step[0]):
            entries.append(config.Read(step[1]))
            step = config.GetNextEntry(step[2])
        config.SetPath('..')
        return entries

    def WriteConfig(self):
        """Writes the entire list to config."""
        config = wx.Config.Get()
        config.DeleteGroup(DEPS_CONFIG)
        config.SetPath(DEPS_CONFIG)
        pos = 0
        for entry in self.array:
            config.Write("Dependency%s" % pos, entry)
            pos += 1
        config.SetPath('..')
