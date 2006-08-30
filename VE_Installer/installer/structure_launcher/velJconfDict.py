"""Data structure for VE-Launcher's Jconf dictionary."""
import wx
import os
from velBase import *
from velDict import *

class JconfDict(VelDict):
    """Stores a list of Jconf pairs in this setup:
    {name: path, name: path..}
    under the variable self.dictionary.

    Functions:
        __init__()
        Add(name, path)
        Rename(oldName, newName)
        Delete(name)
        UniqueName(name)
        GetPath(name)
        Length() / __len__
        GetNames()
        WriteConfig()
    """
    def __init__(self, preset = {}):
        """Creates a list of .jconf names/paths from the Launcher's Config."""
        VelDict.__init__(self, preset)

    def ReadEntries(self):
        entries = {}
        config = wx.Config.Get()
        config.SetPath(JCONF_CONFIG)
        step = config.GetFirstEntry()
        while (step[0]):
            entries[step[1]] = config.Read(step[1])
            step = config.GetNextEntry(step[2])
        config.SetPath('..')
        return entries

    def Rename(self, oldName, newName):
        """Renames the entry at oldName to newName.

        Returns newName, modified until it's unique on the list."""
        ##Don't do anything if newName == pos's old name
        if newName == oldName:
            return newName
        ##Change name if it matches another one on the list.
        path = self.dictionary[oldName]
        self.Delete(oldName)
        finalName = self.Add(newName, path)
        ##Return true if name had to be changed.
        return finalName

    def UniqueName(self, name):
        """Replaces name with a unique name in the list."""
        ##Add a numeric suffix to name if it matches a name
        ##already in the list.
        suffix = ""
        maxCount = 0
        curNames = self.GetNames()
        while curNames.count(name + str(suffix)) > maxCount:
            if suffix == "":
                suffix = '1'
            else:
                suffix = str(int(suffix) + 1)
        return name + str(suffix)

    def GetPath(self, name):
        """Returns the path of name's entry."""
        if name in self.dictionary:
            return self.dictionary[name]
        else:
            return "None"

    def WriteConfig(self):
        """Writes the entire list to config."""
        config = wx.Config.Get()
        config.SetPath(JCONF_CONFIG)
        for name in self.dictionary:
            config.Write(name, self.dictionary[name])
        config.SetPath('..')
