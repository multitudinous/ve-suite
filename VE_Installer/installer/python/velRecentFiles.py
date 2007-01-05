"""Manages VE-Launcher's recent files list."""
import wx
import os
from velBase import *

class RecentFiles:
    """Stores the paths to the last 'count' files opened.
    
    Older files have higher numbers in the array."""
    def __init__(self, preset = None, count = RECENT_COUNT):
        """Creates a list of .jconf names/paths from the Launcher's Config."""
        self.count = count
        if preset == None:
            self.recentArray = self.ReadEntries()
        else:
            self.recentArray = preset
        self.Trim()

    def Add(self, addition):
        """Adds the single item 'addition' to the array.
        If addition is already is array, remove it and reinsert it at the front.
        If the array + addition is over self.count, pop items until it isn't."""
        if addition in self.recentArray:
            self.recentArray.remove(addition)
        self.recentArray.insert(0, addition)
        self.Trim()

    def GetNames(self):
        """Returns an array of file names for the recent files."""
        ##Add a numeric suffix to name if it matches a name
        ##already in the list.
        nameArray = []
        for entry in self.recentArray:
            nameArray.append(os.path.basename(entry))
        return nameArray

    def IsEmpty(self):
        """Returns true if array's empty, false if it isn't."""
        if len(self.recentArray) != 0:
            return False
        else:
            return True

    def Clear(self):
        """Clears the entire list of recent files."""
        self.recentArray = []

    def GetPath(self, indexNumber):
        """Returns the path of entry at indexNumber."""
        if indexNumber < len(self.recentArray):
            return self.recentArray[indexNumber]
        else:
            return None

    def Trim(self):
        """Trims self.recentArray down to self.count size."""
        while len(self.recentArray) > self.count:
            self.recentArray.pop()

    def ReadEntries(self):
        entries = []
        config = wx.Config.Get()
        config.SetPath(RECENTFILES_CONFIG)
        place = 0
        while place < self.count and config.Exists("Recent%s" % (place)):
            entries.append(config.Read("Recent%s" % (place)))
            place += 1
        config.SetPath('..')
        return entries

    def WriteConfig(self):
        """Writes the entire list to config."""
        config = wx.Config.Get()
        config.DeleteGroup(RECENTFILES_CONFIG)
        config.SetPath(RECENTFILES_CONFIG)
        place = 0
        while place < len(self.recentArray):
            config.Write("Recent%s" % (place), self.recentArray[place])
            place += 1
        config.SetPath('..')
        return
