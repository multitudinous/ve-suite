"""Data structure for VE-Launcher's Cluster dictionary."""
import wx
import os
from velBase import *
from velDict import *

class ClusterDict(VelDict):
    """Stores a dictionary of cluster comps in this setup:
    {location: True, location: True,...}
    under the variable self.list.

    Functions:
        __init__([preset])
        *Add(name, value)
        *Delete(name)
        *Length / *__len__
        *GetNames
        ReadEntries
        GetLocations
        UniqueName(name)
        WriteConfig    
    * Defined in VelDict."""
    def __init__(self, preset = None):
        """Creates a dict of cluster names/locations from Launcher's Config."""
        VelDict.__init__(self, preset)

    def ReadEntries(self):
        """Reads the list of slaves from the config's Cluster folder."""
        entries = {}
        config = wx.Config.Get()
        config.SetPath(CLUSTER_CONFIG)
        step = config.GetFirstGroup()
        while (step[0]):
            entries[step[1]] = True
            step = config.GetNextGroup(step[2])
        config.SetPath('..')
        return entries

    def GetLocations(self):
        """Returns a name list."""
        return self.GetNames()

    def UniqueName(self, name):
        """Returns the name passed. No modification."""
        return name

    def WriteConfig(self):
        """Writes the entire list to config."""
        config = wx.Config.Get()
        config.SetPath(CLUSTER_CONFIG)
        for location in self.dictionary:
            ##Make a group named location, but put redundant info in it.
            ##Groups left in for future functionality.
            config.SetPath(location)
            config.Write(location, "True")
            config.SetPath('..')
        config.SetPath('..')
