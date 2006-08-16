"""Data structure for VE-Launcher's Cluster dictionary."""
import wx
import os
from velBase import *

class ClusterDict(VelDict):
    """Stores a dictionary of cluster comps in this setup:
    {location: True, location: True,...}
    under the variable self.list.

    Functions:
        __init__()
        Add(location)
        Delete(name)
        GetLocation(index)
        Length / __len__
        GetNames()
        GetLocations()
        GetCheckedLocations()
        WriteConfig()
    """
    def __init__(self):
        """Creates a dict of cluster names/locations from Launcher's Config."""
        VelDict.__init__(self)
##        self.cluster = {}
##        config.SetPath(CLUSTER_CONFIG)
##        bCont = config.GetFirstGroup()
##        while (bCont[0]):
##            name = bCont[1]
##            config.SetPath(bCont[1])
##            location = config.Read("location", "localhost")
##            config.SetPath('..')
##            self.cluster[location] = True
##            bCont = config.GetNextGroup(bCont[2])
##        config.SetPath('..')

    def ReadEntries(self):
        entries = {}
        config = wx.Config.Get()
        config.SetPath(CLUSTER_CONFIG)
        step = config.GetFirstGroup()
        while (step[0]):
            entries[step[1]] = True
            step = config.GetNextGroup(step[2])
        config.SetPath('..')
        return entries

##    def Add(self, location):
##        """Adds location: True to the list & config."""
##        ##Add to list.
##        self.cluster[location] = True
##        ##Add to cluster.
##        config.SetPath(CLUSTER_CONFIG)
##        config.SetPath(location)
##        config.Write("location", location)
##        config.SetPath('..')
##        config.SetPath('..')
##        ##NOTE: Adding the same cluster file twice results in the previous one
##        ##being overwritten. This is normal functioning now.

    def GetLocations(self):
        return self.GetNames()

    def UniqueName(self, name):
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
