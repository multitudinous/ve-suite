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

    def Rename(self, oldName, newName):
        """Renames the entry at oldName to newName.

        Returns newName.
        Assumes newName isn't already on the list."""
        ##Don't do anything if newName == pos's old name
        if newName == oldName:
            return newName
        ##Add the name.
        finalName = self.Add(newName, self.dictionary[oldName])
        self.Delete(oldName)
        ##Return true if name had to be changed. Should never return true.
        return finalName

    def GetLocations(self):
        """Returns a name list."""
        return self.GetNames()

    def UniqueName(self, name):
        """Returns True if name not in dict, False if it is."""
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
