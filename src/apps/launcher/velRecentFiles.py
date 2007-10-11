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

    def __len__(self):
        return len(self.recentArray)

    def Add(self, addition):
        """Adds the single item 'addition' to the array.
        If addition is already is array, remove it and reinsert it at the front.
        If the array + addition is over self.count, pop items until it isn't."""
        if addition in self.recentArray:
            self.recentArray.remove(addition)
        self.recentArray.insert(0, addition)
        self.Trim()

    def Delete(self, entry):
        """Deletes entry from the recent array."""
        try:
            index = self.recentArray.index(entry)
        except ValueError:
            ##print "Recent Files Error: %s not in array." % (entry)
            return
        self.DeleteIndex(index)

    def DeleteIndex(self, index):
        """Deletes item at index from the recent array."""
        try:
            self.recentArray.pop(index)
        except IndexError:
            print "Recent Files Error: accessing index %s out of %s." % (index, len(self.recentArray))

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
