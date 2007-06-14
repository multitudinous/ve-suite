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
