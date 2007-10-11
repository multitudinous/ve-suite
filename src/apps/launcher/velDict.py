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
"""Parent class of Jconf & Cluster dictionaries."""

class VelDict:
    """Parent class of the Jconf & Cluster dictionary data classes.

    Functions:
        __init__([preset])
        Add(name, value)
        Delete(name)
        Length / __len__
        GetNames
        ReadEntries [empty]
        UniqueName(name) [empty]
        WriteConfig [empty]"""
    def __init__(self, preset = None):
        """Initializes dictionary to Config if no presets are sent."""
        if preset == None:
            self.dictionary = self.ReadEntries()
        else:
            self.dictionary = {}
            for entry in preset:
                self.dictionary[entry] = preset[entry]

    def Add(self, name, value):
        """Adds [name: value] to dictionary."""
        name = self.UniqueName(name)
        self.dictionary[name] = value
        return name

    def Delete(self, name):
        """Deletes name from dictionary."""
        del self.dictionary[name]

    def Length(self):
        """Returns the length of self.dictionary."""
        return len(self.dictionary)

    def __len__(self):
        """Returns the length of self.dictionary."""
        return self.Length()

    def GetNames(self):
        """Returns a sorted list of the entries' names."""
        nList = []
        for name in self.dictionary:
            nList.append(name)
        nList.sort(lambda x, y: cmp(x.lower(), y.lower()))
        return nList

    def ReadEntries(self):
        """Reads entries from config and places them in dictionary."""
        return {}      

    def UniqueName(self, name):
        """Returns a unique name in dictionary for name."""
        return

    def WriteConfig(self):
        """Writes entries into config from self.dictionary. Empty."""
        """Writes dictionary to config."""
        return
