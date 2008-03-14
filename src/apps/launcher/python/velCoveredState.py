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
"""Contains CoveredState, data structure base class."""
class CoveredState:
    """Base class of the configuration. Saves base & cover vars.

    This class is structured like sheet of paper (the base) overlapped
    by a series of transparencies (the covers) that cover up certain values
    with their own.

    Getting the Base just returns the base's values (for saving user configs),
    while getting the Surface returns the values once they've been overwritten
    by the variables covering them.

    Functions:
        __init__([dictionary, coverLayers])
        BuildRange([layer])
        Edit(name, value)
        Cover(name, value, [layer])
        ImportCover(cover, [layer])
        Uncover(name, [layer])
        UncoverAll([layer])
        IsCovered(name)
        GetBase([var])
        GetCover([layer, var])
        GetSurface([var])"""
    def __init__(self, dictionary = {}, coverLayers = 1):
        """Builds the base dictionary and a number of cover dictionaries."""
        self.base = dictionary
        self.coverSheet = []
        for height in range(coverLayers):
            self.coverSheet.append({})

    def BuildRange(self, layer = None):
        """Returns an array of layers to check. Private function.

        int -> Just that layer.
        None -> All layers."""
        if layer == None:
            fields = range(len(self.coverSheet))
        else:
            fields = [layer]
        return fields
        
    def Edit(self, name, value):
        """Add/edit a variable in the base."""
        self.base[name] = value

    def Cover(self, name, value, layer = 0):
        """Covers [name] var with [value] in cover layer [layer]."""
        if name not in self.base:
            self.Edit(name, None)
        self.coverSheet[layer][name] = value

    def ImportCover(self, cover, layer = 0):
        """Replaces the old cover in [layer] with the new [cover]."""
        for name in cover:
            if name not in self.base:
                self.Edit(name, None)
        self.coverSheet[layer] = cover

    def Uncover(self, name, layer = None):
        """Removes any covers in [layer] on [name]."""
        fields = self.BuildRange(layer)
        for field in fields:
            if name in self.coverSheet[field]:
                del self.coverSheet[field][name]

    def UncoverAll(self, layer = None):
        """Removes all covers in [layer]."""
        fields = self.BuildRange(layer)
        for field in fields:
            self.coverSheet[field] = {}

    def IsCovered(self, name):
        """Returns whether [name] is covered or not."""
        covered = False
        for cover in self.coverSheet:
            if name in cover:
                covered = True
        return covered

    def GetBase(self, var = None):
        """Returns the base value of [var] or the base itself."""
        ##One var from base
        if var != None:
            if var in self.base:
                return self.base[var]
            else:
                return None
        ##Whole base
        else:
            return self.base

    def GetCover(self, layer = None, var = None):
        """Returns [var]'s value in [layer] or [layer] itself.

        Values in higher-numbered layers override lower-numbered layers."""
        cover = {}
        fields = self.BuildRange(layer)
        fields.reverse()
        ##One var from cover
        if var != None:
            result = None
            for field in fields:
                if var in self.coverSheet[field]:
                    return self.coverSheet[field][var]
            return None
        ##Whole cover
        for field in fields:
            for name in self.coverSheet[field]:
                if name not in cover:
                    cover[name] = self.coverSheet[field][name]
        return cover

    def GetSurface(self, var = None):
        """Returns the surface value of [var] or the surface itself.

        The surface is the base, covered by any covers."""
        ##One var from surface
        if var != None:
            result = self.GetCover(var = var)
            if result == None:
                result = self.GetBase(var = var)
            return result
        ##Whole surface
        surface = self.GetCover()
        for name in self.base:
            if name not in surface: 
                surface[name] = self.base[name]
        return surface
