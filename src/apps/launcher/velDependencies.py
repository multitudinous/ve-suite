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
"""Handles checking and choosing the Dependencies folder for VE-Launcher.

Used only for Windows now. See velDepsWindow for Linux side.

Functions:
    Check(dependenciesDir)
    Change(parent)
    Get(parent)"""
from velBase import *
import os
import wx
import sys

def GetSpecific(dependenciesDir):
    """Returns a hosttype-specific dependencies dir if generic one's passed."""
    ##If depDir == None, return.
    if not dependenciesDir:
        return False
    if unix:
        nameServiceFile = "Naming_Service"
    elif windows:
        nameServiceFile = "Naming_Service.exe"
    else:
        nameServiceFile = "None"
    if not os.path.exists(os.path.join(dependenciesDir,
                                       "bin", nameServiceFile)):
        alternate = os.path.join(dependenciesDir, CFD_HOST_TYPE)
        if os.path.exists(alternate):
            if os.path.exists(os.path.join(alternate,
                                           "bin", nameServiceFile)):
                return alternate
    return False

def Check(dependenciesDir):
    """Returns True if Dependencies folder checks out, false if it doesn't.

    Automatically called during velauncher's __init__.
    Checks if dependenciesDir exists,
    then checks if it looks like the Dependencies directory.
    If any check fails, it returns False. Else it returns True."""
    ##Set name of the file to check in the Dependencies folder
    if unix:
        nameServiceFile = "Naming_Service"
    elif windows:
        nameServiceFile = "Naming_Service.exe"
    else:
        nameServiceFile = "None"
    ##Check if DependenciesDir's path exists.
    if not os.path.exists(dependenciesDir):
        dlg = wx.MessageDialog(None,
                               "I can't find the " +
                               "VE_Suite_Dependencies directory.\n" +
                               "It may have been moved, renamed," +
                               " or deleted.\n" +
                               "Please find it for me.",
                               "Error: Dependencies Directory" +
                               " Not Found",
                               wx.OK)
        dlg.ShowModal()
        dlg.Destroy()
        return False
    ##Check if DependenciesDir's contents look legitimate.
    elif not os.path.exists(os.path.join(dependenciesDir,
                            "bin", nameServiceFile)) and \
         not GetSpecific(dependenciesDir):
        dlg = wx.MessageDialog(None,
                               "%s\n" % str(dependenciesDir) +
                               "doesn't look like the Dependencies " +
                               "directory I need.\n" +
                               "Are you sure you want to use it?",
                               "Warning: Dependencies Directory" +
                               " Looks Unfamiliar",
                               wx.YES_NO | wx.NO_DEFAULT)
        response = dlg.ShowModal()
        dlg.Destroy()
        ##Did the user choose to still use it or not?
        if response == wx.ID_NO:
            return False
        else:
            return dependenciesDir
    ##If all checks passed, return dependenciesDir (True).
    return dependenciesDir

def Change(parent):
    """Asks user for a new DependenciesDir."""
    legitimateDependenciesDir = False
    while not legitimateDependenciesDir:
        dependenciesDir = Get(parent)
        if dependenciesDir == None:
            return None
        legitimateDependenciesDir = Check(dependenciesDir)
    return dependenciesDir

def Get(parent):
    """Ask user for DependenciesDir. Called by Change.

    Returns the directory path the user chose.
    Helper function for Check."""
    ##Go up a directory if it's a Unix os to get out
    ##of the VE_Suite directory.
    config = wx.Config.Get()
    existingDepDir = config.Read("DependenciesDir", ":::")
    del config
    if existingDepDir != ":::":
        searchDir = existingDepDir
    elif windows:
        searchDir = VELAUNCHER_DIR
    elif unix:
        searchDir = os.path.split(VELAUNCHER_DIR)[0]
    else:
        searchDir = "None"
    ##User chooses the directory.
    dlg = wx.DirDialog(None,
                       "Choose the VE Dependencies directory:",
                       searchDir,
                       style=wx.DD_DEFAULT_STYLE | ~wx.DD_NEW_DIR_BUTTON)
    if dlg.ShowModal() == wx.ID_OK:
        ##If a directory's chosen, exit the loop and return it.
        searchDir = dlg.GetPath()
        dirChosen = True
        dlg.Destroy()
    elif existingDepDir == ":::":
        ##If not, and no existing DependenciesDir exists,
        ##show an error message and ask the user to choose
        ##another directory or quit the launcher.
        dlg.Destroy()
        dlg = wx.MessageDialog(None,
                               "You didn't choose a Dependencies" +
                               " directory.\n" +
                               "VE Suite Launcher won't run" +
                               " without one.\n" +
                               "Press OK to find the directory" +
                               " or Cancel to quit VE Suite Launcher.",
                               "Error: Directory Not Chosen",
                               wx.OK | wx.CANCEL)
        ##Quit if the user refuses to choose a Dependencies directory.
        if dlg.ShowModal() == wx.ID_CANCEL:
            sys.exit(0)
        else:
            return Change()
    else:
        ##Else if none chosen & one already exists,
        ##return None.
        searchDir = None
        dlg.Destroy()
    return searchDir
