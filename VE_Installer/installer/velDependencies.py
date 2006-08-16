"""Handles checking and choosing the Dependencies folder for VE-Launcher."""
from velBase import *
import os
import wx
import sys

def Check(dependenciesDir):
    """Returns true if Dependencies folder checks out, false if it doesn't.

    Automatically called during __init__.
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
                            "bin", nameServiceFile)):
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
            return True
    ##If all checks passed, return True.
    return True

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
        searchDir = os.getcwd()
    elif unix:
        searchDir = os.path.split(os.getcwd())[0]
    else:
        searchDir = "None"
    ##User chooses the directory.
    dlg = wx.DirDialog(None,
                       "Choose the VE Dependencies directory:",
                       searchDir,
                       style=wx.DD_DEFAULT_STYLE)
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
            ##TESTER necessity of parent.Hide/Destroy
            ##parent.Hide()
            ##parent.Destroy()
            sys.exit(0)
        else:
            return Change()
    else:
        ##Else if none chosen & one already exists,
        ##return None.
        searchDir = None
        dlg.Destroy()
    return searchDir
