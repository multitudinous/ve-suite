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
"""VE-Launcher's Linux Dependencies window.

Non-Windows only. See velDependencies for Windows dependency code."""
import wx
import os
from velBase import *
from velCoveredConfig import *
from velDepsArray import *
from string import strip, replace

class DepsWindow(wx.Dialog):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, state)
        UpdateData()
        UpdateDisplay(cursor)
        AddNew(event)
        Delete(event)
        OnClose(event)
    """
    def __init__(self, parent, state, title = "Set Dependencies"):
        """Sets up the dependencies window."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build displays.
        self.depList = wx.ListBox(self, -1, size=DEPS_LIST_DISPLAY_MIN_SIZE)
        ##Build Add & Delete buttons.
        self.bAdd = wx.Button(self, -1, "Add")
        self.bAdd.SetToolTip(wx.ToolTip("Add a dependency."))
        self.bDelete = wx.Button(self, -1, "Delete")
        self.bDelete.SetToolTip(wx.ToolTip("Delete a dependency."))
        ##Build VR-Juggler display & button.
        self.bChoose = wx.Button(self, -1, "Choose")
        self.bChoose.SetToolTip(wx.ToolTip("Choose the VR-Juggler directory."))
        self.displayVR = wx.TextCtrl(self, -1, "None", style = wx.TE_READONLY)
        ##Build master display.
        ##self.masterCtrl = wx.TextCtrl(self, -1)
        ##self.masterCtrl.SetToolTip(wx.ToolTip("Name the master computer."))
        ##Build user display. Displayed in Windows only.
        ##self.userCtrl = wx.TextCtrl(self, -1)
        ##self.userCtrl.SetToolTip(wx.ToolTip("Username for logging into" +
        ##                                    " the cluster."))
        ##self.userExampleText = wx.StaticText(self, -1,
        ##                                     style=wx.ST_NO_AUTORESIZE)
        ##self.userExampleText.SetToolTip(wx.ToolTip("Code example" +
        ##                                           " for username."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Save dependencies and quit."))
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, self.bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        ##self.Bind(wx.EVT_TEXT, self.UpdateExampleCode, self.userCtrl)
        self.Bind(wx.EVT_BUTTON, self.ChooseVJ, self.bChoose)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.AddMany([self.bAdd, VERTICAL_SPACE,
                          self.bDelete])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##List field + buttons.
        columnSizer.Add(self.depList, 1, wx.EXPAND)
        columnSizer.AddMany([HORIZONTAL_SPACE, rowSizer])
        ##Path display
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Dependencies:"))
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "VR-Juggler's Directory:"),
                          VERTICAL_SPACE])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(self.displayVR, 1)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.bChoose)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background colors.
        Style(self)

    ##def UpdateData(self):
    ##    """Updates data to match the display."""
    ##    if self.masterCtrl.IsEnabled():
    ##        self.state.Edit("ClusterMaster", self.masterCtrl.GetValue())
    ##    if self.userCtrl.IsEnabled():
    ##        self.state.Edit("User", self.GetUser())
    ##    return

    ##def UpdateExampleCode(self, event = None):
    ##    user = self.GetUser()
    ##    if user == "":
    ##        phrase = ""
    ##    else:
    ##        phrase = "-u %s" %(user)
    ##    self.userExampleText.SetLabel("psexec slave %s -i..." %phrase)

    def UpdateData(self):
        """Update data to match the display.

        Nothing to do currently."""
        return

    def UpdateDisplay(self, cursor = None):
        """Updates display to match the data."""
        ##Set cursor if it's blank.
        if cursor == None:
            cursor = self.depList.GetStringSelection()
        newDepsList = self.state.GetSurface("Dependencies").GetNames()
        self.depList.Set(newDepsList)
        if cursor in newDepsList:
            self.depList.SetStringSelection(cursor)
        self.depList.Enable(self.state.IsEnabled("Dependencies"))
        self.bAdd.Enable(self.state.IsEnabled("Dependencies"))
        self.bDelete.Enable(self.state.IsEnabled("Dependencies") and
                            len(self.state.GetSurface("Dependencies")) > 0)
        ##Juggler folder.
        if self.state.IsEnabled("JugglerDep"):
            self.displayVR.Enable(True)
            self.displayVR.SetBackgroundColour(READONLY_COLOR)
        else:
            self.displayVR.Enable(False)
        displayVRLabel = self.state.GetSurface("JugglerDep")
        if displayVRLabel == None:
            displayVRLabel = "None"
        self.displayVR.SetValue(displayVRLabel)
        self.bChoose.Enable(self.state.IsEnabled("JugglerDep"))

    def AddNew(self, event):
        """User chooses a new dependency to add to the list."""
        locationList = self.state.GetBase("Dependencies").GetNames()
        if len(locationList) > 0:
            defaultDirectory = locationList[ len(locationList) - 1 ]
        else:
            defaultDirectory = VELAUNCHER_DIR
        ##User chooses the directory.
        dlg = wx.DirDialog(None,
                           "Choose the dependency's directory:",
                           defaultDirectory,
                           style=wx.DD_DEFAULT_STYLE | ~wx.DD_NEW_DIR_BUTTON)
        if dlg.ShowModal() == wx.ID_OK:
            location = dlg.GetPath()
            dlg.Destroy()
            ##Return if this location's already listed.
            if location in locationList:
                dlg = wx.MessageDialog(self,
                                       "[%s] is already in the" %(location)+
                                       " dependencies.\n" +
                                       "You don't need to add it again.",
                                       "ERROR: Directory Already in List",
                                       wx.OK)
                dlg.ShowModal()
                dlg.Destroy()
                return
            else:
                self.state.GetBase("Dependencies").Add(location)
                self.UpdateData()
                self.UpdateDisplay()
        else:
            dlg.Destroy()
        return

    def ChooseVJ(self, event):
        """User choose the VR-Juggler dependency."""
	if self.state.GetSurface("JugglerDep") == None:
	    jugglerDirStart = VELAUNCHER_DIR
	else:
	    jugglerDirStart = self.state.GetSurface("JugglerDep")
        dlg = wx.DirDialog(None,
                           "Choose VR-Juggler's directory:",
                           jugglerDirStart,
                           style=wx.DD_DEFAULT_STYLE | ~wx.DD_NEW_DIR_BUTTON)
        if dlg.ShowModal() == wx.ID_OK:
            location = dlg.GetPath()
            dlg.Destroy()
            ##NOTE: Enhance by putting VR-Juggler check here, like the
            ##dependencies one.
            self.state.Edit("JugglerDep", location)
            self.UpdateData()
            self.UpdateDisplay()
        else:
            dlg.Destroy()
        return

    def Delete(self, event):
        """Deletes the selected entry from the list.

        Also moves the selection index if it would be off the list."""
        ##Error catch if nothing's selected.
        if self.depList.GetStringSelection() == "":
            dlg = wx.MessageDialog(self, "Can't delete; nothing is selected.",
                                   "Deletion Error: Nothing Selected",
                                   wx.OK)
            dlg.ShowModal()
            return
        ##Confirm delete.
        dlg = wx.MessageDialog(self,
                               "Are you sure you want to delete" +
                               " %s?" %(self.depList.GetStringSelection()),
                               "Confirm Deletion",
                               wx.YES_NO | wx.NO_DEFAULT)
        dlg.CenterOnParent(wx.BOTH)
        if dlg.ShowModal() == wx.ID_YES:
            name = self.depList.GetStringSelection()
            self.state.GetBase("Dependencies").Delete(name)
            ##Update other data & display.
            self.UpdateData()
            self.UpdateDisplay()

    def OnClose(self, event):
        """Closes ClusterWindow."""
        self.UpdateData()
        self.Hide()
        self.Destroy()
