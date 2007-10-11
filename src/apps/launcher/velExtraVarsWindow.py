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
"""VE-Launcher's Cluster window."""
import wx
import os
from velBase import *
from velCoveredConfig import *
from string import *

class ExtraVarsWindow(wx.Dialog):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, state)
        UpdateData()
        UpdateDisplay(cursor)
        AddNew(event)
        Delete(event)
        OnClose(event)
    """
    def __init__(self, parent, state, title = "Extra Env. Variables to Pass"):
        """Sets up the window."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build list box.
        self.variableBlock = wx.TextCtrl(self, -1, style = wx.TE_MULTILINE)
        self.variableBlock.SetToolTip(wx.ToolTip("Enter a list of environment variables."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(wx.StaticText(self, -1, "List the variables you want passed. Ex:"))
        mainSizer.Add(wx.StaticText(self, -1, "OSG_HOME, PF_ENABLED, ROTO_PATH"))
        mainSizer.Add(self.variableBlock, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND, BORDER)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        Style(self)


    def UpdateData(self):
        """Updates data to match the display."""
        self.state.Edit("ExtraVariables", self.variableBlock.GetValue())
        return


    def UpdateDisplay(self, cursor = None):
        """Updates display to match the data.."""
        ##Set cursor if it's blank.
        ##Master Node
        self.variableBlock.SetValue(self.state.GetSurface("ExtraVariables"))
        self.variableBlock.Enable(self.state.IsEnabled("ExtraVariables"))


    def OnClose(self, event):
        """Closes ExtraVarsWindow."""
        self.UpdateData()
        self.Hide()
        self.Destroy()
