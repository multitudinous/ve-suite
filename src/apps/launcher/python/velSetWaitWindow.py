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
"""VE-Launcher's dialog for setting wait times."""
import wx
from string import digits
from velBase import *
from velCoveredConfig import *

class SetWaitWindow(wx.Dialog):
    """Subwindow for changing wait settings for VE-Suite."""
    def __init__(self, parent, state):
        wx.Dialog.__init__(self, parent, wx.ID_ANY, "Cluster Wait Settings",
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build master wait display.
        self.masterWaitCtrl = wx.TextCtrl(self, -1)
        self.masterWaitCtrl.SetToolTip(wx.ToolTip("Seconds to wait" +
                                                  " after the master."))
        ##Build slave wait display.
        self.slaveWaitCtrl = wx.TextCtrl(self, -1)
        self.slaveWaitCtrl.SetToolTip(wx.ToolTip("Seconds to wait" +
                                                 " between slaves."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Launcher."))
        ##Bind settings.
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Set values.
        self.UpdateDisplay()
        ##Construct layout.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "Master Wait:"),
                        0, wx.ALIGN_CENTER)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.masterWaitCtrl, 1)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "Slave Wait:"),
                        0, wx.ALIGN_CENTER)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.slaveWaitCtrl, 1)
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.CenterOnParent(wx.BOTH)
        ##Show self
        self.Show()

    def UpdateData(self):
        ##MasterWait
        if self.masterWaitCtrl.IsEnabled():
            self.state.Edit("MasterWait", int(self.masterWaitCtrl.GetValue()))
        ##SlaveWait
        if self.slaveWaitCtrl.IsEnabled():
            self.state.Edit("SlaveWait", int(self.slaveWaitCtrl.GetValue()))
    
    def UpdateDisplay(self):
        ##MasterWait
        self.masterWaitCtrl.SetValue(str(self.state.GetSurface("MasterWait")))
        self.masterWaitCtrl.Enable(self.state.IsEnabled("MasterWait"))
        ##SlaveWait
        self.slaveWaitCtrl.SetValue(str(self.state.GetSurface("SlaveWait")))
        self.slaveWaitCtrl.Enable(self.state.IsEnabled("SlaveWait"))

    def WaitDataOkay(self):
        """Returns false and changes waits' values if they're not ints >= 0"""
        testPool = {"MasterWait": self.masterWaitCtrl,
                    "SlaveWait": self.slaveWaitCtrl}
        legit = True
        for field in testPool:
            phrase = testPool[field].GetValue()
            ##Make sure it's an int >= 0
            try:
                if int(phrase) < 0:
                    legit = False
                    phrase = None
            except ValueError:
                legit = False
                phrase = None
            ##Change the field back to its previous value.
            if phrase == None:
                testPool[field].SetValue(str(self.state.GetSurface(field)))
        return legit

    def OnClose(self, event):
        """Closes ClusterWindow."""
        ##Update data
        ##Abort OnClose if illegal values entered.
        if not self.WaitDataOkay():
            ##Display error message.
            dlg=wx.MessageDialog(self,
                                 "The wait fields contained illegal values.\n" +
                                 "The illegal values were reset.\n" +
                                 "Please enter non-negative integers" +
                                 " for waits.",
                                 "Error: Illegal Wait Values",
                                 wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        else:
            self.UpdateData()
            self.Hide()
            self.Destroy()
