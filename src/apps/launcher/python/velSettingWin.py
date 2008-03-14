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
"""VE-Launcher's Settings window."""
import os ##Used for setting environmental variables, running programs
import wx ##Used for GUI

from velBase import *
from velJconfWindow import *
from velClusterWindow import *
from velJconfDict import *
from velClusterDict import *
from velCoveredConfig import *

class SettingsWindow(wx.Dialog):
    """Subwindow for viewing/changing mode settings.

    Functions:
        __init__(parent, state, [position])
        UpdateData([event])
        React
        UpdateDisplay
        EditJconf([event])
        EditCluster([event])
        OnClose([event])"""
    def __init__(self, parent, state, position = wx.DefaultPosition):
        """Creates the Settings window."""
        ##Set up data.
        self.state = state
        modeName = MODE_LIST[self.state.GetSurface("Mode")]
        wx.Dialog.__init__(self, parent, -1, "%s Mode Settings" %(modeName),
                           pos = position,
                           style = wx.DEFAULT_FRAME_STYLE ^ (wx.RESIZE_BORDER | 
                                                             wx.MINIMIZE_BOX |
                                                             wx.MAXIMIZE_BOX)
                           | wx.TAB_TRAVERSAL)
        ##Jconf pull-down menu.
        
        self.lblStBox1 = wx.StaticBox(self, -1, "Programs to launch" )
        ##Name Server checkbox.
        self.cbNameServer = wx.CheckBox(self, -1, "Name Server", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbNameServer.SetToolTip(wx.ToolTip("Run Name Server at Launch"))
        ##Conductor checkbox.
        self.cbConductor = wx.CheckBox(self, -1, "Conductor", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbConductor.SetToolTip(wx.ToolTip("Run Conductor at Launch"))
        ##Xplorer checkbox.
        self.cbXplorer = wx.CheckBox(self, -1, "Xplorer", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbXplorer.SetToolTip(wx.ToolTip("Run Xplorer at Launch"))
        ##Desktop checkbox.
        self.cbDesktop = wx.CheckBox(self, -1, "Desktop Mode", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbDesktop.SetToolTip(wx.ToolTip("Set Desktop Mode for" +
                                             " Conductor and Xplorer"))
        
        self.lblStBox2 = wx.StaticBox(self, -1, "Xplorer Configuration" )
        ##Xplorer Type radio box.
        self.rbXplorer = wx.RadioBox(self, -1, "Mode",
                                     wx.DefaultPosition, wx.DefaultSize,
                                     RADIO_XPLORER_LIST, 1, wx.RA_SPECIFY_ROWS)
        self.rbXplorer.SetToolTip(wx.ToolTip("Which Xplorer format do you" +
                                             " want to launch?"))
        ##Cluster button.
        self.bCluster = wx.Button(self, -1, "Cluster Settings", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bCluster.SetToolTip(wx.ToolTip("Set the computers and extra" +
                                            " variables in the cluster."))
        ##Configuration Choice
        self.chJconf = wx.Choice(self, -1, wx.DefaultPosition, [150,-1])
        self.chJconf.SetToolTip(wx.ToolTip("Choose Xplorer's configuration."))
        ##Edit Jconf button.
        self.bEditJconf = wx.Button(self, -1, "Edit Configuration List", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bEditJconf.SetToolTip(wx.ToolTip("Edit the list of Xplorer" +
                                              " configurations."))        
        #OK and Cancel button
        if windows:
            self.bOk = wx.Button( self, wx.ID_OK, "OK", wx.DefaultPosition, wx.DefaultSize, 0 )
        else:
            self.bOk = wx.Button( self, wx.ID_SAVE, "Save", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bCancel = wx.Button( self, wx.ID_CANCEL, "Cancel", wx.DefaultPosition, wx.DefaultSize, 0 )
        
        ##Bind events.
        self.Bind(wx.EVT_LISTBOX, self.Refresh, self.chJconf)
        self.Bind(wx.EVT_CHECKBOX, self.Refresh, self.cbXplorer)
        self.Bind(wx.EVT_RADIOBOX, self.Refresh, self.rbXplorer)
        self.Bind(wx.EVT_CHECKBOX, self.Refresh, self.cbConductor)
        self.Bind(wx.EVT_CHECKBOX, self.Refresh, self.cbDesktop)
        """
        self.Bind(wx.EVT_LISTBOX, self.UpdateData, self.chJconf)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateData, self.cbXplorer)
        self.Bind(wx.EVT_RADIOBOX, self.UpdateData, self.rbXplorer)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateData, self.cbConductor)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateData, self.cbDesktop)
        """
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        if windows:
            self.Bind(wx.EVT_BUTTON, self.OnOk, id = wx.ID_OK)
        else:
            self.Bind(wx.EVT_BUTTON, self.OnOk, id = wx.ID_SAVE)
        self.Bind(wx.EVT_BUTTON, self.EditJconf, self.bEditJconf)
        self.Bind(wx.EVT_BUTTON, self.EditCluster, self.bCluster)
        
        ##Set sizers.
        vSizerMain = wx.BoxSizer( wx.VERTICAL )
        vSizer1 = wx.BoxSizer( wx.VERTICAL )
        svSizer1 = wx.StaticBoxSizer( self.lblStBox1, wx.VERTICAL )
        svSizer1.Add( self.cbNameServer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        hSizer1 = wx.BoxSizer( wx.HORIZONTAL )
        hSizer1.Add( self.cbConductor, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
        spacer1 = wx.StaticText(self, -1, "      ", wx.DefaultPosition, wx.DefaultSize, 0 )
        hSizer1.Add( spacer1, 0, wx.ALIGN_CENTER, 5 )
        hSizer1.Add( self.cbDesktop, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
        svSizer1.Add( hSizer1, 0, wx.ALIGN_CENTER_VERTICAL, 5 )
        svSizer1.Add( self.cbXplorer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        vSizer1.Add( svSizer1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.TOP, 5 )
        spacer2 = wx.StaticText(self, -1, "", wx.DefaultPosition, [10,10], 0 )
        vSizer1.Add( spacer2, 0, wx.ALIGN_CENTER, 5 )
        svSizer2 = wx.StaticBoxSizer( self.lblStBox2, wx.VERTICAL )
        hSizer2 = wx.BoxSizer( wx.HORIZONTAL )
        hSizer2.Add( self.rbXplorer, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
        hSizer2.Add( self.bCluster, 0, wx.ALIGN_CENTER|wx.LEFT|wx.RIGHT|wx.TOP, 5 )
        svSizer2.Add( hSizer2, 0, wx.ALIGN_CENTER_VERTICAL, 5 )
        hSizer3 = wx.BoxSizer( wx.HORIZONTAL )
        hSizer3.Add( self.chJconf, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
        hSizer3.Add( self.bEditJconf, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
        svSizer2.Add( hSizer3, 0, wx.ALIGN_CENTER, 5 )
        vSizer1.Add( svSizer2, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL, 5 )
        hSizer4 = wx.BoxSizer( wx.HORIZONTAL )
        if windows:
            hSizer4.Add( self.bOk, 0, wx.ALIGN_CENTER|wx.ALL, 5 )    
            hSizer4.Add( self.bCancel, 0, wx.ALIGN_CENTER|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        else:    
            hSizer4.Add( self.bCancel, 0, wx.ALIGN_CENTER|wx.ALL, 5 )    
            hSizer4.Add( self.bOk, 0, wx.ALIGN_CENTER|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        vSizer1.Add( hSizer4, 0, wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.TOP, 5 )
        vSizerMain.Add( vSizer1, 0, wx.ALIGN_CENTER|wx.ALL, 5 )            
                        
        vSizerMain.SetSizeHints(self)
        self.SetSizer(vSizerMain)
        #self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        #Style(self)
        if not CLUSTER_ENABLED:
            self.bCluster.Hide()
        ##Set up OK button.
        ##Update Display
        self.React()

    def UpdateData(self, event = None):
        """Saves the user's input to the launcher's data variables."""
        ##NOTE: Will have to change way user's variables are saved if 
        ##modes allow users to change these in the future.
        ##Probably by grabbing the oldMode and checking its settings.
        array = {"JconfSelection": [self.chJconf,
                                    self.chJconf.GetStringSelection()],
                 "NameServer": [self.cbNameServer,
                                self.cbNameServer.GetValue()],
                 "Xplorer": [self.cbXplorer, self.cbXplorer.GetValue()],
                 "Conductor": [self.cbConductor, self.cbConductor.GetValue()],
                 "DesktopMode": [self.cbDesktop, self.cbDesktop.GetValue()],
                 "XplorerType": [self.rbXplorer,
                                XPLORER_TYPE_LIST[self.rbXplorer.GetSelection()]]}

        for var in array:
            ##if array[var][0].IsEnabled():
            self.state.Edit(var, array[var][1])
        self.React()
        return

    def React(self):
        """Covers/uncovers data based on user input."""
        ##Disable DesktopMode if Xplorer & Conductor == False
        #self.state.React(self.state.GetSurface("Xplorer") == False and
        #                 self.state.GetSurface("Conductor") == False,
        #                 "DesktopMode", False)
        if self.state.GetSurface("DesktopMode"):
            self.rbXplorer.SetSelection(0)
            
        
        self.UpdateDisplay()
        return
    
    def UpdateDisplay(self):
        """Changes settings to match the selected mode."""
        ##Jconf
        self.chJconf.Clear()
        for name in self.state.GetSurface("JconfDict").GetNames():
            self.chJconf.Append(name)
        self.chJconf.SetStringSelection(self.state.GetSurface("JconfSelection"))
        self.chJconf.Enable(self.state.IsEnabled("JconfDict") == True and
                            self.state.IsEnabled("JconfSelection") == True and
                            self.state.GetSurface("Xplorer") == True)
        self.bEditJconf.Enable(self.state.IsEnabled("JconfDict") and
                               self.state.GetSurface("Xplorer") == True)
        ##Name Server
        self.cbNameServer.SetValue(self.state.GetSurface("NameServer"))
        self.cbNameServer.Enable(self.state.IsEnabled("NameServer"))
        ##Conductor
        self.cbConductor.SetValue(self.state.GetSurface("Conductor"))
        self.cbConductor.Enable(self.state.IsEnabled("Conductor"))
        ##Xplorer
        self.cbXplorer.SetValue(self.state.GetSurface("Xplorer"))
        self.cbXplorer.Enable(self.state.IsEnabled("Xplorer"))
        ##Desktop Mode
        self.cbDesktop.SetValue(self.state.GetSurface("DesktopMode"))
        self.cbDesktop.Enable(self.state.IsEnabled("DesktopMode"))
        ##Xplorer Type
        if self.state.GetSurface("DesktopMode"):
            self.rbXplorer.SetSelection(0)
        else:
            if (self.state.GetSurface("XplorerType") == "OSG-VEP"):
                self.rbXplorer.SetSelection(0)
            else:
                self.rbXplorer.SetSelection(1)
        self.rbXplorer.Enable(self.state.IsEnabled("XplorerType") == True and
                              self.state.GetSurface("DesktopMode") == False and
                              self.state.GetSurface("Xplorer") == True)
        ##Cluster Node button
        self.bCluster.Enable(CLUSTER_ENABLED and
                             self.state.GetSurface("Xplorer") == True and
                             self.state.GetSurface("DesktopMode") == False and
                             self.state.GetSurface("XplorerType") == "OSG-VEPC")
        return

    def Refresh(self, event = None):
        self.state.React(self.state.GetSurface("Xplorer") == False and
                         self.state.GetSurface("Conductor") == False,
                         "DesktopMode", False)        
        """Changes settings to match the selected mode."""
        ##Jconf
        self.chJconf.Clear()
        for name in self.state.GetSurface("JconfDict").GetNames():
            self.chJconf.Append(name)
        self.chJconf.SetStringSelection(self.state.GetSurface("JconfSelection"))
        
        self.chJconf.Enable(self.state.IsEnabled("JconfDict") and
                            self.state.IsEnabled("JconfSelection") and
                            self.cbXplorer.GetValue() == True)
        
        self.bEditJconf.Enable(self.state.IsEnabled("JconfDict") and
                               self.cbXplorer.GetValue() == True)
        ##Name Server
        self.cbNameServer.SetValue(self.cbNameServer.GetValue())
        self.cbNameServer.Enable(self.state.IsEnabled("NameServer"))
        ##Conductor
        self.cbConductor.SetValue(self.cbConductor.GetValue())
        self.cbConductor.Enable(self.state.IsEnabled("Conductor"))
        ##Xplorer
        self.cbXplorer.SetValue(self.cbXplorer.GetValue())
        self.cbXplorer.Enable(self.state.IsEnabled("Xplorer"))
        ##Desktop Mode
        self.cbDesktop.SetValue(self.cbDesktop.GetValue())
        self.cbDesktop.Enable(self.state.IsEnabled("DesktopMode"))
        ##Xplorer Type
        
        if self.cbDesktop.GetValue():
                    self.rbXplorer.SetSelection(0)
        else:
            self.rbXplorer.SetSelection(self.rbXplorer.GetSelection())
            
        self.rbXplorer.Enable(self.state.IsEnabled("XplorerType") and
                              self.cbDesktop.GetValue() == False and
                              self.cbXplorer.GetValue() == True)
        ##Cluster Node button
        self.bCluster.Enable(CLUSTER_ENABLED and
                             self.cbXplorer.GetValue() == True and
                             self.cbDesktop.GetValue() == False and
                             self.rbXplorer.GetSelection() == 1)
        return


    def EditJconf(self, event = None):
        """Brings up the Jconf editing window."""
        self.UpdateData()
        jconfWindow = JconfWindow(self, self.state)
        jconfWindow.ShowModal()
        self.React()

    def EditCluster(self, event = None):
        """Brings up the Cluster editing window."""
        self.UpdateData()
        clusterWindow = ClusterWindow(self, self.state)
        clusterWindow.ShowModal()
        self.React()
        
    def OnOk(self, event = None):
        """Sends current configuration back to parent & closes window."""
        ##Close.
        self.UpdateData()
        self.Hide()
        self.Destroy()        

    def OnClose(self, event = None):
        """Sends current configuration back to parent & closes window."""
        ##Close.
        self.Hide()
        self.Destroy()
