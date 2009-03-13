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
# *************** <auto-copyright.pl END do not edit this line> ***************
import wx
import wx.grid
import os
from velBase import *
from velModes import *
from velClusterDict import *
from velCoveredConfig import *
from velSaveLoadConfig import *
from velSaveConfigWindow import *
from velExtraVarsWindow import *
from string import strip, replace

class PrefWindow(wx.Dialog):

    def __init__(self, parent, state, title = "Preferences"):
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE ^ (wx.RESIZE_BORDER | 
                                                             wx.MINIMIZE_BOX |
                                                             wx.MAXIMIZE_BOX)
                           | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Prepare Panel
        ##Build displays.
        self.isAutoShutdown = self.state.GetSurface("AutoShutDown")
        self.isEnableVSync = self.state.GetSurface("EnableVSync")
        self.isEnableRTT = self.state.GetSurface("RTT")

        self.sbLableDirectory = wx.StaticBox( self, -1, "Default Working Directory" )
        #Text box and Browse button
        self.txDirectory = wx.TextCtrl( self, 800, 
                                        self.state.GetSurface("DefaultWorkingDir"), 
                                        wx.DefaultPosition, [200,22], 0 )
        self.bDirectory = wx.Button(self, -1, "Browse..." , wx.DefaultPosition, wx.DefaultSize, 0 )
        
        #Options Section
        self.sbLableOptions = wx.StaticBox( self, -1, "Options" )
        ##Option check boxes
        self.cbDefaultDir = wx.CheckBox( self, 801, "Use Default Working Directory", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbAutoShutdown = wx.CheckBox( self, 802, "Auto Shutdown", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbEnableVSync = wx.CheckBox( self, 803, "Enable VSync", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.cbEnableRTT = wx.CheckBox( self, 804, "Enable RTT", wx.DefaultPosition, wx.DefaultSize, 0 )

        #OK and Cancel button
        if windows:
            self.bOk = wx.Button( self, wx.ID_OK, "OK", wx.DefaultPosition, wx.DefaultSize, 0 )
        else:
            self.bOk = wx.Button( self, wx.ID_SAVE, "Save", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bCancel = wx.Button( self, wx.ID_CANCEL, "Cancel", wx.DefaultPosition, wx.DefaultSize, 0 )


        self.sText = wx.StaticText( self, -1, "", wx.DefaultPosition, [5,5],  0)
        ##self.sLine = wx.StaticLine( self, -1, wx.DefaultPosition, [305,-1], wx.LI_HORIZONTAL )

        boxSizerA = wx.BoxSizer( wx.VERTICAL )
        sbSizerA = wx.StaticBoxSizer( self.sbLableDirectory, wx.VERTICAL )
        sbSubSizer = wx.BoxSizer( wx.HORIZONTAL )

        boxSizerA.Add( self.sText, 0, wx.ALIGN_LEFT|wx.ALL, 1 )
        ##boxSizerA.Add( self.sLine, 0, wx.ALIGN_CENTER|wx.ALL, 0 )

        sbSizerA.Add( self.cbDefaultDir, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        sbSubSizer.Add( self.txDirectory, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        sbSubSizer.Add( self.bDirectory, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        sbSizerA.Add( sbSubSizer, 0, wx.ALIGN_CENTER|wx.ALL, 2 )

        boxSizerA.Add( sbSizerA, 0, wx.ALIGN_CENTER|wx.ALL, 5 )

        sbSizerB = wx.StaticBoxSizer( self.sbLableOptions, wx.VERTICAL )

        sbSizerB.Add( self.cbAutoShutdown, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        ##Add "Enable VSync" on Only PURE POSIX MODE
        sbSizerB.Add( self.cbEnableVSync, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        sbSizerB.Add( self.cbEnableRTT, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        boxSizerA.Add( sbSizerB, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )
        boxSizerB = wx.BoxSizer( wx.HORIZONTAL )

        if windows:
            boxSizerB.Add( self.bOk, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
            boxSizerB.Add( self.bCancel, 0, wx.ALIGN_CENTER|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        else:
            boxSizerB.Add( self.bCancel, 0, wx.ALIGN_CENTER|wx.ALL, 5 )
            boxSizerB.Add( self.bOk, 0, wx.ALIGN_CENTER|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        boxSizerA.Add( boxSizerB, 0, wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5 )

        boxSizerA.SetSizeHints(self)
        self.SetSizer( boxSizerA )
        self.CenterOnParent(wx.BOTH)

        ##Bind buttons.
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_CHECKBOX, self.Refresh, id = 801)
        self.Bind(wx.EVT_BUTTON, self.FileButtonBranch, self.bDirectory)
        if windows:
            self.Bind(wx.EVT_BUTTON, self.UpdateData, id = wx.ID_OK)
        else:    
            self.Bind(wx.EVT_BUTTON, self.UpdateData, id = wx.ID_SAVE)
        if (MODE_LIST[self.state.GetSurface("Mode")]) == "Computation":
            self.cbAutoShutdown.Disable()
        
        if not posix:
            self.cbEnableVSync.Disable()

        if not self.state.GetSurface("EnableDefWorkingDir"):
            self.txDirectory.Disable()
            self.bDirectory.Disable()
        
        self.React()

    def UpdateData(self, event = None, depDir = None):

        react = False
        ##DependenciesDir
        if depDir != None:
            self.state.Edit("DependenciesDir", depDir)
        ##Directory
        if self.txDirectory.IsEnabled():
            self.state.Edit("DefaultWorkingDir", self.txDirectory.GetValue())
        ##AutoShutDown
        if self.cbAutoShutdown.IsEnabled():
            self.state.Edit("AutoShutDown", self.cbAutoShutdown.GetValue())
        ##RTT
        if self.cbEnableRTT.IsEnabled():
            self.state.Edit("RTT", self.cbEnableRTT.GetValue())
        ##Use Default Working Directory
        if self.cbDefaultDir.IsEnabled():
            self.state.Edit("EnableDefWorkingDir", self.cbDefaultDir.GetValue())
        ##Enable VSync
        if posix:
            if self.cbEnableVSync.IsEnabled():
                self.state.Edit("EnableVSync", self.cbEnableVSync.GetValue())

        if react:
            self.React()
            
        self.OnClose()
        return


    def React(self):
        """Covers/uncovers data based on user input."""
        ##UpdateDisplay
        self.UpdateDisplay()
        return


    def UpdateDisplay(self):
        ##RecentFiles menu
        ##self.recentMenu.Enable(not self.state.GetSurface("RecentFiles").IsEmpty())
        ##DependenciesDir

        ##AutoShutDown menu.
        confCheck = self.state.GetSurface("AutoShutDown")
        self.cbAutoShutdown.SetValue(confCheck)

        ##RTT menu.
        confCheck = self.state.GetSurface("RTT")
        self.cbEnableRTT.SetValue(confCheck)

        ##Use Default Working Directory
        useDefDir = self.state.GetSurface("EnableDefWorkingDir")
        self.cbDefaultDir.SetValue(useDefDir)

        ##Enable VSync
        if posix:
            vSyncCheck = self.state.GetSurface("EnableVSync")
            self.cbEnableVSync.SetValue(vSyncCheck)
            
        return

   
    def CloseFiles(self, event = None):
        if not self.state.GetSurface("VESFile") and \
           not self.state.GetSurface("ShellScript"):
            ##No files to close notification.
            dlg = wx.MessageDialog(self,
                                   "You don't have any files opened.",
                                   "No Files to Close", wx.OK)
            dlg.ShowModal()
            dlg.Destroy()
            return
        else:
            confirm = wx.MessageDialog(self,
                                       "Are you sure you want to\n" +
                                       "close any .ves or script\n" +
                                       "files you have opened?",
                                       "Confirm File Close",
                                       wx.YES_NO | wx.YES_DEFAULT)
            if confirm.ShowModal() == wx.ID_YES:
                self.state.InterpretArgument(None)
                self.UpdateDisplay()
            confirm.Destroy()
        return

    
    def FileButtonBranch(self, event = None):
        if self.state.GetSurface("VESFile") or \
           self.state.GetSurface("ShellScript"):
            self.CloseFiles()
        else:
            self.ChooseDirectory()


    def ChooseDirectory(self, event = None):
        curDir = self.txDirectory.GetValue()
        ##NOTE: If curDir doesn't exist, it automatically goes
        ##to the user's directory
        dlg = wx.DirDialog(self, "Choose VE Suite's working directory:",
                           self.txDirectory.GetValue(),
                           style=wx.DD_DEFAULT_STYLE | ~wx.DD_NEW_DIR_BUTTON)
        if dlg.ShowModal() == wx.ID_OK:
            self.txDirectory.SetValue(dlg.GetPath())
            self.txDirectory.SetInsertionPointEnd()
        dlg.Destroy()
        

    def Refresh(self, event = None):

        if self.cbDefaultDir.IsEnabled():
            self.state.Edit("EnableDefWorkingDir", self.cbDefaultDir.GetValue())
                    
        if not self.state.GetSurface("EnableDefWorkingDir"):
            self.txDirectory.Disable()
            self.bDirectory.Disable()
        else:
            self.txDirectory.Enable()
            self.bDirectory.Enable()
            
        self.React()
        
        
    def OnClose(self, event = None):
        self.Hide()
        self.Destroy()
