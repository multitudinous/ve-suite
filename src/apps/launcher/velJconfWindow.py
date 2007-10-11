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
"""VE-Launcher's Jconf window."""
import wx
import os
from velBase import *
from velJconfDict import *
from velCoveredConfig import *

class JconfWindow(wx.Dialog):
    """A window for editing a list of Jconf files.

    Functions:
        __init__(parent, state, [iD, title])
        UpdateData([event])
        React
        UpdateDisplay([selection, refreshList])
        AddNew(event)
        Delete(event)
        Rename(event)
        NameChangeWarning(oldName, newName)
        OnClose(event)
    """
    def __init__(self, parent, state, iD = wx.ID_ANY, title = "Xplorer Configurations"):
        """Sets up the Jconf window."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE ^ (wx.RESIZE_BORDER | 
                                                             wx.MINIMIZE_BOX |
                                                             wx.MAXIMIZE_BOX)
                           | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build displays.
        self.lblListBox = wx.StaticText(self, -1, "Xplorer Configurations:", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.lblPath = wx.StaticText(self, -1)
        self.confList = wx.ListBox(self, -1, wx.DefaultPosition, [150,150])
        self.display = wx.TextCtrl(self, -1, "", wx.DefaultPosition, [80,-1], style=wx.TE_READONLY)
        ##self.display.SetBackgroundColour(BACKGROUND_COLOR)

        ##Build buttons.
        self.bAdd = wx.Button(self, -1, "Add", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bAdd.SetToolTip(wx.ToolTip("Add a configuration listing."))
        self.bRename = wx.Button(self, -1, "Rename", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bRename.SetToolTip(wx.ToolTip("Rename a configuration listing."))
        self.bDelete = wx.Button(self, -1, "Delete", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bDelete.SetToolTip(wx.ToolTip("Delete a configuration listing."))
        #OK and Cancel button
        if windows:
            self.bOk = wx.Button( self, wx.ID_OK, "OK", wx.DefaultPosition, wx.DefaultSize, 0 )
        else:
            self.bOk = wx.Button( self, wx.ID_SAVE, "Save", wx.DefaultPosition, wx.DefaultSize, 0 )
        self.bCancel = wx.Button( self, wx.ID_CANCEL, "Cancel", wx.DefaultPosition, wx.DefaultSize, 0 )        
        self.UpdateDisplay(self.state.GetSurface("JconfSelection"), True)
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, self.bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.Rename, self.bRename)
        if windows:
            self.Bind(wx.EVT_BUTTON, self.OnOk, id = wx.ID_OK)
        else:
            self.Bind(wx.EVT_BUTTON, self.OnOk, id = wx.ID_SAVE)
        self.Bind(wx.EVT_LISTBOX, self.UpdateData, self.confList)
        ##self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        vSizerMain = wx.BoxSizer( wx.VERTICAL )
        vSizer1 = wx.BoxSizer( wx.VERTICAL )
        vSizer1.Add( self.lblListBox, 0, wx.ALIGN_CENTER_VERTICAL|wx.TOP, 5 )
        hSizer1 = wx.BoxSizer( wx.HORIZONTAL )
        hSizer1.Add( self.confList, 0, wx.GROW|wx.ALIGN_CENTER_HORIZONTAL|wx.TOP, 5 )
        vSizer2 = wx.BoxSizer( wx.VERTICAL )
        vSizer2.Add( self.bAdd, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        vSizer2.Add( self.bRename, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        vSizer2.Add( self.bDelete, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        hSizer1.Add( vSizer2, 0, wx.GROW|wx.ALIGN_CENTER_HORIZONTAL, 5 )
        vSizer1.Add( hSizer1, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL, 5 )
        vSizer1.Add( self.lblPath, 0, wx.ALIGN_CENTER_VERTICAL|wx.TOP, 5 )
        vSizer1.Add( self.display, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.TOP, 5 )
        hSizer2 = wx.BoxSizer( wx.HORIZONTAL )

        if windows:
            hSizer2.Add( self.bOk, 0, wx.GROW|wx.ALIGN_CENTER_HORIZONTAL|wx.ALL, 5 )
            hSizer2.Add( self.bCancel, 0, wx.GROW|wx.ALIGN_CENTER_HORIZONTAL|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        else:
            hSizer2.Add( self.bCancel, 0, wx.GROW|wx.ALIGN_CENTER_HORIZONTAL|wx.ALL, 5 )
            hSizer2.Add( self.bOk, 0, wx.GROW|wx.ALIGN_CENTER_HORIZONTAL|wx.LEFT|wx.TOP|wx.BOTTOM, 5 )
        vSizer1.Add( hSizer2, 0, wx.ALIGN_RIGHT|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.TOP, 5 )
        vSizerMain.Add( vSizer1, 0, wx.ALIGN_CENTER|wx.ALL, 5 )           ##Set size, position.
 
        vSizerMain.SetSizeHints(self)
        self.SetSizer(vSizerMain)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        #Style(self)

    def UpdateData(self, event = None):
        """Updates the data to match recent user changes.

        Only covers selecting a new Jconf;
        the functions for adding/deleting one directly changes the data."""
        #currentSelection = self.confList.GetStringSelection()
        #self.state.Edit("JconfSelection", currentSelection)
        self.React()
        self.UpdateDisplay()
        return
    
    def React(self):
        """Covers/uncovers data based on user input. Unused for now."""
        return

    def UpdateDisplay(self, selection = None, refreshList = False):
        """Updates the shown entries list to match recent changes."""
        if refreshList:
            self.confList.Clear()
            self.confList.Set(self.state.GetSurface("JconfDict").GetNames())
            
            if selection in self.state.GetSurface("JconfDict").GetNames():
                self.confList.SetStringSelection(selection)
            else:
                self.confList.SetSelection(0)
                
            self.bDelete.Enable(len(self.state.GetSurface("JconfDict")) > 1 and
                                self.state.IsEnabled("JconfDict"))
            self.bAdd.Enable(self.state.IsEnabled("JconfDict"))
            self.confList.Enable(self.state.IsEnabled("JconfSelection"))
            
        s = self.confList.GetStringSelection()
        f = self.state.GetSurface("JconfDict").GetPath(s)
        self.display.SetValue(f)
        self.display.SetInsertionPointEnd()
        self.lblPath.SetLabel("%s's path:" %(s))

    def AddNew(self, event):
        """User chooses a new Jconf file to add to the list.

        Default name: Name of Jconf file."""
        ##Default directory for the search is the
        ##DepsDir/JUGGLER_FOLDER/configFiles.
##        f = self.state.GetSurface("DependenciesDir")
##        if f != None:
##            f = os.path.join(f, JUGGLER_FOLDER, "configFiles")
##        else:
##            f = VELAUNCHER_DIR
        if self.state.GetSurface("EnableDefWorkingDir"):
            f = self.state.GetSurface("DefaultWorkingDir")
        else:    
            f = self.state.GetSurface("Directory")
        dlg = wx.FileDialog(self,
                            "Choose a configuration file.",
                            defaultDir = f,
                            wildcard = "Jconfig (*.jconf)|*.jconf",
                            style=wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            name = os.path.split(path)[1][:-6]
            finalName = self.state.GetBase("JconfDict").Add(name, path)
            if name != finalName:
                self.NameChangeWarning(name, finalName)
            self.UpdateDisplay(finalName, True)
        dlg.Destroy()

    def Delete(self, event):
        """Deletes the selected entry from the list.

        Also moves the selection index if it would be off the list."""
        victim = self.confList.GetStringSelection()
        cursor = self.confList.GetSelection()
        dlg = wx.MessageDialog(self,
                               "Are you sure you want to delete\n" +
                               "%s?" % victim,
                               "Confirm Deletion",
                               wx.YES_NO | wx.NO_DEFAULT)
        dlg.CenterOnParent(wx.BOTH)
        if dlg.ShowModal() == wx.ID_YES:
            self.state.GetBase("JconfDict").Delete(victim)
            ##Move the cursor to the next string above/below.
            if cursor == self.confList.GetCount() - 1:
                cursor -= 1
            else:
                cursor += 1
            self.UpdateDisplay(self.confList.GetString(cursor), True)
        dlg.Destroy()

    def Rename(self, event):
        """Renames the selected Jconf entry.
        
        Ensures the new name:
        -Contains no slashes.
        -Isn't empty spaces."""
        name = self.confList.GetStringSelection()
        while True:
            n = self.confList.GetStringSelection()
            p = self.state.GetSurface("JconfDict").GetPath(n)
            f = os.path.split(p)[1]
            dlg = wx.TextEntryDialog(self,
                                     "What do you want to rename" + \
                                     " %s to?\n\n" %(n) + \
                                     "Jconf File: %s" %(f),
                                     "Rename %s" %(n), name)
            if dlg.ShowModal() == wx.ID_OK:
                name = dlg.GetValue()
                dlg.Destroy()
                selection = self.confList.GetStringSelection()
                ##Check for slashes
                if name.count('/') > 0 or name.count('\\') > 0:
                    dlg = wx.MessageDialog(self,
                                           "Your new name has slashes" + \
                                           " in it.\n" + \
                                           "Please choose a different name.",
                                           "ERROR: Name Contains Slashes",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    name = name.replace('/', '-')
                    name = name.replace('\\', '-')
                ##Check if it's empty/spaces
                elif name.isspace() or name == '':
                    dlg = wx.MessageDialog(self,
                                           "Your new name is empty." + \
                                           " Please choose a different name.",
                                           "ERROR: Name is Empty",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    name = self.confList.GetStringSelection()
                ##Else accept it.
                else:
                    finalName = self.state.GetBase("JconfDict").Rename(selection,
                                                                   name)
                    if finalName != name:
                        self.NameChangeWarning(name, finalName)
                    self.UpdateDisplay(finalName, True)
                    break
            else:
                break

    def NameChangeWarning(self, oldName, newName):
        """Warns user if oldName was changed to newName."""
        dlg = wx.MessageDialog(None,
                               "The name %s already existed" %(oldName) + \
                               " in the list.\n" + \
                               "Your entry was given the" + \
                               " name %s instead." %(newName),
                               "NOTE: Name Changed",
                               wx.OK)
        dlg.ShowModal()
        dlg.Destroy()


    def OnOk(self, event):
        """Save Changes and Closes JconfWindow."""
        selection = self.confList.GetStringSelection()
        if selection != "":
            self.state.Edit("JconfSelection", selection)
        self.Hide()
        self.Destroy()


    def OnClose(self, event):
        self.Hide()
        self.Destroy()
