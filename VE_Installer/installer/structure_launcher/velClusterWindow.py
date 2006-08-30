"""VE-Launcher's Cluster window."""
import wx
import os
from velBase import *
from velClusterDict import *
from velCoveredConfig import *

class ClusterWindow(wx.Dialog):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, ID, title, L, cursor=0)
        DisplayJconfFile(event)
        DeleteEnabledCheck
        Update(cursor)
        AddNew(event)
        Delete(event)
        Rename(event)
        OnClose(event)
    """
    def __init__(self, parent, state,
                 ID = -1, title = "Cluster Settings"):
        """Sets up the Jconf window.

        Keyword arguments:
        D: The linked Cluster dictionary this window modifies.
        clusterMaster: Name of the cluster's Master.
        """
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
##        self.cDict = state.GetSurface("ClusterDict")
##        self.clusterMaster = state.GetSurface("ClusterMaster")
        ##Build displays.
        self.clustList = wx.ListBox(self, -1,
                                         size=JCONF_LIST_DISPLAY_MIN_SIZE)
        ##Build Add & Delete buttons.
        self.bAdd = wx.Button(self, -1, "Add")
        self.bAdd.SetToolTip(wx.ToolTip("Add a slave listing."))
        self.bDelete = wx.Button(self, -1, "Delete")
        self.bDelete.SetToolTip(wx.ToolTip("Delete a slave listing."))
        ##Build master display.
        self.masterCtrl = wx.TextCtrl(self, -1)
##        self.masterCtrl.SetValue(self.clusterMaster)
        self.masterCtrl.SetToolTip(wx.ToolTip("Name the master computer."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Fill in info.
        self.UpdateDisplay()
##        ##Check if Delete's enabled.
##        self.DeleteEnabledCheck()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, self.bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.AddMany([self.bAdd, VERTICAL_SPACE,
                          self.bDelete])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##List field + buttons.
        columnSizer.Add(self.clustList, 1, wx.EXPAND)
        columnSizer.AddMany([HORIZONTAL_SPACE, rowSizer])
        ##List field + Path display
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Slave names:"))
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Master's name:")])
        rowSizer.Add(self.masterCtrl, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        Style(self)

##    def DeleteEnabledCheck(self):
##        """Disables/Enables the Delete button based on number of entries.
##
##        Disabled if entries <= 1
##        Enabled if entries > 1"""
##        if self.cDict.Length() <= 0:
##            self.bDelete.Enable(False)
##        else:
##            self.bDelete.Enable(True)

    def UpdateData(self):
        if self.masterCtrl.IsEnabled():
            self.state.Edit("ClusterMaster", self.masterCtrl.GetValue())
        return

    def React(self):
        return
    
    def UpdateDisplay(self, cursor = None):
        """Updates the shown entries list & checks to match recent changes."""
        ##Set cursor if it's blank.
        ##Master Node
        self.masterCtrl.SetValue(self.state.GetSurface("ClusterMaster"))
        self.masterCtrl.Enable(self.state.IsEnabled("ClusterMaster"))
        ##Slave Nodes
        if cursor == None:
            cursor = self.clustList.GetStringSelection()
        newSlaveList = self.state.GetSurface("ClusterDict").GetNames()
        self.clustList.Set(newSlaveList)
        if cursor in newSlaveList:
            self.clustList.SetStringSelection(cursor)
        self.clustList.Enable(self.state.IsEnabled("ClusterDict"))
        self.bAdd.Enable(self.state.IsEnabled("ClusterDict"))
        self.bDelete.Enable(self.state.IsEnabled("ClusterDict") and
                            len(self.state.GetSurface("ClusterDict")) > 0)
##        if cursor == "":
##            cursor = self.clustList.GetStringSelection()
##        nameList = self.cDict.GetNames()
##        self.clustList.Set(nameList)
##        if cursor == wx.NOT_FOUND or self.clustList.GetCount() == 0:
##            self.clustList.SetSelection(wx.NOT_FOUND)
##        elif self.clustList.FindString(str(cursor)) == wx.NOT_FOUND:
##            self.clustList.SetSelection(wx.NOT_FOUND)
##        else:
##            self.clustList.SetStringSelection(cursor)

    def AddNew(self, event):
        """User chooses a new cluster computer to add to the list.

        Default name: Address of cluster computer."""
        while True:
            dlg = wx.TextEntryDialog(self,
                                     "Please enter the name of the computer:",
                                     "Add Cluster Computer")
            if dlg.ShowModal() == wx.ID_OK:
                location = dlg.GetValue()
                dlg.Destroy()
                locationList = self.state.GetBase("ClusterDict").GetLocations()
                ##Reject if it's empty.
                if location.isspace() or location == '':
                    dlg = wx.MessageDialog(self,
                                           "Your name is empty." + \
                                           " Please try again.",
                                           "ERROR: Name is Empty",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                ##Reject if it has slashes.
                elif '/' in location or '\\' in location:
                    dlg = wx.MessageDialog(self,
                                           "Your name has slashes in it.\n" + \
                                           "Please try again.",
                                           "ERROR: Name Contains Slashes",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()                
                ##Return if this location's already listed.
                elif location in locationList:
                    dlg = wx.MessageDialog(self,
                                           "[%s] is already in the" %(location)+
                                           " cluster list.\n" +
                                           "You don't need to add it again.",
                                           "ERROR: Computer Already in List",
                                           wx.OK)
                    dlg.ShowModal()
                    dlg.Destroy()
                    return
                else:
                    self.state.GetBase("ClusterDict").Add(location, location)
                    self.UpdateData()
                    self.UpdateDisplay()
##                    self.DeleteEnabledCheck()
                    break
            else:
                dlg.Destroy()
                break

    def Delete(self, event):
        """Deletes the selected entry from the list.

        Also moves the selection index if it would be off the list."""
        ##Error catch if nothing's selected.
        if self.clustList.GetStringSelection() == "":
            dlg = wx.MessageDialog(self, "Can't delete; nothing is selected.",
                                   "Deletion Error: Nothing Selected",
                                   wx.OK)
            dlg.ShowModal()
            return
        ##Confirm delete.
        dlg = wx.MessageDialog(self,
                               "Are you sure you want to delete" +
                               " %s?" %(self.clustList.GetStringSelection()),
                               "Confirm Deletion",
                               wx.YES_NO | wx.NO_DEFAULT)
        dlg.CenterOnParent(wx.BOTH)
        if dlg.ShowModal() == wx.ID_YES:
            name = self.clustList.GetStringSelection()
            self.state.GetBase("ClusterDict").Delete(name)
            ##Update display.
            self.UpdateDisplay()
##            self.DeleteEnabledCheck()

    def OnClose(self, event):
        """Closes ClusterWindow."""
##        self.GetParent().clusterMaster = self.masterCtrl.GetValue()
##        self.cDict.WriteConfig()
        self.UpdateData()
        self.Hide()
        self.Destroy()
