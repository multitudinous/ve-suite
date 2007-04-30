"""VE-Launcher's Cluster window."""
import wx
import os
from velBase import *
from velClusterDict import *
from velCoveredConfig import *
from velExtraVarsWindow import *
from string import strip, replace

class ClusterWindow(wx.Dialog):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, state)
        UpdateData()
        UpdateDisplay(cursor)
        AddNew(event)
        Delete(event)
        OnClose(event)
    """
    def __init__(self, parent, state, title = "Cluster Settings"):
        """Sets up the Jconf window."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build displays.
        self.clustList = wx.ListBox(self, -1,
                                    size=JCONF_LIST_DISPLAY_MIN_SIZE)
        ##Build Add & Delete buttons.
        self.bAdd = wx.Button(self, -1, "Add")
        self.bAdd.SetToolTip(wx.ToolTip("Add a slave listing."))
        self.bEdit = wx.Button(self, -1, "Rename")
        self.bEdit.SetToolTip(wx.ToolTip("Rename a slave listing."))
        self.bDelete = wx.Button(self, -1, "Delete")
        self.bDelete.SetToolTip(wx.ToolTip("Delete a slave listing."))
        self.bExtraVars = wx.Button(self, -1, "Pass Extra Vars")
        self.bExtraVars.SetToolTip(wx.ToolTip("Set extra environment" +
                                              " variables for VE-Suite to" +
                                              " pass."))
        ##Build master display.
        self.masterCtrl = wx.TextCtrl(self, -1)
        self.masterCtrl.SetToolTip(wx.ToolTip("Name the master computer."))
        ##Build user display. Displayed in Windows only.
        self.userCtrl = wx.TextCtrl(self, -1)
        self.userCtrl.SetToolTip(wx.ToolTip("Username for logging into" +
                                            " the cluster."))
        self.userExampleText = wx.StaticText(self, -1,
                                             style=wx.ST_NO_AUTORESIZE)
        self.userExampleText.SetToolTip(wx.ToolTip("Code example" +
                                                   " for username."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, self.bAdd)
        self.Bind(wx.EVT_BUTTON, self.Rename, self.bEdit)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.ExtraVars, self.bExtraVars)
        self.Bind(wx.EVT_TEXT, self.UpdateExampleCode, self.userCtrl)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.AddMany([self.bAdd, VERTICAL_SPACE,
                          self.bEdit, VERTICAL_SPACE,
                          self.bDelete, VERTICAL_SPACE,
                          self.bExtraVars])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##List field + buttons.
        columnSizer.Add(self.clustList, 1, wx.EXPAND)
        columnSizer.AddMany([HORIZONTAL_SPACE, rowSizer])
        ##Path display
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Slave names:"))
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Master's name:")])
        rowSizer.Add(self.masterCtrl, 0, wx.EXPAND)
        ##User name display (Windows only)
        if windows:
            rowSizer.Add(VERTICAL_SPACE)
            rowSizer.Add(wx.StaticText(self, -1, "User name:"))
            rowSizer.Add(self.userCtrl, 0, wx.EXPAND)
            rowSizer.Add(VERTICAL_SPACE)
            rowSizer.Add(wx.StaticText(self, -1, "Example:"))
            rowSizer.Add(self.userExampleText, 0, wx.EXPAND)
        else:
            self.userExampleText.Hide()
            self.userCtrl.Hide()
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

    def UpdateData(self):
        """Updates data to match the display."""
        if self.masterCtrl.IsEnabled():
            self.state.Edit("ClusterMaster", self.masterCtrl.GetValue())
        if self.userCtrl.IsEnabled():
            self.state.Edit("User", self.GetUser())
        return

    def GetUser(self):
        """Returns the username input with slashes corrected."""
        return replace(strip(self.userCtrl.GetValue()), '/', '\\')

    def UpdateExampleCode(self, event = None):
        user = self.GetUser()
        if user == "":
            phrase = ""
        else:
            phrase = "-u %s" %(user)
        self.userExampleText.SetLabel("psexec slave %s -i..." %phrase)

    def UpdateDisplay(self, cursor = None):
        """Updates display to match the data."""
        ##Set cursor if it's blank.
        ##Master Node
        self.masterCtrl.SetValue(self.state.GetSurface("ClusterMaster"))
        self.masterCtrl.Enable(self.state.IsEnabled("ClusterMaster"))
        ##User Name
        self.userCtrl.SetValue(self.state.GetSurface("User"))
        self.userCtrl.Enable(self.state.IsEnabled("User"))
        self.UpdateExampleCode()
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
                    break
            else:
                dlg.Destroy()
                break


    def Rename(self, event):
        """Renames the selected slave entry.
        
        Ensures the new name:
        -Contains no slashes.
        -Isn't empty spaces."""
        name = self.clustList.GetStringSelection()
        while True:
            n = name
            dlg = wx.TextEntryDialog(self,
                                     "What do you want to rename" + \
                                     " %s to?\n\n" %(n),
                                     "Rename %s" %(n), name)
            if dlg.ShowModal() == wx.ID_OK:
                name = dlg.GetValue()
                selection = self.clustList.GetStringSelection()
                dlg.Destroy()
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
                    name = self.clustList.GetStringSelection()
                ##Check if it's already in the list.
                elif name in self.state.GetBase("ClusterDict").GetNames():
                    dlg = wx.MessageDialog(self,
                                           "[%s] is already in the" %(name)+
                                           " cluster list.\n" +
                                           "Do you want to delete" +
                                           " [%s] instead?" %(selection),
                                           "ERROR: Computer Already in List",
                                           wx.YES_NO | wx.NO_DEFAULT)
                    if dlg.ShowModal() == wx.ID_YES:
                        self.state.GetBase("ClusterDict").Delete(selection)
                    dlg.Destroy()
                    self.UpdateDisplay()
                    break                    
                ##Else accept it.
                else:
                    self.state.GetBase("ClusterDict").Rename(selection, name)
                    self.UpdateDisplay(name)
                    break
            else:
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
            ##Update other data & display.
            self.UpdateData()
            self.UpdateDisplay()

    def ExtraVars(self, event):
        """Opens up the Extra Vars editing window."""
        extraVarsWindow = ExtraVarsWindow(self, self.state)
        extraVarsWindow.ShowModal()

    def OnClose(self, event):
        """Closes ClusterWindow."""
        self.UpdateData()
        self.Hide()
        self.Destroy()
