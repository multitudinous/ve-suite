import wx
import os
from velBase import *

class JconfDict(VelDict):
    """Stores a list of Jconf pairs in this setup:
    {name: path, name: path..}
    under the variable self.dictionary.

    Functions:
        __init__()
        Add(name, path)
        Rename(oldName, newName)
        Delete(name)
        UniqueName(name)
        GetPath(name)
        Length() / __len__
        GetNames()
        WriteConfig()
    """
    def __init__(self):
        """Creates a list of .jconf names/paths from the Launcher's Config."""
        VelDict.__init__(self)

    def ReadEntries(self):
        entries = {}
        config = wx.Config.Get()
        config.SetPath(JCONF_CONFIG)
        step = config.GetFirstEntry()
        while (step[0]):
            entries[step[1]] = config.Read(step[1])
            step = config.GetNextEntry(step[2])
        config.SetPath('..')
        return entries

    def Rename(self, oldName, newName):
        """Renames the entry at oldName to newName.

        Returns newName, modified until it's unique on the list."""
        ##Don't do anything if newName == pos's old name
        if newName == oldName:
            return newName
        ##Change name if it matches another one on the list.
        path = self.dictionary[oldName]
        self.Delete(oldName)
        finalName = self.Add(newName, path)
        ##Return true if name had to be changed.
        return finalName

    def UniqueName(self, name):
        """Replaces name with a unique name in the list."""
        ##Add a numeric suffix to name if it matches a name
        ##already in the list.
        suffix = ""
        maxCount = 0
        curNames = self.GetNames()
        while curNames.count(name + str(suffix)) > maxCount:
            if suffix == "":
                suffix = '1'
            else:
                suffix = str(int(suffix) + 1)
        return name + str(suffix)

    def GetPath(self, name):
        """Returns the path of name's entry."""
        if name in self.dictionary:
            return self.dictionary[name]
        else:
            return "None"

    def WriteConfig(self):
        """Writes the entire list to config."""
        config = wx.Config.Get()
        config.SetPath(JCONF_CONFIG)
        for name in self.dictionary:
            config.Write(name, self.dictionary[name])
        config.SetPath('..')



class JconfWindow(wx.Dialog):
    """A window for editing a list of Jconf files.

    Functions:
        __init__(parent, ID, title, L, cursor=0)
        DisplayJconfFile(event)
        DeleteEnabledCheck()
        Update(selection)
        AddNew(event)
        Delete(event)
        Rename(event)
        NameChangeWarning(oldName, newName)
        OnClose(event)
    """
    def __init__(self, parent, ID, title, L, cursor):
        """Sets up the Jconf window.

        Keyword arguments:
        L: The linked Jconf dict this window modifies.
        cursor: Name of the current selection in L."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.dictionary = L
        ##Build displays.
        self.lblPath = wx.StaticText(self, -1)
        self.confList = wx.ListBox(self, -1, size=JCONF_LIST_DISPLAY_MIN_SIZE,
                                   choices=self.dictionary.GetNames())
        self.confList.SetStringSelection(cursor)
        self.display = wx.TextCtrl(self, -1, style=wx.TE_READONLY)
        self.DisplayJconfFile("")
        ##Build buttons.
        bAdd = wx.Button(self, -1, "Add")
        bAdd.SetToolTip(wx.ToolTip("Add a configuration listing."))
        bRename = wx.Button(self, -1, "Rename")
        bRename.SetToolTip(wx.ToolTip("Rename a configuration listing."))
        self.bDelete = wx.Button(self, -1, "Delete")
        self.bDelete.SetToolTip(wx.ToolTip("Delete a configuration listing."))
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Check if Delete's enabled.
        self.DeleteEnabledCheck()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.AddNew, bAdd)
        self.Bind(wx.EVT_BUTTON, self.Delete, self.bDelete)
        self.Bind(wx.EVT_BUTTON, self.Rename, bRename)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_LISTBOX, self.DisplayJconfFile, self.confList)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.AddMany([bAdd, VERTICAL_SPACE,
                          bRename, VERTICAL_SPACE,
                          self.bDelete])
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        ##List field + buttons.
        columnSizer.Add(self.confList, 1, wx.EXPAND)
        columnSizer.AddMany([HORIZONTAL_SPACE, rowSizer])
        ##List field + Path display
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Xplorer Configurations:"))
        rowSizer.Add(columnSizer, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          self.lblPath])
        rowSizer.Add(self.display, 0, wx.EXPAND)
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

    def DisplayJconfFile(self, event):
        """Shows the .jconf file of the selection in the text field."""
        s = self.confList.GetStringSelection()
        if s in self.dictionary.GetNames():
            f = self.dictionary.GetPath(s)
        else:
            f = "ERROR: Entry missing from list."
        self.display.SetValue(f)
        self.display.SetInsertionPointEnd()
        self.lblPath.SetLabel("%s's path:" %(s))
        
    def DeleteEnabledCheck(self):
        """Disables/Enables the Delete button based on number of entries.

        Disabled if entries <= 1
        Enabled if entries > 1"""
        if self.dictionary.Length() <= 1:
            self.bDelete.Enable(False)
        else:
            self.bDelete.Enable(True)

    def Update(self, selection):
        """Updates the shown entries list to match recent changes."""
        self.confList.Set(self.dictionary.GetNames())
        self.confList.SetStringSelection(selection)
        self.DisplayJconfFile("")

    def AddNew(self, event):
        """User chooses a new Jconf file to add to the list.

        Default name: Name of Jconf file."""
        ##Default directory for the search is the
        ##DepsDir/JUGGLER_FOLDER/configFiles.
        config = wx.Config.Get()
        f = config.Read("DependenciesDir", os.getcwd())
        del config
        f = os.path.join(f, JUGGLER_FOLDER, "configFiles")
        dlg = wx.FileDialog(self,
                            "Choose a configuration file.",
                            defaultDir = f,
                            wildcard = "Jconfig (*.jconf)|*.jconf",
                            style=wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            name = os.path.split(path)[1][:-6]
            finalName = self.dictionary.Add(name, path)
            if name != finalName:
                self.NameChangeWarning(name, finalName)
            self.Update(finalName)
            self.DeleteEnabledCheck()
        dlg.Destroy()

    def Delete(self, event):
        """Deletes the selected entry from the list.

        Also moves the selection index if it would be off the list."""
        dlg = wx.MessageDialog(self,
                               "Are you sure you want to delete\n" +
                               self.confList.GetStringSelection() + "?",
                               "Confirm Deletion",
                               wx.YES_NO | wx.NO_DEFAULT)
        dlg.CenterOnParent(wx.BOTH)
        if dlg.ShowModal() == wx.ID_YES:
            cursor = self.confList.GetSelection()
            self.dictionary.Delete(self.confList.GetStringSelection())
            ##Move the cursor if it wouldn't be on the list anymore.
            if cursor >= len(self.dictionary):
                selection = self.confList.GetString(len(self.dictionary) - 1)
            else:
                selection = self.confList.GetString(cursor + 1)
##            if cursor >= self.dictionary.Length():
##                cursor = self.dictionary.Length() - 1
            self.Update(selection)
            self.DeleteEnabledCheck()
        dlg.Destroy()

    def Rename(self, event):
        """Renames the selected Jconf entry.
        
        Ensures the new name:
        -Contains no slashes.
        -Isn't empty spaces."""
        loop = True
        name = self.confList.GetStringSelection()
        while loop:
            n = self.confList.GetStringSelection()
            p = self.dictionary.GetPath(n)
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
                    finalName = self.dictionary.Rename(selection, name)
                    if finalName != name:
                        self.NameChangeWarning(name, finalName)
                    self.Update(finalName)
                    loop = False
            else:
                loop = False

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

    def OnClose(self, event):
        """Closes JconfWindow."""
        self.GetParent().UpdateChJconf(self.confList.GetStringSelection())
        self.Hide()
        self.Destroy()


