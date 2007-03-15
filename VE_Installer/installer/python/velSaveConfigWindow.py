"""VE-Launcher's Save Configuration window."""
import os ##Used for setting environmental variables, running programs
import wx ##Used for GUI

from velBase import *
from velCoveredConfig import *
from velSaveLoadConfig import *

class SaveConfigWindow(wx.Dialog):
    """Lets user choose name to save config under.

    Functions:
        __init__(parent, state)
        ChangeName([event])
        SaveConfig([event])
        OnClose([event])"""
    def __init__(self, parent, state):
        """Builds the Save Configuration window."""
        wx.Dialog.__init__(self, parent, -1, "Choose a Save Name",
                           style = wx.DEFAULT_FRAME_STYLE &
                           ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                           wx.MAXIMIZE_BOX))
        self.state = state
        config = wx.Config.Get()
        ##Get the list of saved configurations.
        choices = []
        config.SetPath("..")
        configEntry = config.GetFirstGroup()
        while (configEntry[0]):
            if configEntry[1] != DEFAULT_CONFIG:
                choices.append(configEntry[1])
            configEntry = config.GetNextGroup(configEntry[2])
        config.SetPath(DEFAULT_CONFIG)
        ##Build list.
        self.configList = wx.ListBox(self, -1,
                                     size=JCONF_LIST_DISPLAY_MIN_SIZE,
                                     choices = choices)
        ##Build text display.
        self.txtSaveName = wx.TextCtrl(self, -1)
        self.txtSaveName.SetToolTip(wx.ToolTip("Name to save it under."))
        ##Build OK & Cancel buttons.
        bCancel = wx.Button(self, -1, "Cancel")
        bCancel.SetToolTip(wx.ToolTip("Cancel the save."))
        bOk = wx.Button(self, -1, "Save")
        bOk.SetToolTip(wx.ToolTip("Save and return."))
        bOk.SetDefault()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.OnClose, bCancel)
        self.Bind(wx.EVT_BUTTON, self.SaveConfig, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_LISTBOX, self.ChangeName, self.configList)
        self.Bind(wx.EVT_LISTBOX_DCLICK, self.SaveConfig, self.configList)
        ##Construct layout.
        ##Add/Rename/Delete buttons.
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(wx.StaticText(self, -1, "Saved configurations:"))
        rowSizer.Add(self.configList, 1, wx.EXPAND)
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Save's name:")])
        rowSizer.Add(self.txtSaveName, 0, wx.EXPAND)
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(bCancel, 1, wx.EXPAND)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(bOk, 1, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(columnSizer, 0, wx.EXPAND)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_DIALOG_SIZE)
        self.CenterOnParent(wx.BOTH)
        return

    def ChangeName(self, event = None):
        """Changes the configuration name displayed in the text box."""
        newName = self.configList.GetStringSelection()
        if newName != "":
            self.txtSaveName.SetValue(newName)
        return

    def SaveConfig(self, event = None):
        """Saves the configuration under the name in the text box.

        Ensures the new name:
        -Contains no slashes.
        -Isn't empty spaces.
        -Doesn't overwrite an existing name w/o confirmation."""
        name = self.txtSaveName.GetValue()
        ##Don't overwrite the default config.
        if name == DEFAULT_CONFIG:
            err = wx.MessageDialog(self,
                                   "You can't name it '%s'.\n" %DEFAULT_CONFIG +
                                   "The default config uses that.",
                                   "Error: Reserved Name Chosen",
                                   wx.OK)
            err.ShowModal()
            err.Destroy()
            return
        ##Check for slashes
        if name.count('/') > 0 or name.count('\\') > 0:
            err = wx.MessageDialog(self,
                                   "You can't use a name with\n" +
                                   "slashes in it.\n",
                                   "Error: Name Contains Slashes",
                                   wx.OK)
            err.ShowModal()
            err.Destroy()
            return
        ##Check if it's empty/spaces
        elif name.isspace() or name == '':
            err = wx.MessageDialog(self,
                                   "You can't use an empty name.",
                                   "Error: Name is Empty",
                                   wx.OK)
            err.ShowModal()
            err.Destroy()
            return
        ##Confirm if it'll overwrite another file.
        config = wx.Config.Get()
        config.SetPath("..")
        overwrite = config.Exists(name)
        config.SetPath(DEFAULT_CONFIG)
        if overwrite:
            confirm = wx.MessageDialog(self,
                                       "%s already exists.\n" % name +
                                       "Do you want to overwrite it?",
                                       "Confirm Overwrite",
                                       wx.YES_NO | wx.YES_DEFAULT)
            choice = confirm.ShowModal()
            confirm.Destroy()
            if choice == wx.ID_NO:
                return
        ##Save the config.
        SaveConfig(name, self.state)
        ##Close the dialog.
        self.OnClose()
        return

    def OnClose(self, event = None):
        """Closes the window."""
        self.Hide()
        self.Destroy()

