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
    def __init__(self, parent,
                 state, position = wx.DefaultPosition):
        """Creates the Settings window."""
        ##Set up data.
        self.state = state
        modeName = MODE_LIST[self.state.GetSurface("Mode")]
        wx.Dialog.__init__(self, parent, -1, "%s Settings" %(modeName),
                           pos = position,
                           style = wx.DEFAULT_FRAME_STYLE &
                           ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                           wx.MAXIMIZE_BOX))
        ##Jconf pull-down menu.
        self.chJconf = wx.Choice(self, -1)
        self.chJconf.SetToolTip(wx.ToolTip("Choose Xplorer's configuration."))
        ##Edit Jconf button.
        self.bEditJconf = wx.Button(self, -1, "Edit Configuration List")
        self.bEditJconf.SetToolTip(wx.ToolTip("Edit the list of Xplorer" +
                                              " configurations."))
        ##Name Server checkbox.
        self.cbNameServer = wx.CheckBox(self, -1, "Name Server")
        self.cbNameServer.SetToolTip(wx.ToolTip("Run Name Server at Launch"))
        ##Conductor checkbox.
        self.cbConductor = wx.CheckBox(self, -1, "Conductor")
        self.cbConductor.SetToolTip(wx.ToolTip("Run Conductor at Launch"))
        ##Xplorer checkbox.
        self.cbXplorer = wx.CheckBox(self, -1, "Xplorer")
        self.cbXplorer.SetToolTip(wx.ToolTip("Run Xplorer at Launch"))
        ##Desktop checkbox.
        self.cbDesktop = wx.CheckBox(self, -1, "Desktop Mode")
        self.cbDesktop.SetToolTip(wx.ToolTip("Set Desktop Mode for" +
                                             " Conductor and Xplorer"))
        ##Xplorer Type radio box.
        self.rbXplorer = wx.RadioBox(self, -1, "Xplorer Type",
                                     wx.DefaultPosition, wx.DefaultSize,
                                     RADIO_XPLORER_LIST, 2, wx.RA_SPECIFY_COLS)
##                                     RADIO_XPLORER_LIST, 2, wx.RA_SPECIFY_ROWS)
        self.rbXplorer.SetToolTip(wx.ToolTip("Which Xplorer format do you" +
                                             " want to launch?"))
        ##Cluster button.
        self.bCluster = wx.Button(self, -1, "Set Cluster Computers")
        self.bCluster.SetToolTip(wx.ToolTip("Set the computers in" +
                                            " the cluster."))
        if not CLUSTER_ENABLED:
            self.bCluster.Hide()
        ##Set up OK button.
        bOk = wx.Button(self, -1, "OK")
        bOk.SetToolTip(wx.ToolTip("Return to the Launcher."))
        ##Update Display
        self.React()
        ##Bind events.
        self.Bind(wx.EVT_LISTBOX, self.UpdateData, self.chJconf)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateData, self.cbXplorer)
        self.Bind(wx.EVT_RADIOBOX, self.UpdateData, self.rbXplorer)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateData, self.cbConductor)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateData, self.cbDesktop)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_BUTTON, self.EditJconf, self.bEditJconf)
        self.Bind(wx.EVT_BUTTON, self.EditCluster, self.bCluster)
        ##Set sizers.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        ##Construct & insert the Jconf column.
        rowSizer.Add(wx.StaticText(self, -1, "Xplorer configuration:"))
        columnSizer.Add(self.chJconf, 1, wx.ALIGN_BOTTOM)
        columnSizer.AddMany([HORIZONTAL_SPACE,
                             self.bEditJconf])
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        ##Construct & insert the check box/radio box grid.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(self.cbDesktop)
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.bCluster)
        gridSizer = wx.FlexGridSizer(3, 2,
                                     VERTICAL_SPACE[1], HORIZONTAL_SPACE[0])
        gridSizer.Add(self.cbNameServer)
        gridSizer.Add((-1, self.bCluster.GetSize()[1]))
        gridSizer.AddMany([self.cbConductor, columnSizer,
                           self.cbXplorer, self.rbXplorer])
        ##Insert the Programs to Launch grid.
        rowSizer.AddMany([VERTICAL_SPACE,
                          wx.StaticText(self, -1, "Programs to launch:"),
                          VERTICAL_SPACE,
                          gridSizer])
        ##Set the main sizer, insert OK button.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 0, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 1, wx.EXPAND)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        ##Set the background color.
        Style(self)

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
            if array[var][0].IsEnabled():
                self.state.Edit(var, array[var][1])
        self.React()
        return

    def React(self):
        """Covers/uncovers data based on user input."""
        ##Disable DesktopMode if Xplorer & Conductor == False
        self.state.React(self.state.GetSurface("Xplorer") == False and
                         self.state.GetSurface("Conductor") == False,
                         "DesktopMode", False)
        self.UpdateDisplay()
        return
    
    def UpdateDisplay(self):
        """Changes settings to match the selected mode."""
        ##Jconf
        self.chJconf.Clear()
        for name in self.state.GetSurface("JconfDict").GetNames():
            self.chJconf.Append(name)
        self.chJconf.SetStringSelection(self.state.GetSurface("JconfSelection"))
        self.chJconf.Enable(self.state.IsEnabled("JconfDict") and
                            self.state.IsEnabled("JconfSelection") and
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
        self.rbXplorer.SetSelection(XPLORER_TYPE_LIST.index(self.state.GetSurface("XplorerType")))
        self.rbXplorer.Enable(self.state.IsEnabled("XplorerType") and
                              self.state.GetSurface("Xplorer") == True)
        ##Cluster Node button
        self.bCluster.Enable(CLUSTER_ENABLED and
                             self.state.GetSurface("Xplorer") == True and
                             self.state.GetSurface("XplorerType") == "OSG-VEPC")
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

    def OnClose(self, event = None):
        """Sends current configuration back to parent & closes window."""
        ##Close.
        self.UpdateData()
        self.Hide()
        self.Destroy()
