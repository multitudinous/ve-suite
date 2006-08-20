"""VE-Launcher's Settings window."""
import os ##Used for setting environmental variables, running programs
import wx ##Used for GUI

from velBase import *
from velJconfWindow import *
from velClusterWindow import *
from velJconfDict import *
from velClusterDict import *

class SettingsWindow(wx.Dialog):
    """Subwindow for viewing/changing mode settings."""
    def __init__(self, parent,
                 jconf, jconfSelection, clusterDict, clusterMaster,
                 desktop, nameServer, conductor, xplorer, xplorerType,
                 modeRules, modeName, position = wx.DefaultPosition):
        """Creates the Settings window."""
        wx.Dialog.__init__(self, parent, -1, "%s Settings" %(modeName),
                           pos = position,
                           style = wx.DEFAULT_FRAME_STYLE &
                           ~ (wx.RESIZE_BORDER | wx.RESIZE_BOX |
                           wx.MAXIMIZE_BOX))
        ##While creating, go through each rule listed in modeRules.
        ##Change the corresponding controls to the value given.
        ##If it's FIXED, prevent the user from changing it.
        ##If a rule isn't given, it uses its regular value.
        ##Set up data.
        self.JconfDict = jconf
        self.clusterDict = clusterDict
        self.clusterMaster = clusterMaster
        self.xplorerTypeFixed = False
        self.desktop = desktop
        if ("xplorerType" in modeRules) and \
           (modeRules["xplorerType"][0] == FIXED):
            self.xplorerTypeFixed = True
        self.desktopFixed = False
        if ("desktop" in modeRules) and (modeRules["desktop"][0] == FIXED):
            self.desktopFixed = True
        ##Jconf pull-down menu.
        self.chJconf = wx.Choice(self, -1)
        self.chJconf.SetToolTip(wx.ToolTip("Choose Xplorer's configuration."))
        ##Edit Jconf button.
        self.bEditJconf = wx.Button(self, -1, "Edit Configuration List")
        self.bEditJconf.SetToolTip(wx.ToolTip("Edit the list of Xplorer" +
                                              " configurations."))
        if "jconf" in modeRules:
            self.chJconf.Append(modeRules["jconf"][1])
            self.chJconf.SetSelection(0)
            self.chJconf.Enable(modeRules["jconf"][0])
            self.bEditJconf.Enable(modeRules["jconf"][0])
        elif ("xplorer" in modeRules) and (modeRules["xplorer"][0] == FIXED) \
             and (modeRules["xplorer"][1] == False):
            self.chJconf.Append("None")
            self.chJconf.SetSelection(0)
            self.chJconf.Enable(False)
            self.bEditJconf.Enable(False)
        else:
            self.UpdateChJconf(jconfSelection)
        ##Name Server checkbox.
        self.cbNameServer = wx.CheckBox(self, -1, "Name Server")
        self.cbNameServer.SetToolTip(wx.ToolTip("Run Name Server at Launch"))
        if "nameServer" in modeRules:
            self.cbNameServer.SetValue(modeRules["nameServer"][1])
            self.cbNameServer.Enable(modeRules["nameServer"][0])
        else:
            self.cbNameServer.SetValue(nameServer)
        ##Conductor checkbox.
        self.cbConductor = wx.CheckBox(self, -1, "Conductor")
        self.cbConductor.SetToolTip(wx.ToolTip("Run Conductor at Launch"))
        if "conductor" in modeRules:
            self.cbConductor.SetValue(modeRules["conductor"][1])
            self.cbConductor.Enable(modeRules["conductor"][0])
        else:
            self.cbConductor.SetValue(conductor)
        ##Xplorer checkbox.
        self.cbXplorer = wx.CheckBox(self, -1, "Xplorer")
        self.cbXplorer.SetToolTip(wx.ToolTip("Run Xplorer at Launch"))
        if "xplorer" in modeRules:
            self.cbXplorer.SetValue(modeRules["xplorer"][1])
            self.cbXplorer.Enable(modeRules["xplorer"][0])
        else:
            self.cbXplorer.SetValue(xplorer)
        ##Desktop checkbox.
        self.cbDesktop = wx.CheckBox(self, -1, "Desktop Mode")
        self.cbDesktop.SetToolTip(wx.ToolTip("Set Desktop Mode for" +
                                             " Conductor and Xplorer"))
        if "desktop" in modeRules:
            self.cbDesktop.SetValue(modeRules["desktop"][1])
            self.cbDesktop.Enable(modeRules["desktop"][0])
        else:
            self.cbDesktop.SetValue(self.desktop)
        ##Xplorer Type radio box.
        self.rbXplorer = wx.RadioBox(self, -1, "Xplorer Type",
                                     wx.DefaultPosition, wx.DefaultSize,
                                     RADIO_XPLORER_LIST, 2, wx.RA_SPECIFY_ROWS)
        self.rbXplorer.SetToolTip(wx.ToolTip("Which Xplorer format do you" +
                                             " want to launch?"))
        if "xplorerType" in modeRules:
            self.rbXplorer.SetSelection(modeRules["xplorerType"][1])
            self.rbXplorer.Enable(modeRules["xplorerType"][0])
        else:
            self.rbXplorer.SetSelection(xplorerType)
        ##Cluster button.
        self.bCluster = wx.Button(self, -1, "Set Cluster Computers")
        self.bCluster.SetToolTip(wx.ToolTip("Set the computers in" +
                                            " the cluster."))
        if os.name == "nt" or not CLUSTER_ENABLED:
            self.bCluster.Hide()
        ##Set up OK button.
        bOk = wx.Button(self, -1, "OK")
        bOk.SetToolTip(wx.ToolTip("Return to the Launcher."))
        self.EvtCheckXplorer("dead parrot sketch")
        ##Also calls self.EvtCheckDesktop as a follow-up
        ##Bind events.
        self.Bind(wx.EVT_CHECKBOX, self.EvtCheckXplorer, self.cbXplorer)
        self.Bind(wx.EVT_RADIOBOX, self.EvtCheckCluster, self.rbXplorer)
        self.Bind(wx.EVT_CHECKBOX, self.EvtCheckDesktop, self.cbConductor)
        self.Bind(wx.EVT_CHECKBOX, self.UpdateDesktop, self.cbDesktop)
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
        if os.name == "nt":
            gridSizer.Add((-1, -1))
        else:
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
##        self.SetSize(INITIAL_WINDOW_SIZE)
        ##Set the background color.
        Style(self)

    def EvtCheckXplorer(self, event):
        """Enables/Disables Xplorer's radio box.

        Prevents the user from choosing Xplorer's mode if Xplorer
        won't be launched. The radio box is enabled if Xplorer's
        check box is checked, disabled if it isn't."""
        if self.xplorerTypeFixed == True:
            self.rbXplorer.Enable(False)
        else:
            self.rbXplorer.Enable(self.cbXplorer.IsChecked())
        ##Goes into EvtCheckCluster to check that against cbXplorer, too.
        self.EvtCheckCluster("dead parrot sketch")

    def EvtCheckCluster(self, event):
        """Enables/Disables the Cluster button.

        Prevents the user from choosing Cluster computers if
        Xplorer won't run in OSG Cluster mode."""
        if self.cbXplorer.IsChecked() and self.rbXplorer.GetSelection() == 2:
            self.bCluster.Enable(True)
        else:
            self.bCluster.Enable(False)
        ##Goes into EvtCheckDesktop to check that against rbXplorer, too.
        self.EvtCheckDesktop("dead parrot sketch")

    def EvtCheckDesktop(self, event):
        """Enables/Disables the Desktop button.

        Prevents the user from choosing Desktop mode if
        Conductor and Xplorer won't be launched."""
        if self.desktopFixed == True:
            self.cbDesktop.Enable(False)
        else:
            self.cbDesktop.Enable((self.cbConductor.IsChecked() or
                                   self.cbXplorer.IsChecked()) and
                                  (not self.cbXplorer.IsChecked() or
                                   self.rbXplorer.GetSelection() != 2))
            if self.cbDesktop.IsEnabled() == False:
                self.cbDesktop.SetValue(False)
            else:
                self.cbDesktop.SetValue(self.desktop)

    def UpdateDesktop(self, event):
        self.desktop = self.cbDesktop.GetValue()

    def UpdateChJconf(self, selection):
        """Updates the Jconf choice window in the Launcher.

        Keyword arguments:
        selection -- name of selected choice)"""
        ##Rebuild the choice list
        self.chJconf.Clear()
        nameArray = self.JconfDict.GetNames()
        for i in range(len(nameArray)):
            self.chJconf.Append(nameArray[i])
        ##Set the selection
        if selection not in nameArray:
            selection = self.chJconf.GetString(0)
        ##Error catcher for lists without any items. Should never happen.
        if self.chJconf.GetCount() == 0:
            cursor = wx.NOT_FOUND
        ##Set the selection
        self.chJconf.SetStringSelection(selection)

    def EditJconf(self, event):
        """Brings up the Jconf editing window."""
        jconfWindow = JconfWindow(self, wx.ID_ANY, "Xplorer Configurations",
                                  self.JconfDict,
                                  self.chJconf.GetStringSelection())
        jconfWindow.ShowModal()
        ##jconfWindow.Destroy()        

    def GetSelectedJconf(self):
        """Returns the path of the selected Jconf file."""
        jconfFile = self.JconfDict.GetPath(self.chJconf.GetStringSelection())
        return jconfFile

    def EditCluster(self, event):
        """Brings up the Cluster editing window."""
        clusterWindow = ClusterWindow(self, self.clusterDict,
                                      self.clusterMaster)
        clusterWindow.ShowModal()
        ##clusterWindow.Destroy()

    ##Saves the current configuration under the prefs file before closing.
    def OnClose(self, event):
        """Sends current configuration back to parent & closes window."""
        ##Update parent.
        parent = self.GetParent()
        if self.cbConductor.IsEnabled():
            parent.conductor = self.cbConductor.GetValue()
        if self.cbNameServer.IsEnabled():
            parent.nameServer = self.cbNameServer.GetValue()
        if self.cbXplorer.IsEnabled():
            parent.xplorer = self.cbXplorer.GetValue()
        if self.cbXplorer.IsEnabled() or self.rbXplorer.IsEnabled():
            parent.xplorerType = self.rbXplorer.GetSelection()
        if self.cbXplorer.IsEnabled() or self.cbConductor.IsEnabled() or \
           self.cbDesktop.IsEnabled():
            parent.desktop = self.desktop
        if self.chJconf.IsEnabled():
            parent.JconfDict = self.JconfDict
            parent.jconfSelection = self.chJconf.GetStringSelection()
        parent.clusterMaster = self.clusterMaster
        ##Close.
        self.Hide()
        self.Destroy()
