"""Window for setting debug levels."""
import wx
from velBase import *

VPR_LIST = ["None",
            "0 (Minimal)",
            "1",
            "2",
            "3",
            "4",
            "5 (Verbose)"]
OSG_LIST = ["None",
            "ALWAYS",
            "FATAL",
            "WARN",
            "NOTICE",
            "INFO",
            "DEBUG_INFO",
            "DEBUG_FP"]
READ = True
WRITE = False

class DebugWindow(wx.Dialog):
    """Subwindow for changing debug settings for VE-Suite."""
    def __init__(self, parent, state):
        wx.Dialog.__init__(self, parent, wx.ID_ANY, "Debug Settings",
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build VPR menu.
        self.vprMenu = wx.Choice(self, -1, choices = VPR_LIST)
        self.vprMenu.SetToolTip(wx.ToolTip("Choose the VPR debug level" +
                                           " for VE-Suite."))
        ##Build OSG menu.
        self.osgMenu = wx.Choice(self, -1, choices = OSG_LIST)
        self.osgMenu.SetToolTip(wx.ToolTip("Choose the OSG debug level" +
                                           " for VE-Suite."))
        ##Build Launch Debug checkbox.
        self.cbLauncherDebug = wx.CheckBox(self, -1, "Debug Launch")
        self.cbLauncherDebug.SetToolTip(wx.ToolTip("Debug VE-Suite's launch."))
        ##Build VE-Suite Debug checkbox.
        self.cbRunDebugPrograms = wx.CheckBox(self, -1,
                                              "Run VE-Suite, Debug Build")
        self.cbRunDebugPrograms.SetToolTip(wx.ToolTip("Launch debug build of" +
                                           " VE-Suite instead of opt build."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Launcher."))
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "VPR Debug Level:"))
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.vprMenu, 1)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "OSG Debug Level:"))
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.osgMenu, 1)
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(columnSizer, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(VERTICAL_SPACE)
        mainSizer.Add(self.cbLauncherDebug, 0, wx.LEFT | wx.RIGHT | wx.EXPAND,
                      BORDER)
        mainSizer.Add(self.cbRunDebugPrograms, 0, wx.TOP | wx.LEFT |
                      wx.RIGHT | wx.EXPAND, BORDER)
        if not windows:
            self.cbRunDebugPrograms.Hide()
        mainSizer.Add(bOk, 0, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.CenterOnParent(wx.BOTH)
        ##Show self
        self.Show()

    def VPRDebug(self, mode = READ):
        if mode == READ:
            return self.state.GetSurface("VPRDebug") + 1
        else: ##mode == WRITE
            return self.vprMenu.GetSelection() - 1

    def UpdateData(self):
        if self.vprMenu.IsEnabled():
            self.state.Edit("VPRDebug", self.VPRDebug(WRITE))
        if self.osgMenu.IsEnabled():
            self.state.Edit("OSGNotifyLevel", self.osgMenu.GetStringSelection())
        if self.cbLauncherDebug.IsEnabled():
            self.state.Edit("Debug", self.cbLauncherDebug.GetValue())
        if self.cbRunDebugPrograms.IsEnabled():
            self.state.Edit("RunDebugPrograms",
                            self.cbRunDebugPrograms.GetValue())

    def UpdateDisplay(self):
        ##VPR Menu
        self.vprMenu.SetSelection(self.VPRDebug(READ))
        self.vprMenu.Enable(self.state.IsEnabled("VPRDebug"))
        ##OSG Field
        self.osgMenu.SetStringSelection(self.state.GetSurface("OSGNotifyLevel"))
        self.osgMenu.Enable(self.state.IsEnabled("OSGNotifyLevel"))
        ##Checkboxes
        self.cbLauncherDebug.SetValue(self.state.GetSurface("Debug"))
        self.cbLauncherDebug.Enable(self.state.IsEnabled("Debug"))
        self.cbRunDebugPrograms.SetValue(self.state.GetSurface("RunDebugPrograms"))
        self.cbRunDebugPrograms.Enable(self.state.IsEnabled("RunDebugPrograms"))

    def OnClose(self, event = None):
        self.UpdateData()
        self.Hide()
        self.Destroy()
        
