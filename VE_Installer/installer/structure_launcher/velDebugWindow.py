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
        ##Build OSG text field.
        self.osgCtrl = wx.TextCtrl(self, -1)
        self.osgCtrl.SetToolTip(wx.ToolTip("Set arguments for OSG debug."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        columnSizer = wx.BoxSizer(wx.HORIZONTAL)
        columnSizer.Add(wx.StaticText(self, -1, "VPR Debug Level:"))
        columnSizer.Add(HORIZONTAL_SPACE)
        columnSizer.Add(self.vprMenu, 0, wx.EXPAND)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        rowSizer.Add(columnSizer)
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(wx.StaticText(self, -1, "OSG Debug Arguments:"))
        rowSizer.Add(self.osgCtrl, 0, wx.EXPAND)
        rowSizer.Add(VERTICAL_SPACE)
        rowSizer.Add(bOk, 0, wx.EXPAND)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(rowSizer, 1, wx.ALL | wx.EXPAND, BORDER)
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
        if self.osgCtrl.IsEnabled():
            self.state.Edit("OSGNotifyLevel", self.osgCtrl.GetValue())

    def UpdateDisplay(self):
        ##VPR Menu
        self.vprMenu.SetSelection(self.VPRDebug(READ))
        self.vprMenu.Enable(self.state.IsEnabled("VPRDebug"))
        ##OSG Field
        self.osgCtrl.SetValue(self.state.GetSurface("OSGNotifyLevel"))
        self.osgCtrl.Enable(self.state.IsEnabled("OSGNotifyLevel"))

    def OnClose(self, event = None):
        self.UpdateData()
        self.Hide()
        self.Destroy()
        
