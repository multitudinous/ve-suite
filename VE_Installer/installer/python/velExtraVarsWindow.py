"""VE-Launcher's Cluster window."""
import wx
import os
from velBase import *
from velCoveredConfig import *
from string import *

class ExtraVarsWindow(wx.Dialog):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, state)
        UpdateData()
        UpdateDisplay(cursor)
        AddNew(event)
        Delete(event)
        OnClose(event)
    """
    def __init__(self, parent, state, title = "Extra Env. Variables to Pass"):
        """Sets up the window."""
        wx.Dialog.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.state = state
        ##Build master display.
        self.variableBlock = wx.TextCtrl(self, -1, style = wx.TE_MULTILINE)
        self.variableBlock.SetToolTip(wx.ToolTip("Enter a list of environment variables."))
        ##Build OK button.
        bOk = wx.Button(self, -1, "Ok")
        bOk.SetToolTip(wx.ToolTip("Return to Settings."))
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_BUTTON, self.OnClose, bOk)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(self.variableBlock, 1, wx.ALL | wx.EXPAND, BORDER)
        mainSizer.Add(bOk, 0, wx.EXPAND, BORDER)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_JCONF_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        Style(self)


    def UpdateData(self):
        """Updates data to match the display."""
        self.state.Edit("ExtraVariables", self.variableBlock.GetValue())
        return


    def UpdateDisplay(self, cursor = None):
        """Updates display to match the data.."""
        ##Set cursor if it's blank.
        ##Master Node
        self.variableBlock.SetValue(self.state.GetSurface("ExtraVariables"))
        self.variableBlock.Enable(self.state.IsEnabled("ExtraVariables"))


    def OnClose(self, event):
        """Closes ExtraVarsWindow."""
        self.UpdateData()
        self.Hide()
        self.Destroy()
