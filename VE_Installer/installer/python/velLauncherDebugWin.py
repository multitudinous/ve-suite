"""VE-Launcher's Cluster window."""
import wx
import os
from velBase import *
from string import *

class LauncherDebugWin(wx.Frame):
    """A window for editing a list of clustered computers.

    Functions:
        __init__(parent, debugOutput)
        UpdateDisplay()
        OnClose(event)
    """
    def __init__(self, parent, debugOutput, title = "Debug Output"):
        """Sets up the window."""
        wx.Frame.__init__(self, parent, wx.ID_ANY, title,
                           style = wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)
        ##Data storage.
        self.debugOutput = debugOutput
        ##Build list box.
        self.debugField = wx.TextCtrl(self, -1, style = wx.TE_READONLY | wx.TE_MULTILINE)
        ##Fill in info.
        self.UpdateDisplay()
        ##Bind buttons.
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        ##Construct layout.
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(self.debugField, 1, wx.ALL | wx.EXPAND, BORDER)
        ##Set size, position.
        mainSizer.SetSizeHints(self)
        self.SetSizer(mainSizer)
        self.SetSize(INITIAL_DEBUG_WINDOW_SIZE)
        self.CenterOnParent(wx.BOTH)
        ##Set the background color.
        Style(self)
        self.Show(True)


    def UpdateDisplay(self):
        """Updates display to match the data."""
        ##Set plain font.
        plainText = wx.TextAttr()
        ##Set bold font.
        boldText = wx.TextAttr()
        boldFont = plainText.GetFont()
        boldFont.SetWeight(wx.FONTWEIGHT_BOLD)
        boldText.SetFont(boldFont)
        for entry in self.debugOutput:
            if entry[1] == "Bold":
                self.debugField.SetDefaultStyle(boldText)
            else:
                self.debugField.SetDefaultStyle(plainText)
            self.debugField.AppendText(entry[0])


    def OnClose(self, event):
        """Closes ExtraVarsWindow."""
        self.Hide()
        self.Destroy()
