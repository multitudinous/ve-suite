"""VE-Launcher's Name Server Kill window."""
import wx
import os
from velBase import *
if windows:
    import win32api

class ServerKillWindow(wx.Frame):
    """A window to kill the Nameserver after launch."""
    def __init__(self, pids = [], parent = None, title = "Kill Name Server"):
        """Creates the Server Kill Window."""
        wx.Frame.__init__(self, parent, wx.ID_ANY, title,
                          style = wx.DEFAULT_FRAME_STYLE &
                          ~ (wx.RESIZE_BORDER | wx.CLOSE_BOX | wx.MAXIMIZE_BOX))
        self.pids = pids
        lblMsg = wx.StaticText(self, -1, "After you're done with VE-Suite,\n"+\
                                         "press the button below to kill\n"+\
                                         "the Name Server.")
        bDone = wx.Button(self, -1, "Kill Name Server")
        self.Bind(wx.EVT_BUTTON, self.KillNameserver, bDone)
        rowSizer = wx.BoxSizer(wx.VERTICAL)
        border = 10
        rowSizer.Add(lblMsg, 2, wx.ALL, border)
        rowSizer.Add(bDone, 1, wx.EXPAND)
        rowSizer.SetMinSize(KILL_WINDOW_SIZE)
        rowSizer.SetSizeHints(self)
        Style(self)
        self.SetSizer(rowSizer)
        self.SetSize(KILL_WINDOW_SIZE)
        self.CentreOnScreen()
        self.Show()
    
    def KillNameserver(self, event):
        """Kills any Nameservers running on this computer."""
        if windows:
            PROCESS_TERMINATE = 1
            for pid in self.pids:
                handle = win32api.OpenProcess(PROCESS_TERMINATE,
                                              False, pid)
                win32api.TerminateProcess(handle, -1)
                win32api.CloseHandle(handle)
        elif unix:
            os.system("killall Naming_Service Exe_server")
        self.OnClose("this event doesn't exist")

    def OnClose(self, event):
        """Closes ServerKillWindow."""
        self.Hide()
        self.Destroy()
