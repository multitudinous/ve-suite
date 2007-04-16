"""VE-Launcher's Name Server Shutdown window"""
import wx
import os
from velBase import *
from sys import exc_info
from subprocess import *
if windows:
    import win32api

class ServerKillWindow(wx.Frame):
    """A window to shutdown the Nameserver after launch.

    Functions:
        __init__(pids, [parent, title])
        KillNameserver(event)
        OnClose(event)"""
    def __init__(self, pids, parent = None, title = "Shutdown Name Server"):
        """Creates the Server Shutdown Window"""
        wx.Frame.__init__(self, parent, wx.ID_ANY, title, wx.Point(0, 0),
                          style = wx.DEFAULT_FRAME_STYLE &
                          ~(wx.RESIZE_BORDER | wx.CLOSE_BOX | wx.MAXIMIZE_BOX))
        self.pids = pids
        lblMsg = wx.StaticText(self, -1, "After you're done with VE-Suite,\n"+\
                                         "press the button below to shutdown\n"+\
                                         "the Name Server.")
        bDone = wx.Button(self, -1, "Shutdown Name Server")
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
        #self.CentreOnScreen()
        self.Show()
    
    def KillNameserver(self, event):
        """Shutdown any Nameservers running on this computer."""
        if windows:
            PROCESS_TERMINATE = 1
            for pid in self.pids:
                try:
                    handle = win32api.OpenProcess(PROCESS_TERMINATE,
                                                  False, pid)
                    win32api.TerminateProcess(handle, -1)
                    win32api.CloseHandle(handle)
                except:
                    pass
        elif unix:
            killArray = ["kill"]
            for pid in self.pids:
                killArray[len(killArray):] = [str(pid)]
            Popen(killArray)
        self.OnClose()

    def OnClose(self, event = None):
        """Closes Server Shutdown Window"""
        self.Hide()
        self.Destroy()
