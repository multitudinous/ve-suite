"""VE-Launcher's Name Server Shutdown window"""
#import wx
import os, sys, string, thread
from velBase import *
from sys import exc_info
from subprocess import *
from time import sleep
if windows:
    import win32api

class ServerKillWindow:
#class ServerKillWindow(wx.Frame):
    """A window to shutdown the Nameserver after launch.
    Functions:
        __init__(pids, [parent, title])
        KillNameserver(event)
        OnClose(event)"""
    def __init__(self, pids, conduct_Pid, parent = None, title = "Shutdown Name Server"):
        """Creates the Server Shutdown Window"""
        #wx.Frame.__init__(self, parent, wx.ID_ANY, title, wx.Point(0, 0),
        #                  style = wx.DEFAULT_FRAME_STYLE &
        #                  ~(wx.RESIZE_BORDER | wx.CLOSE_BOX | wx.MAXIMIZE_BOX))
        self.pids = pids
        self.c_Pid = str(conduct_Pid[0])
        #lblMsg = wx.StaticText(self, -1, "After you're done with VE-Suite,\n"+\
        #                                 "press the button below to shutdown\n"+\
        #                                 "the Name Server.")
        #bDone = wx.Button(self, -1, "Shutdown Name Server")
        #self.Bind(wx.EVT_BUTTON, self.KillNameserver, bDone)
        #rowSizer = wx.BoxSizer(wx.VERTICAL)
        #border = 10
        #rowSizer.Add(lblMsg, 2, wx.ALL, border)
        #rowSizer.Add(bDone, 1, wx.EXPAND)
        #rowSizer.SetMinSize(KILL_WINDOW_SIZE)
        #rowSizer.SetSizeHints(self)
        #Style(self)
        #self.SetSizer(rowSizer)
        #self.SetSize(KILL_WINDOW_SIZE)
        #self.CentreOnScreen()
        #self.Show()
        self.runWhile()
        #thread.start_new_thread(self.test, ())
	
    def runWhile(self):
	if windows:
	   #pslist is precompiled dos systemtool (pslist.exe) which needs to be distributed as a dependency
           ps = os.popen("pslist " + self.c_Pid).read().split()
	   while (len(ps) >= 20):
	       ps = os.popen("pslist " + self.c_Pid).read().split()
	       sleep(2)
           """
           getWinPids = velSearchWinPid.pyperf()
           winPids = getWinPids.procids()

           while self.hasPid(winPids):
              winPids = getWinPids.procids()
              sleep(2)
           """  
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
	   ps = os.popen("ps | grep " + self.c_Pid + " &").readline().split()
	   while (len(ps) <= 4):
	       ps = os.popen("ps | grep " + self.c_Pid + " &").readline().split()
	       sleep(2)
           killArray = ["kill"]
           for pid in self.pids:
               killArray.append(str(pid))
           Popen(killArray)
         
    """	
    def KillNameserver(self, event):
        #Shutdown any Nameservers running on this computer.
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
            os.popen(str(killArray))
        self.OnClose()

    def OnClose(self, event = None):
        #Closes Server Shutdown Window
        self.Hide()
        self.Destroy()
    """		
