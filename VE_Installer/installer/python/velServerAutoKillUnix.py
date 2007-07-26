# /*************** <auto-copyright.pl BEGIN do not edit this line> *************
# *
# * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
# *
# * Original Development Team:
# *   - ISU's Thermal Systems Virtual Engineering Group,
# *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
# *   - Reaction Engineering International, www.reaction-eng.com
# *
# * This library is free software; you can redistribute it and/or
# * modify it under the terms of the GNU Library General Public
# * License as published by the Free Software Foundation; either
# * version 2 of the License, or (at your option) any later version.
# *
# * This library is distributed in the hope that it will be useful,
# * but WITHOUT ANY WARRANTY; without even the implied warranty of
# * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# * Library General Public License for more details.
# *
# * You should have received a copy of the GNU Library General Public
# * License along with this library; if not, write to the
# * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# * Boston, MA 02111-1307, USA.
# *
# * -----------------------------------------------------------------
# * Date modified: $Date: 2007-06-19 13:48:43 -0500 (Tue, 19 Jun 2007) $
# * Version:       $Rev: 8225 $
# * Author:        $Author: mikelem $
# * Id:            $Id: velServerKillWindow.py 8225 2007-06-19 18:48:43Z mikelem $
# * -----------------------------------------------------------------
# *
# *************** <auto-copyright.pl END do not edit this line> **************
"""VE-Launcher's Name Server Shutdown window"""
import wx
import os, sys, string
from velBase import *
from sys import exc_info
from subprocess import *
from time import sleep
if windows:
    import win32api
    import wmi

class ServerAutoKillUnix(wx.Frame):
    """A window to shutdown the Nameserver after launch.
    Functions:
        __init__(pids, [parent, title])
        KillNameserver(event)
        OnClose(event)"""
    def __init__(self, pids, conduct_Pid, parent = None, id = -1, title = ""):
        """Creates the Server Shutdown Window"""
        wx.Frame.__init__(self, parent, id, title, size = (0, 0), 
                          style = wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE) 

        self.pids = pids
        self.c_Pid = str(conduct_Pid[0])
        self.Show()
        self.OnClose()
        self.KillNameServer()
        
    def KillNameServer(self):
        
	    ps = os.popen("ps | grep " + self.c_Pid + " &").readline().split()

	    while (len(ps) <= 4):
	        ps = os.popen("ps | grep " + self.c_Pid + " &").readline().split()
	        sleep(2)

            killArray = ["kill"]
            for pid in self.pids:
                killArray.append(str(pid))
                Popen(killArray)

        	     
    def OnClose(self, event = None):
        #Closes Server Shutdown Window
        self.Hide()
        self.Destroy()
