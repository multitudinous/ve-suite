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
# * Id:            $Id: velServerAutoKillWindow.py 8225 2007-06-19 18:48:43Z mikelem $
# * -----------------------------------------------------------------
# *
# *************** <auto-copyright.pl END do not edit this line> **************
"""VE-Launcher's Name Server Shutdown window"""
import os, sys, string
from velBase import *
from sys import exc_info
from subprocess import *
from time import sleep
if windows:
    import win32api
    import wmi

class ServerAutoKillWin32:
    """A window to shutdown the Nameserver after launch."""

    def __init__(self, pids, conduct_Pid):
        self.pids = pids
        self.c_Pid = str(conduct_Pid[0])
        self.KillNameServer()
        
    def KillNameServer(self):
        c = wmi.WMI()
        pID = self.c_Pid
        ps = len (c.Win32_Process (ProcessId=pID))
        
        while (ps == 1):
            ps = len (c.Win32_Process (ProcessId=pID))
            sleep(2)
            
        PROCESS_TERMINATE = 1
        for pid in self.pids:
            try:
                handle = win32api.OpenProcess(PROCESS_TERMINATE, False, pid)
                win32api.TerminateProcess(handle, -1)
                win32api.CloseHandle(handle)
            except:
                pass
            
        