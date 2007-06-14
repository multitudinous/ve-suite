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
# * Date modified: $Date$
# * Version:       $Rev$
# * Author:        $Author$
# * Id:            $Id$
# * -----------------------------------------------------------------
# *
# *************** <auto-copyright.pl END do not edit this line> **************
import os
import wx

from velBase import *

class LaunchSplash(wx.SplashScreen):
    def __init__(self):
        try:
            image = wx.Bitmap(SPLASH_IMAGE, wx.BITMAP_TYPE_XPM)
            wx.SplashScreen.__init__(self, image,
                                     wx.SPLASH_CENTRE_ON_SCREEN | wx.SPLASH_TIMEOUT,
                                     SPLASH_TIME, None, -1)
            self.Bind(wx.EVT_CLOSE, self.OnClose)
	except:
	    pass

        #wx.Yield() 
	
    def OnClose(self, event):
        self.Hide()
        self.Destroy()
