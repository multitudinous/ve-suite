import os
import wx

from velBase import *

class LaunchSplash(wx.SplashScreen):
    def __init__(self):
        image = wx.Image(SPLASH_IMAGE).ConvertToBitmap()
        wx.SplashScreen.__init__(self, image,
                                 wx.SPLASH_CENTRE_ON_SCREEN | wx.SPLASH_TIMEOUT,
                                 SPLASH_TIME, None, -1)
        self.Bind(wx.EVT_CLOSE, self.OnClose)

    def OnClose(self, event):
        self.Hide()
        self.Destroy()
