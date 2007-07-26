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
import wx

class VeSplashScreen(wx.Frame):
    def __init__(self, parent, ID=-1, title="",
                 style=wx.SIMPLE_BORDER | wx.FRAME_NO_TASKBAR | wx.STAY_ON_TOP,
                 duration=1500, bitmapfile=""):

        ### Loading bitmap
        self.bitmap = bmp = wx.Image(bitmapfile, wx.BITMAP_TYPE_ANY).ConvertToBitmap()

        ### Determine size of bitmap to size window...
        size = (bmp.GetWidth(), bmp.GetHeight())
        self.textfont = wx.Font(1, wx.SWISS, wx.NORMAL, wx.NORMAL, False , "Arial")
        self.textpos = (0,0)
        self.textcolor = wx.BLACK
        self.text1 = ""
        self.text2 = ""
        self.textsize = 10.0
        self.textfont.SetPointSize(self.textsize)

        # size of screen
        width = wx.SystemSettings_GetMetric(wx.SYS_SCREEN_X)
        height = wx.SystemSettings_GetMetric(wx.SYS_SCREEN_Y)
        pos = ((width-size[0])/2, (height-size[1])/2)

        # check for overflow...
        if pos[0] < 0:
            size = (wx.SystemSettings_GetSystemMetric(wx.SYS_SCREEN_X), size[1])
        if pos[1] < 0:
            size = (size[0], wx.SystemSettings_GetSystemMetric(wx.SYS_SCREEN_Y))

        # Setting Initial Properties        
        wx.Frame.__init__(self, parent, ID, title, pos, size, style)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
        self.Bind(wx.EVT_PAINT, self.OnPaint)
        self.Bind(wx.EVT_ERASE_BACKGROUND, self.OnEraseBG)

        self.Show(True)


    def OnPaint(self, event):
        # We Draw The Text Anyway, Wheter It Is Empty ("") Or Not
        textcolor = self.GetTextColor()
        textfont = self.GetTextFont()
        textpos = self.GetTextPosition()
        text1 = self.GetText1()
        text2 = self.GetText2()

        dc = wx.PaintDC(self)
 
        dc.SetFont(textfont[0])
        dc.SetTextForeground(textcolor)
                
        dc.DrawBitmap(self.bitmap, 0,0, False)
        dc.DrawText(text1, 155, 43)
        dc.DrawText(text2, 155, 58)


    def OnEraseBG(self, event):
        pass


    def OnSplashExitDefault(self, event=None):
        self.Close(True)


    def OnCloseWindow(self, event=None):
        self.Show(False)
        self.Destroy()

    
    def SetText(self, text1 = None, text2 = None):
        """ Sets The Text To Be Displayed On AdvancedSplash."""
        
        if text1 is None:
            text1 = ""

        if text2 is None:
            text2 = ""
            
        self.text1 = text1
        self.text2 = text2
        
        self.Refresh()
        self.Update()
                

    def GetText1(self):
        """ Returns The Text Displayed On AdvancedSplash."""
        return self.text1


    def GetText2(self):
        """ Returns The Text Displayed On AdvancedSplash."""
        return self.text2


    def SetTextFont(self, font=None):
        """ Sets The Font For The Text In AdvancedSplash."""
        
        if font is None:
            self.textfont = wx.Font(8.0, wx.SWISS, wx.NORMAL, wx.NORMAL, False , "Arial")
            self.textsize = 8.0
            self.textfont.SetPointSize(self.textsize)
        else:
            self.textfont = font
            self.textsize = font.GetPointSize()
            self.textfont.SetPointSize(self.textsize)

        self.Refresh()
        self.Update()
        

    def GetTextFont(self):
        """ Gets The Font For The Text In AdvancedSplash."""
        
        return self.textfont, self.textsize


    def SetTextColor(self, color=None):
        """ Sets The Colour For The Text In AdvancedSplash."""
        
        if color is None:
            color = wx.BLACK

        self.textcolor = color
        self.Refresh()
        self.Update()


    def GetTextColor(self):
        """ Gets The Colour For The Text In AdvancedSplash."""
        
        return self.textcolor


    def SetTextPosition(self, position=None):
        " Sets The Text Position Inside AdvancedSplash Frame."""
        
        if position is None:
            position = (0,0)

        self.textpos = position
        self.Refresh()
        self.Update()


    def GetTextPosition(self):
        " Returns The Text Position Inside AdvancedSplash Frame."""
        
        return self.textpos
