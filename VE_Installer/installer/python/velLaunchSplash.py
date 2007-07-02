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
# Python Code By:
#
# Andrea Gavana, @ 10 Oct 2005
# Latest Revision: 10 Oct 2005, 15.50 CET
import wx

# These Are Used To Declare If The AdvancedSplash Should Be Destroyed After The
# Timeout Or Not
AS_TIMEOUT = 1
AS_NOTIMEOUT = 2

# These Flags Are Used To Position AdvancedSplash Correctly On Screen
AS_CENTER_ON_SCREEN = 4
AS_CENTER_ON_PARENT = 8
AS_NO_CENTER = 16

# This Option Allow To Mask A Colour In The Input Bitmap
AS_SHADOW_BITMAP = 32

#----------------------------------------------------------------------
# ADVANCEDSPLASH Class
# This Is The Main Class Implementation. See __init__() Method For
# Details.
#----------------------------------------------------------------------

class AdvancedSplash(wx.Frame):

    def __init__(self, parent, id=-1, pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.FRAME_NO_TASKBAR | wx.FRAME_SHAPED | wx.STAY_ON_TOP,
                 bitmap=None, timeout=5000,
                 extrastyle=AS_TIMEOUT | AS_CENTER_ON_SCREEN,
                 shadowcolour=None):

        """ Default Class Constructor.

        Non Standard wxPython Parameters Are:

        a) bitmap: This Must Be A Valid wx.Bitmap, That You May Construct Using
           Whatever Image File Format Supported By wxPython. If The File You Load
           Already Supports Mask/Transparency (Like PNG), The Transparent Areas
           Will Not Be Drawn On Screen, And The AdvancedSplash Frame Will Have
           The Shape Defined Only By *Non-Transparent* Pixels.
           If You Use Other File Formats That Does Not Supports Transparency, You
           Can Obtain The Same Effect As Above By Masking A Specific Colour In
           Your wx.Bitmap. See "shadowcolour" and "extrastyle" Parameters;

        b) timeout: If You Construct AdvancedSplash Using The Style AS_TIMEOUT,
           AdvancedSplash Will Be Destroyed After "timeout" Milliseconds;

        c) extrastyle: This Value Specifies The AdvancedSplash Styles:
           - AS_TIMEOUT: AdvancedSplash Will Be Destroyed After "timeout"
             Milliseconds;
           - AS_NOTIMEOUT: AdvancedSplash Can Be Destroyed By Clicking On It,
             Pressing A Key Or By Explicitly Call The Close() Method;
           - AS_CENTER_ON_SCREEN: AdvancedSplash Will Be Centered On Screen;
           - AS_CENTER_ON_PARENT: AdvancedSplash Will Be Centered On Parent;
           - AS_NO_CENTER: AdvancedSplash Will Not Be Centered;
           - AS_SHADOW_BITMAP: If The Bitmap You Pass As Input Has No Transparency,
             You Can Choose One Colour That Will Be Masked In Your Bitmap. The
             Final Shape Of AdvancedSplash Will Be Defined Only By Non-Transparent
             (Non-Masked) Pixels.

        d) shadowcolour: If You Construct AdvancedSplash Using The Style
           AS_SHADOW_BITMAP, Here You Can Specify The Colour That Will Be Masked On
           Your Input Bitmap. This Has To BeA Valid wxPython Colour.

        """

        wx.Frame.__init__(self, parent, id, "", pos, size, style)

        # Some Error Checking
        if extrastyle & AS_TIMEOUT and timeout <= 0:
            raise '\nERROR: Style "AS_TIMEOUT" Used With Invalid "timeout" Parameter Value (' \
                  + str(timeout) + ')'

        if extrastyle & AS_SHADOW_BITMAP and not shadowcolour.Ok():
            raise '\nERROR: Style "AS_SHADOW_BITMAP" Used With Invalid "shadowcolour" Parameter'
                            
        if not bitmap.Ok():
            raise "\nERROR: Bitmap Passed To AdvancedSplash Is Invalid."
    
        if extrastyle & AS_SHADOW_BITMAP:
            # Our Bitmap Is Masked Accordingly To User Input
            self.bmp = self.ShadowBitmap(bitmap, shadowcolour)
        else:
            self.bmp = bitmap

        self._extrastyle = extrastyle

        # Setting Initial Properties        
        self.SetText()
        self.SetTextFont()
        self.SetTextPosition()
        self.SetTextColour()

        # Calculate The Shape Of AdvancedSplash Using The Input-Modified Bitmap        
        self.reg = wx.RegionFromBitmap(self.bmp)

        # Don't Know If It Works On Other Platforms!!
        # Tested Only In Windows XP/2000
        
        if wx.Platform == "__WXMSW__":
            self.SetSplashShape()
        else:
            self.Bind(wx.EVT_WINDOW_CREATE, self.SetSplashShape())
            
        w = self.bmp.GetWidth()
        h = self.bmp.GetHeight()

        # Set The AdvancedSplash Size To The Bitmap Size
        self.SetSize((w, h))

        if extrastyle & AS_CENTER_ON_SCREEN:
            self.CenterOnScreen()
        elif extrastyle & AS_CENTER_ON_PARENT:
            self.CenterOnParent()        

        if extrastyle & AS_TIMEOUT:
            # Starts The Timer. Once Expired, AdvancedSplash Is Destroyed
            self._splashtimer = wx.PyTimer(self.OnNotify)
            self._splashtimer.Start(timeout)

        # Catch Some Mouse Events, To Behave Like wx.SplashScreen
        self.Bind(wx.EVT_PAINT, self.OnPaint)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
        self.Bind(wx.EVT_MOUSE_EVENTS, self.OnMouseEvents)
        self.Bind(wx.EVT_CHAR, self.OnCharEvents)
        
        self.Show()


    def SetSplashShape(self):
        """ Sets AdvancedSplash Shape Using The Region Created From The Bitmap."""
        
        self.SetShape(self.reg)
        
        
    def ShadowBitmap(self, bmp, shadowcolour):
        """ Applies A Mask On The Bitmap Accordingly To User Input. """
        
        mask = wx.Mask(bmp, shadowcolour)
        bmp.SetMask(mask)

        return bmp


    def OnPaint(self, event):        
        """ Handles The wx.EVT_PAINT For AdvancedSplash. """
        
        dc = wx.PaintDC(self)

        # Here We Redraw The Bitmap Over The Frame
        dc.DrawBitmap(self.bmp, 0, 0, True)

        # We Draw The Text Anyway, Wheter It Is Empty ("") Or Not
        textcolour = self.GetTextColour()
        textfont = self.GetTextFont()
        textpos = self.GetTextPosition()
        text = self.GetText()
        
        dc.SetFont(textfont[0])
        dc.SetTextForeground(textcolour)
        dc.DrawText(text, textpos[0], textpos[1])

        #wx.SafeYield()
        
        event.Skip()


    def OnNotify(self):
        """ Handles The Timer Expiration, And Calls The Close() Method. """
        
        self.Close()
        

    def OnMouseEvents(self, event):
        """ Handles The wx.EVT_MOUSE_EVENTS For AdvancedSplash.

        This Reproduces The Behavior Of wx.SplashScreen."""
        
        if event.LeftDown() or event.RightDown():
            self.Close()

        event.Skip()


    def OnCharEvents(self, event):
        """ Handles The wx.EVT_CHAR For AdvancedSplash.

        This Reproduces The Behavior Of wx.SplashScreen."""
        
        self.Close()
        

    def OnCloseWindow(self, event):
        """ Handles The wx.EVT_CLOSE For AdvancedSplash.

        This Reproduces The Behavior Of wx.SplashScreen."""
        
        if hasattr(self, "_splashtimer"):
            self._splashtimer.Stop()
            del self._splashtimer
            
        self.Destroy()
        
        
    def SetText(self, text=None):
        """ Sets The Text To Be Displayed On AdvancedSplash."""
        
        if text is None:
            text = ""
            
        self._text = text
        
        self.Refresh()
        self.Update()
                

    def GetText(self):
        """ Returns The Text Displayed On AdvancedSplash."""
        
        return self._text


    def SetTextFont(self, font=None):
        """ Sets The Font For The Text In AdvancedSplash."""
        
        if font is None:
            self._textfont = wx.Font(1, wx.SWISS, wx.NORMAL, wx.BOLD, False)
            self._textsize = 10.0
            self._textfont.SetPointSize(self._textsize)
        else:
            self._textfont = font
            self._textsize = font.GetPointSize()
            self._textfont.SetPointSize(self._textsize)

        self.Refresh()
        self.Update()
        

    def GetTextFont(self):
        """ Gets The Font For The Text In AdvancedSplash."""
        
        return self._textfont, self._textsize


    def SetTextColour(self, colour=None):
        """ Sets The Colour For The Text In AdvancedSplash."""
        
        if colour is None:
            colour = wx.BLACK

        self._textcolour = colour
        self.Refresh()
        self.Update()


    def GetTextColour(self):
        """ Gets The Colour For The Text In AdvancedSplash."""
        
        return self._textcolour


    def SetTextPosition(self, position=None):
        " Sets The Text Position Inside AdvancedSplash Frame."""
        
        if position is None:
            position = (0,0)

        self._textpos = position
        self.Refresh()
        self.Update()


    def GetTextPosition(self):
        " Returns The Text Position Inside AdvancedSplash Frame."""
        
        return self._textpos
    

    def GetSplashStyle(self):
        """ Returns A List Of Strings And A List Of Integers Containing The Styles. """
        
        stringstyle = []
        integerstyle = []

        if self._extrastyle & AS_TIMEOUT:
            stringstyle.append("AS_TIMEOUT")
            integerstyle.append(AS_TIMEOUT)

        if self._extrastyle & AS_NOTIMEOUT:
            stringstyle.append("AS_NOTIMEOUT")
            integerstyle.append(AS_NOTIMEOUT)    

        if self._extrastyle & AS_CENTER_ON_SCREEN:
            stringstyle.append("AS_CENTER_ON_SCREEN")
            integerstyle.append(AS_CENTER_ON_SCREEN)

        if self._extrastyle & AS_CENTER_ON_PARENT:
            stringstyle.append("AS_CENTER_ON_PARENT")
            integerstyle.append(AS_CENTER_ON_PARENT)

        if self._extrastyle & AS_NO_CENTER:
            stringstyle.append("AS_NO_CENTER")
            integerstyle.append(AS_NO_CENTER)

        if self._extrastyle & AS_SHADOW_BITMAP:
            stringstyle.append("AS_SHADOW_BITMAP")
            integerstyle.append(AS_SHADOW_BITMAP)
            
        return stringstyle, integerstyle

    
