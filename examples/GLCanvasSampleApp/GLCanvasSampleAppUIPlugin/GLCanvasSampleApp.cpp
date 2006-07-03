/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "GLCanvasSampleApp.h"
#include "GLCanvasSampleApp_UI_Dialog.h"
#include "sampleglcanvas.xpm"

IMPLEMENT_DYNAMIC_CLASS(GLCanvasSampleApp, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GLCanvasSampleApp
::GLCanvasSampleApp()
{
  RegistVar("radius", &radius);
  radius = 0.0f;
  RegistVar("length", &length);
  length = 0.0f;
  RegistVar("width", &width);
  width = 0.0f;
  RegistVar("xcoord", &xcoord);
  xcoord = 0.0f;
  RegistVar("ycoord", &ycoord);
  ycoord = 0.0f;
  RegistVar("type", &type);
  type = 0;

  wxImage my_img( sampleglcanvas_xpm );
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}



/////////////////////////////////////////////////////////////////////////////
GLCanvasSampleApp
::~GLCanvasSampleApp()
{

}

/////////////////////////////////////////////////////////////////////////////
double GLCanvasSampleApp::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSampleApp::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSampleApp::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSampleApp::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSampleApp::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSampleApp::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSampleApp::DrawIcon(wxDC* dc)
{
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GLCanvasSampleApp::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GLCanvasSampleApp_UI_Dialog(parent, -1,
     &radius,
     &length,
     &width,
     &xcoord,
     &ycoord,
     &type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSampleApp::GetName()
{
  wxString result="Samples_GLCanvas"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSampleApp::GetDesc()
{
  wxString result="Sample App using wxWidgets GLCanvas"; //your description

  return result;
}


