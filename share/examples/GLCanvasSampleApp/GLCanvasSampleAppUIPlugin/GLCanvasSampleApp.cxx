/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
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
    SetImage( my_img );
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


