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
#include "GLCanvasSample.h"
#include "GLCanvasSample_UI_Dialog.h"
#include "sampleglcanvas.xpm"

IMPLEMENT_DYNAMIC_CLASS(GLCanvasSample, ves::conductor::UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
GLCanvasSample
::GLCanvasSample()
{
  RegistVar("radius", &radius);
  RegistVar("length", &length);
  RegistVar("width", &width);
  RegistVar("type", &type);

  wxImage my_img( sampleglcanvas_xpm );
    SetImage( my_img );
    mDescription = _("Sample App using wxWidgets GLCanvas");
}



/////////////////////////////////////////////////////////////////////////////
GLCanvasSample
::~GLCanvasSample()
{

}

/////////////////////////////////////////////////////////////////////////////
double GLCanvasSample::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}
/////////////////////////////////////////////////////////////////////////////
int GLCanvasSample::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSample::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GLCanvasSample::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GLCanvasSample::GetOPorts(POLY &oports)
{
    
}
/////////////////////////////////////////////////////////////////////////////
UIDialog* GLCanvasSample::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GLCanvasSample_UI_Dialog(parent, -1,
     &radius,
     &length,
     &width,
     &type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSample::GetName()
{
  wxString result="Samples_GLCanvas"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GLCanvasSample::GetDesc()
{
  wxString result="Sample App using wxWidgets GLCanvas"; //your description

  return result;
}


