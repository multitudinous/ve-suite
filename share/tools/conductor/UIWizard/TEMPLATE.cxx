/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include "TEMPLATE.h"
#include "TEMPLATE_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(TEMPLATE, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
TEMPLATE
::TEMPLATE()



/////////////////////////////////////////////////////////////////////////////
TEMPLATE
::~TEMPLATE()
{

}

/////////////////////////////////////////////////////////////////////////////
double TEMPLATE::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int TEMPLATE::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int TEMPLATE::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void TEMPLATE::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int TEMPLATE::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void TEMPLATE::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void TEMPLATE::DrawIcon(wxDC* dc)
{
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* TEMPLATE::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new TEMPLATE_UI_Dialog 
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString TEMPLATE::GetName()
{
  wxString result="NEW_MOD"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString TEMPLATE::GetDesc()
{
  wxString result="None"; //your description

  return result;
}

