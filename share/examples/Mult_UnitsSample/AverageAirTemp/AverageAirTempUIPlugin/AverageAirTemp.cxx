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
#include "AverageAirTemp.h"
#include "AverageAirTemp_UI_Dialog.h"
#include "Aver_Air_Temp_Mod.xpm"

IMPLEMENT_DYNAMIC_CLASS(AverageAirTemp, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
AverageAirTemp
::AverageAirTemp()
{
  RegistVar("intakediam", &intakediam);
  RegistVar("airvel", &airvel);
  RegistVar("intaketemp", &intaketemp);
  RegistVar("airinlettemp", &airinlettemp);
  RegistVar("intakelength", &intakelength);
  RegistVar("closesheets", &closesheets);

  intakediam = 0;
  airvel = 0;
  intaketemp = 0;
  airinlettemp = 0;
  intakelength = 0;

  wxImage my_img( Aver_Air_Temp_Mod_xpm );
    SetImage( my_img );
}



/////////////////////////////////////////////////////////////////////////////
AverageAirTemp
::~AverageAirTemp()
{

}

/////////////////////////////////////////////////////////////////////////////
double AverageAirTemp::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}
/////////////////////////////////////////////////////////////////////////////
int AverageAirTemp::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int AverageAirTemp::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*40/43,icon_h*21/41); 
}
/////////////////////////////////////////////////////////////////////////////
UIDialog* AverageAirTemp::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new AverageAirTemp_UI_Dialog(parent, -1,
     &intakediam,
     &airvel,
     &intaketemp,
     &airinlettemp,
     &intakelength,
     &closesheets);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AverageAirTemp::GetName()
{
  wxString result="SampleApps_MultipleUnit_AverAirTemp"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AverageAirTemp::GetDesc()
{
  wxString result="Average Air Temp Module"; //your description

  return result;
}


