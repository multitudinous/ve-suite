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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
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
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]= wxPoint(0,0);
  poly[1]= wxPoint(icon_w,0);
  poly[2]= wxPoint(icon_w,icon_h);
  poly[3]= wxPoint(0,icon_h);
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
int AverageAirTemp::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
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
void AverageAirTemp::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
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


