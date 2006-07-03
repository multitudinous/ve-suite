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
#include "AdiabaticFlameTemp.h"
#include "AdiabaticFlameTemp_UI_Dialog.h"
#include "Adiab_Flame_Temp_Mod.xpm"

IMPLEMENT_DYNAMIC_CLASS(AdiabaticFlameTemp, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
AdiabaticFlameTemp
::AdiabaticFlameTemp()
{
   RegistVar("perc_theor_error", &perc_theor_error);
   RegistVar("closesheets", &closesheets);

   perc_theor_error = 0;

   wxImage my_img( Adiab_Flame_Temp_Mod_xpm );
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
AdiabaticFlameTemp
::~AdiabaticFlameTemp()
{

}

/////////////////////////////////////////////////////////////////////////////
double AdiabaticFlameTemp::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int AdiabaticFlameTemp::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int AdiabaticFlameTemp::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AdiabaticFlameTemp::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*3/43,icon_h*21/41);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int AdiabaticFlameTemp::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AdiabaticFlameTemp::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void AdiabaticFlameTemp::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* AdiabaticFlameTemp::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new AdiabaticFlameTemp_UI_Dialog(parent, -1,
     &perc_theor_error,
     &closesheets);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AdiabaticFlameTemp::GetName()
{
  wxString result="SampleApps_MultipleUnit_AdiabFlameTemp"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AdiabaticFlameTemp::GetDesc()
{
  wxString result="Adiabatic Flame Temp Module"; //your description

  return result;
}


