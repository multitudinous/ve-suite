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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "SampleMFC_Gauges.h"
#include "SampleMFC_Gauges_UI_Dialog.h"
#include "samplemfcgauge.xpm"

IMPLEMENT_DYNAMIC_CLASS(SampleMFC_Gauges, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
SampleMFC_Gauges
::SampleMFC_Gauges()
{
  RegistVar("dbl1", &dbl1);
  RegistVar("dbl2", &dbl2);
  RegistVar("int1", &int1);
	RegistVar("int2", &int2);
  RegistVar("dbllist", &dbllist);

  wxImage my_img( samplemfcgauge_xpm );
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
SampleMFC_Gauges
::~SampleMFC_Gauges()
{

}

/////////////////////////////////////////////////////////////////////////////
double SampleMFC_Gauges::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int SampleMFC_Gauges::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int SampleMFC_Gauges::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SampleMFC_Gauges::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int SampleMFC_Gauges::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SampleMFC_Gauges::GetOPorts(POLY &oports)
{
    
}
/////////////////////////////////////////////////////////////////////////////
UIDialog* SampleMFC_Gauges::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SampleMFC_Gauges_UI_Dialog(parent, -1,
     &dbl1,
     &dbl2,
     &int1,
		 &int2,
     &dbllist);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString SampleMFC_Gauges::GetName()
{
  wxString result="Samples_MFCGauges"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString SampleMFC_Gauges::GetDesc()
{
  wxString result="Sample With MFC and Gauges"; //your description

  return result;
}


