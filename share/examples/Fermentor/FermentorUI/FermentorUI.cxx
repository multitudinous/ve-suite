/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- My Includes --- //
#include "FermentorUI.h"
#include "FermentorUIDialog.h"

// --- VE-Suite Includes --- //
//#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

IMPLEMENT_DYNAMIC_CLASS( FermentorUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
FermentorUI::FermentorUI()
{
    RegistVar( "agitation", &agitation );
    RegistVar( "air_conc", &air_conc );
    RegistVar( "ini_ph", &ini_ph );
    RegistVar( "nitrate_conc", &nitrate_conc );
    RegistVar( "temperature", &temperature );
    RegistVar( "hours", &hours );
    RegistVar( "cycle_ID", &cycle_ID );
    RegistVar( "rotation_ID", &rotation_ID );
    RegistVar( "xray_ID", &xray_ID );
    RegistVar( "loop_ID", &loop_ID );
    RegistVar( "rot_speed", &rot_speed );
    RegistVar( "sim_speed", &sim_speed );

    name = wxString( _( "Fermentor" ) );

    wxImage my_img( _( "Icons/fermentor.jpg" ) );
    icon_w = static_cast< int >( my_img.GetWidth() * 0.1 );
    icon_h = static_cast< int >( my_img.GetHeight() * 0.1 );
    my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );

    n_pts = 4;

    poly = new wxPoint[ n_pts ];
    poly[0] = wxPoint( 0, 0 );
    poly[1] = wxPoint( icon_w, 0 );
    poly[2] = wxPoint( icon_w, icon_h );
    poly[3] = wxPoint( 0, icon_h );
}
////////////////////////////////////////////////////////////////////////////////
FermentorUI::~FermentorUI()
{
    delete my_icon;
}
////////////////////////////////////////////////////////////////////////////////
double FermentorUI::GetVersion()
{
    double result = 1.0;
    //Your code

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int FermentorUI::GetNumPoly()
{
    int result = 0;
    //Your code

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int FermentorUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int FermentorUI::GetNumOports()
{
    int result = 0;
    //Your code

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUI::DrawIcon( wxDC* dc )
{
    //Your implementation
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* FermentorUI::UI( wxWindow* parent )
{
    if( dlg )
    {
        return dlg;
    }
  
    dlg = new FermentorUIDialog( parent, wxID_ANY, &agitation,
                                             &air_conc,
                                             &ini_ph,
                                             &nitrate_conc,
                                             &temperature,
                                             &hours,
                                             &cycle_ID,
                                             &rotation_ID,
                                             &xray_ID,
                                             &loop_ID,
                                             &rot_speed,
                                             &sim_speed );
    ConfigurePluginDialogs( dlg );

    return dlg;
}

////////////////////////////////////////////////////////////////////////////////
wxString FermentorUI::GetConductorName()
{         
    //Your name
    wxString result( _( "IHCC_Fermentor" ) );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString FermentorUI::GetName()
{
    return name;
}

////////////////////////////////////////////////////////////////////////////////
wxString FermentorUI::GetDesc()
{
    //Your description
    wxString result( _( "None" ) );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
