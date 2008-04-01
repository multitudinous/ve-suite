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
#include "CoinFunnelUI.h"
#include "CoinFunnelUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

IMPLEMENT_DYNAMIC_CLASS( CoinFunnelUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
CoinFunnelUI::CoinFunnelUI()
{
    RegistVar( "portNumber", &portNumber );

    name = wxString( _( "CoinFunnel" ) );

    wxImage my_img( _( "Icons/CoinFunnel.xpm" ) );
    icon_w = static_cast< int >( my_img.GetWidth() );
    icon_h = static_cast< int >( my_img.GetHeight() );
    my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );

    n_pts = 4;

    poly = new wxPoint[ n_pts ];
    poly[0] = wxPoint( 0, 0 );
    poly[1] = wxPoint( icon_w, 0 );
    poly[2] = wxPoint( icon_w, icon_h );
    poly[3] = wxPoint( 0, icon_h );
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelUI::~CoinFunnelUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double CoinFunnelUI::GetVersion()
{
    double result = 1.0;
    //Your code

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int CoinFunnelUI::GetNumPoly()
{
    int result = 0;
    //Your code

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int CoinFunnelUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int CoinFunnelUI::GetNumOports()
{
    int result = 0;
    //Your code

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelUI::DrawIcon( wxDC* dc )
{
    //Your implementation
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* CoinFunnelUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new CoinFunnelUIDialog( parent, -1, serviceList, &portNumber );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString CoinFunnelUI::GetConductorName()
{         
    //Your name
    wxString result( _( "Demo_CoinFunnel" ) );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString CoinFunnelUI::GetName()
{
    return name;
}
////////////////////////////////////////////////////////////////////////////////
wxString CoinFunnelUI::GetDesc()
{
    //Your description
    wxString result( _( "None" ) );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
