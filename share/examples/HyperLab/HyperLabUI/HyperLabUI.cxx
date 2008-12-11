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
#include "HyperLabUI.h"
#include "HyperLabUIDialog.h"

// --- wxWidgets Includes --- //
#include <wx/wx.h>

using namespace hyperlab;

IMPLEMENT_DYNAMIC_CLASS( HyperLabUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
HyperLabUI::HyperLabUI()
{
    RegistVar( "portNumber", &portNumber );

    mPluginName = _("HyperLab");

    wxImage my_img( _("Icons/hyper.xpm") );
    icon_w = static_cast< int >( my_img.GetWidth() );
    icon_h = static_cast< int >( my_img.GetHeight() );
    my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );

    n_pts = 4;

    poly[ 0 ] = wxPoint( 0, 0 );
    poly[ 1 ] = wxPoint( icon_w, 0 );
    poly[ 2 ] = wxPoint( icon_w, icon_h );
    poly[ 3 ] = wxPoint( 0, icon_h );
}
////////////////////////////////////////////////////////////////////////////////
HyperLabUI::~HyperLabUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double HyperLabUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabUI::GetNumPoly()
{
    int result = 0;

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int HyperLabUI::GetNumOports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUI::DrawIcon( wxDC* dc )
{
    dc->DrawBitmap( *my_icon, pos.x, pos.y );
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* HyperLabUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new HyperLabUIDialog( parent, -1, serviceList, &portNumber );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString HyperLabUI::GetConductorName()
{
    wxString result( _("NETL_HyperLab") );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString HyperLabUI::GetName()
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = _("PleaseDefineClassName");
    }

    return mPluginName;
}
////////////////////////////////////////////////////////////////////////////////
wxString HyperLabUI::GetDesc()
{
    wxString result = _("None");

    return result;
}
////////////////////////////////////////////////////////////////////////////////
