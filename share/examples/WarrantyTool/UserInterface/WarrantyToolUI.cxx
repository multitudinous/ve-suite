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
#include "WarrantyToolUI.h"
#include "WarrantyToolUIDialog.h"

#include "Icons/camera.xpm"

// --- wxWidgets Includes --- //
#include <wx/wx.h>

using namespace warrantytool;

IMPLEMENT_DYNAMIC_CLASS( WarrantyToolUI, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
WarrantyToolUI::WarrantyToolUI()
{
    mPluginName = wxT( "WarrantyTool" );

    wxImage my_img( camera_xpm );
    icon_w = static_cast< int >( my_img.GetWidth() * 0.5f );
    icon_h = static_cast< int >( my_img.GetHeight() * 0.5f );
    my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );

    n_pts = 4;

    poly[ 0 ] = wxPoint( 0, 0 );
    poly[ 1 ] = wxPoint( icon_w, 0 );
    poly[ 2 ] = wxPoint( icon_w, icon_h );
    poly[ 3 ] = wxPoint( 0, icon_h );
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolUI::~WarrantyToolUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double WarrantyToolUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int WarrantyToolUI::GetNumPoly()
{
    int result = 0;

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int WarrantyToolUI::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUI::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int WarrantyToolUI::GetNumOports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolUI::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* WarrantyToolUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new WarrantyToolUIDialog( parent, -1, serviceList );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString WarrantyToolUI::GetConductorName()
{         
    wxString result = wxT( "WarrantyTool" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString WarrantyToolUI::GetName()
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = wxT( "PleaseDefineClassName" );
    }

    return mPluginName;
}
////////////////////////////////////////////////////////////////////////////////
wxString WarrantyToolUI::GetDesc()
{
    wxString result = wxT( "The generic tool to display warranty data." );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
