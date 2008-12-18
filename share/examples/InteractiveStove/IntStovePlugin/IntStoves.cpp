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
#include "IntStoves.h"
#include "IntStoves_UI_Dialog.h"

#include "plancha.xpm"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#include <wx/wx.h>

IMPLEMENT_DYNAMIC_CLASS( IntStoves, UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
IntStoves::IntStoves()
{
    mPluginName = wxT( "IntStoves" );

    numbaffles = 0;
    RegistVar( "numbaffles", &numbaffles );
    RegistVar( "baffle1", &baffle1 );
    RegistVar( "baffle2", &baffle2 );
    RegistVar( "baffle3", &baffle3 );
    RegistVar( "baffle4", &baffle4 );
    RegistVar( "baffle5", &baffle5 );
    RegistVar( "baffle6", &baffle6 );
    RegistVar( "baffle7", &baffle7 );

    wxImage my_img( plancha_xpm );
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
IntStoves::~IntStoves()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double IntStoves::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves::GetNumPoly()
{
    int result = 0;

    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves::GetNumIports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves::GetIPorts( POLY &iports )
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
int IntStoves::GetNumOports()
{
    int result = 0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void IntStoves::GetOPorts( POLY &oports )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* IntStoves::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new IntStoves_UI_Dialog( parent, -1,
                                   &numbaffles,
                                   &baffle1,
                                   &baffle2,
                                   &baffle3,
                                   &baffle4,
                                   &baffle5,
                                   &baffle6,
                                   &baffle7 );

    //dlg->CenterOnScreen( wxBOTH );

    ConfigurePluginDialogs( dlg );

    return dlg;
}

////////////////////////////////////////////////////////////////////////////////
wxString IntStoves::GetName()
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = wxT( "PleaseDefineClassName" );
    }

    return mPluginName;
}
////////////////////////////////////////////////////////////////////////////////
wxString IntStoves::GetDesc()
{
    wxString result = wxT( "None" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString IntStoves::GetConductorName()
{         
    wxString result( wxT( "StoveDemo_IntStoves" ) );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
