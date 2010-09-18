/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include "CameraPlacementToolUI.h"
#include "CameraPlacementToolUIDialog.h"

#include "Icons/camera.xpm"

// --- wxWidgets Includes --- //
#include <wx/wx.h>

using namespace cpt;

IMPLEMENT_DYNAMIC_CLASS( CameraPlacementToolUI, ves::conductor::UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUI::CameraPlacementToolUI()
{
    mPluginName = wxT( "CameraPlacementTool" );

    wxImage image( camera_xpm );

    float scale = 0.50;
    int iconW = static_cast< int >( ( image.GetWidth() - 1 ) * scale );
    int iconH = static_cast< int >( ( image.GetHeight() - 1 ) * scale );

    SetImage( image.Rescale( iconW, iconH, wxIMAGE_QUALITY_HIGH ) );
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUI::~CameraPlacementToolUI()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double CameraPlacementToolUI::GetVersion()
{
    double result = 1.0;

    return result;
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* CameraPlacementToolUI::UI( wxWindow* parent )
{
    if( dlg != NULL )
    {
        return dlg;
    }

    dlg = new cpt::CameraPlacementToolUIDialog( parent, -1, serviceList );
    ConfigurePluginDialogs( dlg );

    return dlg;
}
////////////////////////////////////////////////////////////////////////////////
wxString CameraPlacementToolUI::GetConductorName()
{         
    wxString result = wxT( "CameraPlacementTool" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString CameraPlacementToolUI::GetName()
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = wxT( "PleaseDefineClassName" );
    }

    return mPluginName;
}
////////////////////////////////////////////////////////////////////////////////
