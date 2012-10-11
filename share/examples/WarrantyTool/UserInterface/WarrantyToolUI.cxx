/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

// --- wxWidgets Includes --- //
#include <wx/wx.h>

using namespace warrantytool;

IMPLEMENT_DYNAMIC_CLASS( WarrantyToolUI, ves::conductor::UIPluginBase )

////////////////////////////////////////////////////////////////////////////////
WarrantyToolUI::WarrantyToolUI()
{
    mPluginName = wxT( "DeereAnalytics" );

    wxImage my_img( _("Icons/TractorIcon.png") );
    iconFilename = "Icons/TractorIcon.png";
    SetImage( my_img );
    mDescription = wxT( "The generic tool to display product data." );
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
    wxString result = wxT( "DeereAnalytics" );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
wxString WarrantyToolUI::GetName()
{
    return mPluginName;
}
////////////////////////////////////////////////////////////////////////////////
