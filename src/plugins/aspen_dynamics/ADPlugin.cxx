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
#include <ves/conductor/util/CORBAServiceList.h>
//#include <apps/conductor/Network.h>

#include "ADPlugin.h"

#include <ves/conductor/xpm/AspenPlus2DIcons/dynamics.xpm>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/image.h>
#include <wx/scrolwin.h>
#include <wx/window.h>
#include <wx/filedlg.h>
#include <wx/filename.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

#define edge_size 10

BEGIN_EVENT_TABLE( ADPlugin, UIPluginBase )
    EVT_MENU( OPEN_SIM, ADPlugin::OnOpen )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( ADPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
ADPlugin::ADPlugin() :
    UIPluginBase()
{
    name = wxString( "AspenDynamics", wxConvUTF8 );

    wxImage my_img( dynamics );
    icon_w = static_cast< int >( my_img.GetWidth() );//*0.30f );
    icon_h = static_cast< int >( my_img.GetHeight() );//*0.30f );
    //my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));
    my_icon = new wxBitmap( my_img );

    n_pts = 4;
    poly = new wxPoint[n_pts];
    poly[0] = wxPoint( 0, 0 );
    poly[1] = wxPoint( icon_w - 1, 0 );
    poly[2] = wxPoint( icon_w - 1, icon_h - 1 );
    poly[3] = wxPoint( 0, icon_h - 1 );

    mPopMenu->Append( OPEN_SIM, _( "Open" ) );
    mPopMenu->Enable( OPEN_SIM, true );
}
////////////////////////////////////////////////////////////////////////////////
ADPlugin::~ADPlugin()
{
    ;
}
/////////////////////////////////////////////////////////////////////////////
wxString ADPlugin::GetConductorName()
{
    return wxString( "Aspen_Dynamics_AD", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
void ADPlugin::OnOpen( wxCommandEvent& event )
{
    wxString dynext( "Aspen Dynamics files (*.dynf)|*.dynf", wxConvUTF8);
    wxString extText = dynext;
    wxFileDialog fd( m_canvas, wxT("Choose a file"), wxT(""), wxT(""), 
        extText, wxOPEN );
    fd.ShowModal();
}