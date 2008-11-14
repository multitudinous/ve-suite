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

#include "APPlugin.h"

#include <ves/conductor/xpm/AspenPlus2DIcons/aspen.xpm>
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

BEGIN_EVENT_TABLE( APPlugin, UIPluginBase )
    EVT_MENU( OPEN_SIM, APPlugin::OnOpen )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( APPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
APPlugin::APPlugin() :
    UIPluginBase()
{
    name = wxString( "AspenPlus", wxConvUTF8 );

    wxImage my_img( aspen );
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
APPlugin::~APPlugin()
{
}

/////////////////////////////////////////////////////////////////////////////
wxString APPlugin::GetConductorName()
{
    return name;
}

/////////////////////////////////////////////////////////////////////////////
void APPlugin::OnOpen( wxCommandEvent& event )
{
    wxString bkpext( "Aspen Plus ASCII files (*.bkp)|*.bkp", wxConvUTF8);
    wxString apwext( "Aspen Plus Binary files (*.apw)|*.apw", wxConvUTF8);
    wxString extText = bkpext + _("|") + apwext;
    wxFileDialog fd( m_canvas, wxT("Choose a file"), wxT(""), wxT(""), 
        extText, wxOPEN );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

   /* wxFileName bkpFileName;
    bkpFileName.ClearExt();
    bkpFileName.SetName( fd.GetFilename() );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "getNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "getNetwork" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",  ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );

    // If there is nothing on the CE
    if( nw_str.compare("BKPDNE") == 0 )
    {
        //Log( "BKP File Does NOT exist.\n" );
        return;
    }    
    else if( nw_str.compare("APWDNE") == 0 )
    {
        //Log( "APW File Does NOT exist.\n" );
        return;
    }

    Network * network = m_canvas->GetActiveNetwork();

    //if( network->modules.empty() )
    //{
        //network->Load( nw_str, true );
        m_canvas->PopulateNetworks( nw_str );

        //create hierarchy page
        //hierarchyTree->PopulateTree( 
        //    XMLDataBufferEngine::instance()->GetTopSystemId() );

        //Log( "Simulation Opened.\n" );
        ///
        CommandPtr aspenBKPFile( new Command() );
        aspenBKPFile->SetCommandName( "Aspen_Plus_Preferences" );
        data = DataValuePairPtr( new DataValuePair() );
        data->SetData( "BKPFileName",
                       ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
        aspenBKPFile->AddDataValuePair( data );
        UserPreferencesDataBuffer::instance()->
        SetCommand( "Aspen_Plus_Preferences", aspenBKPFile );
        ///Submit job to xplorer
        wxCommandEvent event;
        SubmitToServer( event );
		AspenSimOpen = true;
    //}
    //else
    //{
    //    Log( "Simulation is already open.\n" );
    //}*/
}