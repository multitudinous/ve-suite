/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include "OpcUOPlugin.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>
#include "OpcUOVarDialog.h"
#include "DynamicDataDlg.h"

#include <ves/conductor/xpm/square.xpm>

#include <ves/open/xml/model/Model.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( OpcUOPlugin, UIPluginBase )
    EVT_MENU( OPCUOPLUGIN_SHOW_VALUE, OpcUOPlugin::OnShowValue )
	EVT_TIMER( TIMER_ID, OpcUOPlugin::OnTimer )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( OpcUOPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
OpcUOPlugin::OpcUOPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "OpcUO", wxConvUTF8 );
    mDescription = wxString( "OPC Unit Operation Plugin", wxConvUTF8 );
    GetVEModel()->SetPluginType( "OpcUOPlugin" );
	m_timer = new wxTimer(this, TIMER_ID);
	m_timer->Start(4000);
	dynValue = "ERROR";
}
////////////////////////////////////////////////////////////////////////////////
OpcUOPlugin::~OpcUOPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
wxString OpcUOPlugin::GetConductorName()
{
    return wxString( "Opc_OpcUnitOp", wxConvUTF8 );
}
////////////////////////////////////////////////////////////////////////////////
void  OpcUOPlugin::OnShowValue( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
		
    /*std::string compName = GetVEModel()->GetPluginName();
    //compName = "Data.Blocks." + compName;

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOPCValue" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
	//hardcode the "D1_"  This will need to be parsed from the .tree file
    data->SetData( std::string( "ModuleName" ), "D1_"+compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status );

    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );	

    //wxString title;
    //title << wxT( "OPC Value" );
    //wxString desc( pair->GetDataString() );
    //wxMessageDialog( m_canvas, desc, title ).ShowModal();

	//create updating dialog
	DynamicDataDlg * dlg = new DynamicDataDlg( m_canvas );
	dlg->SetCORBAServiceList ( serviceList );
	dlg->SetName( compName );
	dlg->ReadValue( );
    dlg->ShowModal( );*/
}
////////////////////////////////////////////////////////////////////////////////
//void  OpcUOPlugin::OnShowIconChooser( wxCommandEvent& event )
//{
//    UIPLUGIN_CHECKID( event )
//    serviceList->GetMessageLog()->SetMessage( "Icon Chooser\n" );
/*    OpcUOPlugin* tempPlugin = this;
//    if( m_iconChooser == NULL )
    {
        m_iconChooser = new IconChooser( m_canvas );
    }
    m_iconChooser->AddIconsDir( wxString( "2dicons", wxConvUTF8 ) );
    //m_iconChooser->SetPlugin( tempPlugin );
    m_iconChooser->SetPlugin( this );
    //chooser->SetSize( dialogSize );
    m_iconChooser->Show();
	*/
//    event.SetClientData( this );
//    ::wxPostEvent( m_canvas->GetParent(), event );
//}

////////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::OnQueryDynamics( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetPluginName();
    //compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = GetVEModel();

    if( parentTraverser != NULL )
    {
        while( parentTraverser->GetParentModel() != NULL )
        {
            //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
            parentTraverser = parentTraverser->GetParentModel();
           // std::string tempFormat = "Blocks(\"" + compName + "\")";
            compName = parentTraverser->GetPluginName() + "." + compName;

        }
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    //std::ofstream packet("packet.txt");
    //packet<<nw_str;
    //packet.close();
    wxString title( compName.c_str(), wxConvUTF8 );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
    OpcUOVarDialog* params = new OpcUOVarDialog( GetPluginParent() );
    params->SetComponentName( wxString( compName.c_str(), wxConvUTF8 ) );
    params->SetServiceList( serviceList );
    int numdvps = cmd->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numdvps; i++ )
    {
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
        std::vector< std::string > temp_vector;
        pair->GetData( temp_vector );
        params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ), wxString( temp_vector[1].c_str(), wxConvUTF8 ),
            wxString( temp_vector[2].c_str(), wxConvUTF8 ), wxString( temp_vector[3].c_str(), wxConvUTF8 ) );
        //params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ) );
    }
    params->UpdateSizes();
    params->ShowModal();
    params->Destroy();
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* OpcUOPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mAspenMenu )
    {
        return baseMenu;
    }
    
    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, false );

    mAspenMenu = new wxMenu();
    mAspenMenu->Append( OPCUOPLUGIN_SHOW_VALUE, _( "Value" ) );
    mAspenMenu->Enable( OPCUOPLUGIN_SHOW_VALUE, true );
    //mAspenMenu->Append( OpcUOPlugin_QUERY_DYNAMICS, _( "All Variables" ) );
    //mAspenMenu->Enable( OpcUOPlugin_QUERY_DYNAMICS, true );
    baseMenu->Insert( 0, OPCUOPLUGIN_SIM_MENU,   _( "Aspen" ), mAspenMenu,
                    _( "Used in conjunction with Aspen" ) );
    baseMenu->Enable( OPCUOPLUGIN_SIM_MENU, true );
    return baseMenu;
}

void OpcUOPlugin::DrawValue( wxDC* dc )
{
    int x = 0;
    int y = 0;
    int w, h;

    wxCoord xoff = pos.x;
    wxCoord yoff = pos.y;

    for( int i = 0; i < n_pts; ++i )
    {
        x += poly[ i ].x;
        y += poly[ i ].y;
    }

    x = x / n_pts;
    y = y / n_pts;

    dc->GetTextExtent( dynValue.c_str(), &w, &h );
    dc->DrawText( dynValue.c_str(), int( x - w / 2 + xoff ), pos.y + int( y * 2.3 ) );
}

void OpcUOPlugin::ReadValue( )
{
    std::string compName = GetVEModel()->GetPluginName();
    //compName = "Data.Blocks." + compName;

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOPCValue" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
	//hardcode the "D1_"  This will need to be parsed from the .tree file
    data->SetData( std::string( "ModuleName" ), "D1_"+compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status );

    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );	

    dynValue = pair->GetDataString();
}


void OpcUOPlugin::OnTimer( wxTimerEvent& event )
{
    //UIPLUGIN_CHECKID( event )
	ReadValue();
	//DrawValue( wxDC* dc );
    m_canvas->Refresh( true );
}

void OpcUOPlugin::DrawPlugin( wxDC* dc )
{
    //if hidden
    if(nameFlag)
    {
        DrawIcon( dc );
        DrawID( dc );
        DrawValue( dc );
    }

    //if highlighted
    if( highlightFlag )
    {
        if(nameFlag)
        {
            HighlightSelectedIcon( dc );
        }
        DrawPorts( true, dc );
    }
}