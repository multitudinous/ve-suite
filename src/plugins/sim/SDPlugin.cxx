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

#include "SDPlugin.h"
#include "SDOpenDialog.h"
#include "OPCDlg.h"

#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <ves/conductor/xpm/AspenPlus2DIcons/sim.xpm>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/DynamicsDataBuffer.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/Network.h>
#include <ves/conductor/Module.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/System.h>
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

BEGIN_EVENT_TABLE( SDPlugin, ves::conductor::UIPluginBase )
    EVT_MENU( SDPLUGIN_OPEN_SIM, SDPlugin::OnOpen )
    EVT_MENU( SDPLUGIN_SHOW_ASPEN_SIMULATION, SDPlugin::ShowAspenSimulation )
    EVT_MENU( SDPLUGIN_HIDE_ASPEN_SIMULATION, SDPlugin::HideAspenSimulation )
    EVT_MENU( SDPLUGIN_CLOSE_ASPEN_SIMULATION, SDPlugin::OnCloseAspenSimulation )
    EVT_MENU( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, SDPlugin::OnDisconnectAspenSimulation )
    EVT_MENU( SDPLUGIN_RUN_ASPEN_NETWORK, SDPlugin::RunAspenNetwork )
    EVT_MENU( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, SDPlugin::ReinitializeAspenSimulation )
    EVT_MENU( SDPLUGIN_STEP_ASPEN_NETWORK, SDPlugin::StepAspenNetwork )
    EVT_MENU( SDPLUGIN_SAVE_SIMULATION, SDPlugin::SaveSimulation )
    EVT_MENU( SDPLUGIN_SAVEAS_SIMULATION, SDPlugin::SaveAsSimulation )
    EVT_MENU( SDPLUGIN_CREATE_OPC_LIST, SDPlugin::OnCreateOPCList )
	EVT_MENU( SDPLUGIN_CONNECT, SDPlugin::OnConnect )
	EVT_TIMER( SDPLUGIN_TIMER_ID, SDPlugin::OnTimer )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( SDPlugin, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
SDPlugin::SDPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "Sim", wxConvUTF8 );
    mDescription = wxString( "Sim Plugin", wxConvUTF8 );
    GetVEModel()->SetPluginType( "SDPlugin" );
    GetVEModel()->SetVendorName( "ASPENUNIT" );

    iconFilename = "sim";
    wxImage my_img( sim );
    SetImage( my_img );

	m_timer = new wxTimer( this, SDPLUGIN_TIMER_ID );
}
////////////////////////////////////////////////////////////////////////////////
SDPlugin::~SDPlugin()
{
    /*if( IsBKPOpen() )
    {
        DisconnectAspenSimulation();
        CloseAspenSimulation();
    }*/
}

bool SDPlugin::IsBKPOpen()
{
    /*if( mUserPrefBuffer )
    {
        CommandPtr aspenBKPFile = mUserPrefBuffer->
            GetCommand( "Aspen_Plus_Preferences" );

        if( aspenBKPFile->GetCommandName() != "NULL" )
        {
            DataValuePairPtr bkpPtr =
            aspenBKPFile->GetDataValuePair( "BKPFileName" );
            if( bkpPtr )
            {
                return true;
            }
        }
    }
    return false;*/
    return true;
}
////////////////////////////////////////////////////////////////////////////////
wxString SDPlugin::GetConductorName()
{
    return wxString( "sim_SD", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
void SDPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //wxString bkpext( "Aspen Plus ASCII files (*.bkp)|*.bkp", wxConvUTF8);
    //wxString apwext( "Aspen Plus Binary files (*.apw)|*.apw", wxConvUTF8);
    //wxString extText = bkpext + _("|") + apwext;
    //wxFileDialog fd( m_canvas, wxT("Choose a file"), wxT(""), wxT(""), 
    //    extText, wxOPEN );

    /*if( IsBKPOpen() )
    {
        wxMessageDialog md( m_canvas, 
            wxT( "Simulation already open.\nClose it and open another?" ),
            wxT( "Confirm" ),
            wxYES_NO);
        if( md.ShowModal() == wxID_NO )
        {
            return;
        }
        else
        {
            DisconnectAspenSimulation();
            CloseAspenSimulation();
        }
    }
    */

    SDOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName xmlFileName;
    xmlFileName.ClearExt();
    xmlFileName.SetName( fd.GetFilename() + wxT(".xml") );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "getNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "getNetwork" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "XMLFileName",  ConvertUnicode( xmlFileName.GetFullName().c_str() ) );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );
    
    if( nw_str.empty() )
    {
        wxMessageDialog md( m_canvas, wxT("No Aspen Unit connected.\nPlease launch Aspen Unit."), wxT("Error"), wxOK);
        md.ShowModal();
        return;
    }

    // If there is nothing on the CE
    if( nw_str.compare("XMLDNE") == 0 )
    {
        wxMessageDialog md(m_canvas, wxT("Aspen Unit is unable to find the xml file.\nDid you select the correct directory in Aspen Unit?" ), wxT("Error"), wxOK );
        md.ShowModal();
        //Log( "XML File Does NOT exist.\n" );
        return;
    }    
    else if( nw_str.compare("simDNE") == 0 )
    {
        wxMessageDialog md( m_canvas, wxT("Aspen Unit is unable to find the sim file.\nDid you select the correct directory in Aspen Unit?" ), wxT("Error"), wxOK);
        md.ShowModal();
        //Log( "sim File Does NOT exist.\n" );
        return;
    }
    
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    networkWriter.ReadXMLData( nw_str, dataToObtain );
    std::vector< ves::open::xml::XMLObjectPtr >::iterator objectIter;
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();

    ves::open::xml::model::SystemPtr tempSystem;
    tempSystem = boost::dynamic_pointer_cast<ves::open::xml::model::System>( objectVector.at( 0 ) );
    ves::open::xml::model::ModelPtr aspenPlusModel;
    //set parent model on topmost level
    for( int modelCount = 0; modelCount < tempSystem->GetNumberOfModels(); modelCount++)
    {
        ModelPtr tempModel = tempSystem->GetModel( modelCount );
		tempModel->SetParentModel( aspenPlusModel );
		
		//go through models to find opc
		SystemPtr tempSubSystem = tempModel->GetSubSystem();
		for( int subModelCount = 0; 
			subModelCount < tempSubSystem->GetNumberOfModels(); 
			subModelCount++)
		{
			//add OPC plugins to a list
			std::string tempType = tempSubSystem->GetModel( subModelCount )->
				GetPluginType( );
			if(tempType.compare("OpcUOPlugin") == 0)
			{
				m_opcList.push_back(tempSubSystem->GetModel( subModelCount )->
					GetPluginName());
			}
		}
    }

    //aspenPlusModel->SetSubSystem( tempSystem );
    GetVEModel()->SetSubSystem( tempSystem );
    mDataBufferEngine->ParseSystem( tempSystem );

    //Network * network = m_canvas->GetActiveNetwork();

    //if( network->modules.empty() )
    //{
    //network->Load( nw_str, true );
    m_canvas->AddSubNetworks( );

	//m_subNetwork = m_canvas->GetNetwork( );

#if 0
    std::ofstream netdump ("netdump.txt");
    netdump << nw_str;
    netdump.close();
#endif
    
    event.SetId( UPDATE_HIER_TREE );
    ::wxPostEvent( m_canvas, event );

    //create hierarchy page
    //hierarchyTree->PopulateTree( 
    //    mDataBufferEngine->GetTopSystemId() );

    //Log( "Simulation Opened.\n" );
    ///
    CommandPtr aspenXMLFile( new Command() );
    aspenXMLFile->SetCommandName( "sim_Preferences" );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "XMLFileName",
                   ConvertUnicode( xmlFileName.GetFullName().c_str() ) );
    aspenXMLFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "sim_Preferences", aspenXMLFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    //mAspenMenu->Enable( SDPLUGIN_CLOSE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( SDPLUGIN_SHOW_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( SDPLUGIN_HIDE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( SDPLUGIN_RUN_ASPEN_NETWORK, true );
    mAspenMenu->Enable( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( SDPLUGIN_STEP_ASPEN_NETWORK, true );
    mAspenMenu->Enable( SDPLUGIN_SAVE_SIMULATION, true );
    mAspenMenu->Enable( SDPLUGIN_SAVEAS_SIMULATION, true );
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::ShowAspenSimulation( wxCommandEvent& event )
{
    /*UIPLUGIN_CHECKID( event )
    //Log( "Show Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "showSimulation" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "showSimulation" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";
    //Log( nw_str.c_str() );*/
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::HideAspenSimulation( wxCommandEvent& event )
{
    /*UIPLUGIN_CHECKID( event )
    //Log( "Hide Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "hideSimulation" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "hideSimulation" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";
    //Log( nw_str.c_str() );
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::DisconnectAspenSimulation( void )
{    
	/*//Log( "Close Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "closeSimulation" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "closeSimulation" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";
    //Log( nw_str.c_str() );
	//AspenSimOpen = false;
    */
}
////////////////////////////////////////////////////////////////////////////////////
void SDPlugin::CloseAspenSimulation( void )
{ 
    /*SetName( _("SDPlugin") );
    wxCommandEvent event;
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );
    if( mAspenMenu )
    {
        //mAspenMenu->Enable( SDPLUGIN_CLOSE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_SHOW_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_HIDE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_RUN_ASPEN_NETWORK, false );
        mAspenMenu->Enable( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_STEP_ASPEN_NETWORK, false );
        mAspenMenu->Enable( SDPLUGIN_SAVE_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_SAVEAS_SIMULATION, false );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::OnCloseAspenSimulation( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
    DisconnectAspenSimulation();
    CloseAspenSimulation();
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::OnDisconnectAspenSimulation( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
    DisconnectAspenSimulation();
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::RunAspenNetwork( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
    //Log( "Run Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "runNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "runNetwork" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
    */
}
///////////////////////////////////////////////////////////////////////////////
void SDPlugin::ReinitializeAspenSimulation( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
	//Log( "Reinitialize Simulation.\n" );
	CommandPtr returnState( new Command() );
    returnState->SetCommandName( "reinitNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "reinitNetwork" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::StepAspenNetwork( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
    //Log( "Run Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "stepNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "runNetwork" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::SaveSimulation( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
    //Log( "Saving Simulation...\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "saveSimulation" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "saveSimulation" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    serviceList->Query( status );
    //std::string nw_str = serviceList->Query( status ) + "\n";
    //Log(nw_str.c_str());
    //Log( "Simulation Saved.\n" );
    */
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::SaveAsSimulation( wxCommandEvent& event )
{
    /*
    UIPLUGIN_CHECKID( event )
    wxFileName saveFileName;
    wxTextEntryDialog newDataSetName( m_canvas,
                                      wxString( "Enter filename (.apw):", wxConvUTF8 ),
                                      wxString( "Save Flowsheet", wxConvUTF8 ),
                                      wxString( "", wxConvUTF8 ), wxOK | wxCANCEL );

    if( newDataSetName.ShowModal() != wxID_OK )
    {
        return;
    }

    //Log( "Saving Simulation...\n" );
    saveFileName.ClearExt();
    saveFileName.SetName( newDataSetName.GetValue() );
    //bkpFileName.SetExt( wxString( "bkp", wxConvUTF8 ) );

    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "saveAsSimulation" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "saveAsSimulation" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "SaveFileName",
                   ConvertUnicode( saveFileName.GetFullName().c_str() ) );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair < XMLObjectPtr,
                     std::string > ( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    //Get results
    std::string nw_str = serviceList->Query( status );
    //Log( "Simulation Saved.\n" );

    CommandPtr aspenAPWFile( new Command() );
    aspenAPWFile->SetCommandName( "Aspen_Plus_Preferences" );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",
                   ConvertUnicode( saveFileName.GetFullName().c_str() ) );
    aspenAPWFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "Aspen_Plus_Preferences", aspenAPWFile );
    */
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* SDPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mAspenMenu )
    {
        return baseMenu;
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    (baseMenu->FindItemByPosition( 0 ))->GetSubMenu()->Enable( 
        UIPLUGINBASE_MODEL_INPUTS, false );
    (baseMenu->FindItemByPosition( 0 ))->GetSubMenu()->Enable( 
        UIPLUGINBASE_MODEL_RESULTS, false );
    (baseMenu->FindItemByPosition( 0 ))->GetSubMenu()->Enable( 
        UIPLUGINBASE_SHOW_ICON_CHOOSER, false );

    mAspenMenu = new wxMenu();
    mAspenMenu->Append( SDPLUGIN_OPEN_SIM, _( "Open" ) );
    mAspenMenu->Enable( SDPLUGIN_OPEN_SIM, true );
    //mAspenMenu->Append( SDPLUGIN_CLOSE_ASPEN_SIMULATION, _( "Close" ) );
    mAspenMenu->Append( SDPLUGIN_CREATE_OPC_LIST, _( "Create List") );
    mAspenMenu->Enable( SDPLUGIN_CREATE_OPC_LIST, true );
    mAspenMenu->Append( SDPLUGIN_CONNECT, _( "Connect to OPC") );
    mAspenMenu->Enable( SDPLUGIN_CONNECT, true );
    mAspenMenu->Append( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, _( "Disconnect" ) );
    mAspenMenu->Append( SDPLUGIN_SHOW_ASPEN_SIMULATION, _( "Show" ) );
    mAspenMenu->Append( SDPLUGIN_HIDE_ASPEN_SIMULATION, _( "Hide" ) );
    mAspenMenu->Append( SDPLUGIN_RUN_ASPEN_NETWORK, _( "Run" ) );
	mAspenMenu->Append( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, _( "Reinitialize" ) );
    mAspenMenu->Append( SDPLUGIN_STEP_ASPEN_NETWORK, _( "Step" ) );
    mAspenMenu->Append( SDPLUGIN_SAVE_SIMULATION, _( "Save" ) );
    mAspenMenu->Append( SDPLUGIN_SAVEAS_SIMULATION, _( "SaveAs" ) );

    baseMenu->Insert( 0, SDPLUGIN_ASPEN_MENU,   _( "sim" ), mAspenMenu,
                    _( "Used in conjunction with sim" ) );
    baseMenu->Enable( SDPLUGIN_ASPEN_MENU, true );

    if( GetVEModel()->GetSubSystem() != NULL )
    {
        //mAspenMenu->Enable( SDPLUGIN_CLOSE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( SDPLUGIN_SHOW_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( SDPLUGIN_HIDE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( SDPLUGIN_RUN_ASPEN_NETWORK, true );
	    mAspenMenu->Enable( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( SDPLUGIN_STEP_ASPEN_NETWORK, true );
        mAspenMenu->Enable( SDPLUGIN_SAVE_SIMULATION, true );
        mAspenMenu->Enable( SDPLUGIN_SAVEAS_SIMULATION, true );
    }
    else
    {
        //mAspenMenu->Enable( SDPLUGIN_CLOSE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_DISCONNECT_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_SHOW_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_HIDE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_RUN_ASPEN_NETWORK, false );
	    mAspenMenu->Enable( SDPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_STEP_ASPEN_NETWORK, false );
        mAspenMenu->Enable( SDPLUGIN_SAVE_SIMULATION, false );
        mAspenMenu->Enable( SDPLUGIN_SAVEAS_SIMULATION, false );
    }
    return baseMenu;
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::OnCreateOPCList( wxCommandEvent& event )
{
	//create dialog with list of available opc variables
	OPCDlg * opcDlg = new OPCDlg( m_canvas );

	//populate the dialog with available variables and selected
	//opcDlg->PopulateLists( m_opcList, m_selectedOpcList );
	opcDlg->SetParentPlugin( this );

	//display the dialog
	opcDlg->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void SDPlugin::OnConnect( wxCommandEvent& event )
{
    std::string compName = GetVEModel()->GetPluginName();
	ves::open::xml::CommandPtr monitor( new ves::open::xml::Command() );
    monitor->SetCommandName("connectWithList");

    ves::open::xml::DataValuePairPtr
        variables( new ves::open::xml::DataValuePair() );
    variables->SetData( "variables", m_selectedOpcList );
    monitor->AddDataValuePair( variables );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr,
		std::string >( monitor, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

	std::string nw_str = serviceList->Query( status );

	m_timer->Start( 4000 );
}

std::vector< std::string > SDPlugin::GetAvailableVariables()
{
	return m_opcList;
}

std::vector< std::string > SDPlugin::GetSelectVariables()
{
	return m_selectedOpcList;
}

void SDPlugin::SetSelectVariables( std::vector< std::string> selectedVariables )
{
	m_selectedOpcList = selectedVariables;
}

//this is a temporary function
//this timer event is used to update the dynamicsdatabuffer
//this timer event would make more sense in the buffer but is a difficult task
//in the end the buffer will be filled in another fashion
void SDPlugin::OnTimer( wxTimerEvent& event )
{
	DynamicsDataBuffer::instance()->Update();
}