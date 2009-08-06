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

#include "APPlugin.h"
#include "APOpenDialog.h"

#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <ves/conductor/xpm/AspenPlus2DIcons/aspen.xpm>
#include <ves/conductor/UserPreferencesDataBuffer.h>
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

BEGIN_EVENT_TABLE( APPlugin, ves::conductor::UIPluginBase )
    EVT_MENU( APPLUGIN_OPEN_SIM, APPlugin::OnOpen )
    EVT_MENU( APPLUGIN_SHOW_ASPEN_SIMULATION, APPlugin::ShowAspenSimulation )
    EVT_MENU( APPLUGIN_HIDE_ASPEN_SIMULATION, APPlugin::HideAspenSimulation )
    EVT_MENU( APPLUGIN_CLOSE_ASPEN_SIMULATION, APPlugin::OnCloseAspenSimulation )
    EVT_MENU( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, APPlugin::OnDisconnectAspenSimulation )
    EVT_MENU( APPLUGIN_RUN_ASPEN_NETWORK, APPlugin::RunAspenNetwork )
    EVT_MENU( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, APPlugin::ReinitializeAspenSimulation )
    EVT_MENU( APPLUGIN_STEP_ASPEN_NETWORK, APPlugin::StepAspenNetwork )
    EVT_MENU( APPLUGIN_SAVE_SIMULATION, APPlugin::SaveSimulation )
    EVT_MENU( APPLUGIN_SAVEAS_SIMULATION, APPlugin::SaveAsSimulation )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( APPlugin, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
APPlugin::APPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "AspenPlus", wxConvUTF8 );
    mDescription = wxString( "Aspen Plus Plugin", wxConvUTF8 );
    GetVEModel()->SetPluginType( "APPlugin" );
    GetVEModel()->SetVendorName( "ASPENUNIT" );

    iconFilename = "aspen";
    wxImage my_img( aspen );
    SetImage( my_img );
}
////////////////////////////////////////////////////////////////////////////////
APPlugin::~APPlugin()
{
    if( IsBKPOpen() )
    {
        DisconnectAspenSimulation();
        CloseAspenSimulation();
    }
}

bool APPlugin::IsBKPOpen()
{
    if( mUserPrefBuffer )
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
    return false;
}
////////////////////////////////////////////////////////////////////////////////
wxString APPlugin::GetConductorName()
{
    return wxString( "Aspen_Plus_AP", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
void APPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

    if( IsBKPOpen() )
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

    APOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName bkpFileName;
    bkpFileName.ClearExt();
    bkpFileName.SetName( fd.GetFilename() + wxT(".bkp") );

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
    
    if( nw_str.empty() )
    {
        wxMessageDialog md( m_canvas, wxT("No Aspen Unit connected.\nPlease launch Aspen Unit."), wxT("Error"), wxOK);
        md.ShowModal();
        return;
    }

    // If there is nothing on the CE
    if( nw_str.compare("BKPDNE") == 0 )
    {
        wxMessageDialog md(m_canvas, wxT("Aspen Unit is unable to find the bkp file.\nDid you select the correct directory in Aspen Unit?" ), wxT("Error"), wxOK );
        md.ShowModal();
        //Log( "BKP File Does NOT exist.\n" );
        return;
    }    
    else if( nw_str.compare("APWDNE") == 0 )
    {
        wxMessageDialog md( m_canvas, wxT("Aspen Unit is unable to find the apw file.\nDid you select the correct directory in Aspen Unit?" ), wxT("Error"), wxOK);
        md.ShowModal();
        //Log( "APW File Does NOT exist.\n" );
        return;
    }
    
    //Parse the network string thst was returned from the VE-PSI Unit
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    std::vector< std::pair< std::string, std::string > > dataToObtain;
    std::vector< std::pair< std::string, std::string > >::iterator dataIter;
    dataToObtain.push_back( std::make_pair( "Model", "veSystem" ) );
    networkWriter.ReadXMLData( nw_str, dataToObtain );
    //Now get the veopen classes from the network string
    std::vector< ves::open::xml::XMLObjectPtr >::iterator objectIter;
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();

    //Now we need to make this plugin the top level plugin becuase
    //the aspen flowsheet is actually a subnetwork of this 
    //main aspen plus plugin
    ves::open::xml::model::SystemPtr tempSystem;
    tempSystem = boost::dynamic_pointer_cast<ves::open::xml::model::System>( objectVector.at( 0 ) );
    ves::open::xml::model::ModelPtr aspenPlusModel;
    //set a null pointer as the top most parent model on topmost level
    for( size_t modelCount = 0; 
        modelCount < tempSystem->GetNumberOfModels(); 
        ++modelCount )
    {
        //Not sure why we set a null pointer here...
        tempSystem->GetModel( modelCount )->SetParentModel( aspenPlusModel );
    }
    //Now we get this plugins veopen model and set its subsystem as the
    //flowsheet we just queried from VE-PSI
    GetVEModel()->SetSubSystem( tempSystem );
    mDataBufferEngine->ParseSystem( tempSystem );

    //Now let the rest of VE-Conductor know about the new network
    m_canvas->AddSubNetworks();
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
    CommandPtr aspenBKPFile( new Command() );
    aspenBKPFile->SetCommandName( "Aspen_Plus_Preferences" );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",
                   ConvertUnicode( bkpFileName.GetFullName().c_str() ) );
    aspenBKPFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "Aspen_Plus_Preferences", aspenBKPFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    //mAspenMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, true );
    mAspenMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, true );
    mAspenMenu->Enable( APPLUGIN_SAVE_SIMULATION, true );
    mAspenMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, true );
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::ShowAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
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
    //Log( nw_str.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::HideAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
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
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::DisconnectAspenSimulation( void )
{    
	//Log( "Close Simulation.\n" );
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
}
////////////////////////////////////////////////////////////////////////////////////
void APPlugin::CloseAspenSimulation( void )
{ 
    SetName( _("APPlugin") );
    wxCommandEvent event;
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );
    if( mAspenMenu )
    {
        //mAspenMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, false );
        mAspenMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, false );
        mAspenMenu->Enable( APPLUGIN_SAVE_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, false );
    }
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::OnCloseAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    DisconnectAspenSimulation();
    CloseAspenSimulation();
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::OnDisconnectAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    DisconnectAspenSimulation();
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::RunAspenNetwork( wxCommandEvent& event )
{
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
}
///////////////////////////////////////////////////////////////////////////////
void APPlugin::ReinitializeAspenSimulation( wxCommandEvent& event )
{
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
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::StepAspenNetwork( wxCommandEvent& event )
{
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
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::SaveSimulation( wxCommandEvent& event )
{
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
}
////////////////////////////////////////////////////////////////////////////////
void APPlugin::SaveAsSimulation( wxCommandEvent& event )
{
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
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* APPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
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
    mAspenMenu->Append( APPLUGIN_OPEN_SIM, _( "Open" ) );
        mAspenMenu->Enable( APPLUGIN_OPEN_SIM, true );
    //mAspenMenu->Append( APPLUGIN_CLOSE_ASPEN_SIMULATION, _( "Close" ) );
    mAspenMenu->Append( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, _( "Disconnect" ) );
    mAspenMenu->Append( APPLUGIN_SHOW_ASPEN_SIMULATION, _( "Show" ) );
    mAspenMenu->Append( APPLUGIN_HIDE_ASPEN_SIMULATION, _( "Hide" ) );
    mAspenMenu->Append( APPLUGIN_RUN_ASPEN_NETWORK, _( "Run" ) );
	mAspenMenu->Append( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, _( "Reinitialize" ) );
    mAspenMenu->Append( APPLUGIN_STEP_ASPEN_NETWORK, _( "Step" ) );
    mAspenMenu->Append( APPLUGIN_SAVE_SIMULATION, _( "Save" ) );
    mAspenMenu->Append( APPLUGIN_SAVEAS_SIMULATION, _( "SaveAs" ) );

    baseMenu->Insert( 0, APPLUGIN_ASPEN_MENU,   _( "Aspen" ), mAspenMenu,
                    _( "Used in conjunction with Aspen" ) );
    baseMenu->Enable( APPLUGIN_ASPEN_MENU, true );

    if( GetVEModel()->GetSubSystem() != NULL )
    {
        //mAspenMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, true );
	    mAspenMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, true );
        mAspenMenu->Enable( APPLUGIN_SAVE_SIMULATION, true );
        mAspenMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, true );
    }
    else
    {
        //mAspenMenu->Enable( APPLUGIN_CLOSE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_DISCONNECT_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_SHOW_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_HIDE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_RUN_ASPEN_NETWORK, false );
	    mAspenMenu->Enable( APPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_STEP_ASPEN_NETWORK, false );
        mAspenMenu->Enable( APPLUGIN_SAVE_SIMULATION, false );
        mAspenMenu->Enable( APPLUGIN_SAVEAS_SIMULATION, false );
    }
    return baseMenu;
}
