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

#include "APPlugin.h"

#include <ves/conductor/xpm/AspenPlus2DIcons/aspen.xpm>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/FindDialog.h>
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

#define edge_size 10

BEGIN_EVENT_TABLE( APPlugin, UIPluginBase )
    EVT_MENU( OPEN_SIM, APPlugin::OnOpen )
    EVT_MENU( SHOW_ASPEN_SIMULATION, APPlugin::ShowAspenSimulation )
    EVT_MENU( HIDE_ASPEN_SIMULATION, APPlugin::HideAspenSimulation )
    EVT_MENU( CLOSE_ASPEN_SIMULATION, APPlugin::OnCloseAspenSimulation )
    EVT_MENU( RUN_ASPEN_NETWORK, APPlugin::RunAspenNetwork )
    EVT_MENU( REINITIALIZE_ASPEN_SIMULATION, APPlugin::ReinitializeAspenSimulation )
    EVT_MENU( STEP_ASPEN_NETWORK, APPlugin::StepAspenNetwork )
    EVT_MENU( CONDUCTOR_FIND, APPlugin::FindBlocks )
    EVT_MENU( SAVE_SIMULATION, APPlugin::SaveSimulation )
    EVT_MENU( SAVEAS_SIMULATION, APPlugin::SaveAsSimulation )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( APPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
APPlugin::APPlugin() :
    UIPluginBase()
{
    iconFilename = "aspen";
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

    wxMenu * aspen_menu = new wxMenu();
    aspen_menu->Append( OPEN_SIM, _( "Open" ) );
    aspen_menu->Enable( OPEN_SIM, true );
    aspen_menu->Append( SHOW_ASPEN_SIMULATION, _( "Show Simulation" ) );
    aspen_menu->Enable( SHOW_ASPEN_SIMULATION, true );
    aspen_menu->Append( HIDE_ASPEN_SIMULATION, _( "Hide Simulation" ) );
    aspen_menu->Enable( HIDE_ASPEN_SIMULATION, true );
    aspen_menu->Append( CLOSE_ASPEN_SIMULATION, _( "Close Simulation" ) );
    aspen_menu->Enable( CLOSE_ASPEN_SIMULATION, true );
    aspen_menu->Append( RUN_ASPEN_NETWORK, _( "Run" ) );
    aspen_menu->Enable( RUN_ASPEN_NETWORK, true );
	aspen_menu->Append( REINITIALIZE_ASPEN_SIMULATION, _( "Reinitialize" ) );
	aspen_menu->Enable( REINITIALIZE_ASPEN_SIMULATION, true );
    aspen_menu->Append( STEP_ASPEN_NETWORK, _( "Step" ) );
    aspen_menu->Enable( STEP_ASPEN_NETWORK, true );
    aspen_menu->Append( CONDUCTOR_FIND, _( "Find" ) );
    aspen_menu->Enable( CONDUCTOR_FIND, true );
    aspen_menu->Append( SAVE_SIMULATION, _( "Save Simulation" ) );
    aspen_menu->Enable( SAVE_SIMULATION, true );
    aspen_menu->Append( SAVEAS_SIMULATION, _( "SaveAs Simulation" ) );
    aspen_menu->Enable( SAVEAS_SIMULATION, true );
    mPopMenu->Insert( 0, ASPEN_MENU,   _( "Aspen" ), aspen_menu,
                     _( "Used in conjunction with Aspen" ) );
    mPopMenu->Enable( ASPEN_MENU, true );
}

////////////////////////////////////////////////////////////////////////////////
APPlugin::~APPlugin()
{
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
    wxString bkpext( "Aspen Plus ASCII files (*.bkp)|*.bkp", wxConvUTF8);
    wxString apwext( "Aspen Plus Binary files (*.apw)|*.apw", wxConvUTF8);
    wxString extText = bkpext + _("|") + apwext;
    wxFileDialog fd( m_canvas, wxT("Choose a file"), wxT(""), wxT(""), 
        extText, wxOPEN );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName bkpFileName;
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
    GetVEModel()->SetSubSystem( tempSystem );
    XMLDataBufferEngine::instance()->ParseSystem( tempSystem );

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

    //Network * network = m_canvas->GetActiveNetwork();

    //if( network->modules.empty() )
    //{
    //network->Load( nw_str, true );
    m_canvas->AddSubNetworks( );
    std::ofstream netdump ("netdump.txt");
    netdump << nw_str;
    netdump.close();

    event.SetId( UPDATE_HIER_TREE );
    ::wxPostEvent( m_canvas, event );

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
    //wxCommandEvent event;
    //SubmitToServer( event );
    //AspenSimOpen = true;
    //}
    //else
    //{
    //    Log( "Simulation is already open.\n" );
    //}
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
void APPlugin::CloseAspenSimulation( void )
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
////////////////////////////////////////////////////////////////////////////////
void APPlugin::OnCloseAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    CloseAspenSimulation();
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
void APPlugin::FindBlocks( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    Network* network = m_canvas->GetActiveNetwork();

    FindDialog fd( m_canvas );

    std::vector< std::string > moduleNames;
    std::vector< unsigned int > moduleIDs;

    //Find for modules
    //alphabetize map
    std::map< std::string, unsigned int > alphaMap;
    for( std::map<int, Module>::iterator iter = network->modules.begin();
        iter != network->modules.end(); ++iter )
    {
        if( iter->second.GetPlugin()->GetNameFlag() )
        {
            alphaMap[ConvertUnicode( iter->second.GetPlugin()->GetName().c_str() )]
                = iter->second.GetPlugin()->GetVEModel()->GetModelID();
        }
    }

    for( std::map< std::string, unsigned int >::iterator
            iter = alphaMap.begin(); iter != alphaMap.end(); ++iter )
    {
        moduleNames.push_back( iter->first );
        moduleIDs.push_back( iter->second );
    }

    std::vector< std::string > streamNames;
    std::vector< int > streamIDs;

    //Find for streams
    //alphabetize map
    //std::map< std::string, std::string > alphaMapStreams;
    std::map< std::string, int > alphaMapStreams;
    for( int i = 0; i < network->links.size(); i++ )
    {
        //alphaMapStreams[ConvertUnicode( network->links[i].GetName().c_str() )]
        //    = network->links[i].GetUUID();
        alphaMapStreams[ConvertUnicode( network->links[i].GetName().c_str() )]
            = i;
    }

    for( std::map< std::string, int >::iterator
            iter = alphaMapStreams.begin(); iter != alphaMapStreams.end(); ++iter )
    {
        streamNames.push_back( iter->first );
        streamIDs.push_back( iter->second );
    }

    fd.SetStreamList( streamNames );
    fd.SetModuleList( moduleNames );
    fd.ShowModal();

    std::pair< int, int > selectedModulePos = fd.GetSelectedModulePos();

    //highlight and center block
    if( selectedModulePos.first != wxNOT_FOUND ||
        selectedModulePos.second != wxNOT_FOUND )
    {
        std::string selectModuleName = "Find Failed!";
        if( selectedModulePos.first == 0)
        {
            network->
                HighlightCenter( moduleIDs[selectedModulePos.second] );
            selectModuleName = "\nFind Block: " +
                std::string( fd.GetSelectedModule() ) + "\n";
        }
        else
        {
            network->
                HighlightCenterLink( streamIDs[selectedModulePos.second] );
            selectModuleName = "\nFind Link: " +
                std::string( fd.GetSelectedModule() ) + "\n";
        }
        //Log( selectModuleName.c_str() );
    }
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
    UserPreferencesDataBuffer::instance()->
    SetCommand( "Aspen_Plus_Preferences", aspenAPWFile );
}