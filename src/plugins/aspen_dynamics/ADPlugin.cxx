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
#include <ves/conductor/util/CORBAServiceList.h>

#include "ADPlugin.h"
#include "ADOpenDialog.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/xpm/AspenPlus2DIcons/dynamics.xpm>
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

#include <fstream>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ADPlugin, ves::conductor::UIPluginBase )
    EVT_MENU( ADPLUGIN_SET_UNIT, ADPlugin::OnUnitName )
    EVT_MENU( ADPLUGIN_OPEN_SIM, ADPlugin::OnOpen )
    EVT_MENU( ADPLUGIN_SHOW_ASPEN_SIMULATION, ADPlugin::ShowAspenSimulation )
    EVT_MENU( ADPLUGIN_HIDE_ASPEN_SIMULATION, ADPlugin::HideAspenSimulation )
    EVT_MENU( ADPLUGIN_CLOSE_ASPEN_SIMULATION, ADPlugin::OnCloseAspenSimulation )
    EVT_MENU( ADPLUGIN_RUN_ASPEN_NETWORK, ADPlugin::RunAspenNetwork )
    EVT_MENU( ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION, ADPlugin::ReinitializeAspenSimulation )
    EVT_MENU( ADPLUGIN_STEP_ASPEN_NETWORK, ADPlugin::StepAspenNetwork )
    EVT_MENU( ADPLUGIN_SAVE_SIMULATION, ADPlugin::SaveSimulation )
    EVT_MENU( ADPLUGIN_SAVEAS_SIMULATION, ADPlugin::SaveAsSimulation )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( ADPlugin, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
ADPlugin::ADPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "AspenDynamics", wxConvUTF8 );
    mDescription = wxString( "Aspen Dynamics Plugin", wxConvUTF8 );
    m_pluginType = "ADPlugin";
    m_unitName = "VE-PSI";

    iconFilename = "dynamics";
    wxImage my_img( dynamics );
    SetImage( my_img );
    mIsSheetOpen = false;
}
////////////////////////////////////////////////////////////////////////////////
ADPlugin::~ADPlugin()
{
    if( mIsSheetOpen )
    {
        CloseAspenSimulation();
    }
}
/////////////////////////////////////////////////////////////////////////////
wxString ADPlugin::GetConductorName()
{
    return wxString( "Aspen_Dynamics_AD", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
void ADPlugin::OnUnitName( wxCommandEvent& event )
{    
    UIPLUGIN_CHECKID( event )

    //query ce for the list of available units
    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "GetUnitList" );
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >
        ( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    std::string unitList = serviceList->Query( status );

    //parse return message
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( unitList, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );

    std::vector< std::string > data;
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    pair->GetData( data );
    
    //populate the array with the unit names
    wxArrayString choices;
    for( int i = 0; i < data.size(); i++ )
    {
        choices.Add(wxString(data[i].c_str(), wxConvUTF8 ));
    }

    //create the dialog and get the selection
    wxSingleChoiceDialog scd(0,wxT("Currently connected units:"),
        wxT("Select a unit"),choices);
    if( scd.ShowModal() == wxID_OK )
    {
        SetUnitName( ConvertUnicode( scd.GetStringSelection().c_str() ) );
    }
}
/////////////////////////////////////////////////////////////////////////////
void ADPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //wxString dynext( "Aspen Dynamics files (*.dynf)|*.dynf", wxConvUTF8);
    //wxString extText = dynext;
    //wxFileDialog fd( m_canvas, wxT("Choose a file"), wxT(""), wxT(""), 
    //    extText, wxOPEN );

    if( mIsSheetOpen )
    {
        wxMessageDialog md( m_canvas, 
            wxT( "Simulation already open.\nClose it and open another?" ),
            wxT( "Confirm" ),
            wxYES_NO);
        if( md.ShowModal() == wxCANCEL )
        {
            return;
        }
        else
        {
            CloseAspenSimulation();
        }
    }

    ADOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    //set the unit name
    GetVEModel()->SetVendorName( m_unitName );
    vendorData = DataValuePairPtr( new DataValuePair() );
    vendorData->SetData( "vendorUnit", m_unitName );

    wxFileName dynFileName;
    dynFileName.ClearExt();
    dynFileName.SetName( fd.GetFilename() + wxT(".dynf") );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "getNetwork" );
    returnState->AddDataValuePair( vendorData );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "getNetwork" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "DYNFileName",  ConvertUnicode( dynFileName.GetFullName().c_str() ) );
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
    if( nw_str.compare("DYNDNE") == 0 )
    {
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
    //set parent model on topmost level
    for( int modelCount = 0; modelCount < tempSystem->GetNumberOfModels(); modelCount++)
    {
        tempSystem->GetModel( modelCount )->SetParentModel( m_veModel );
    }

    //aspenPlusModel->SetSubSystem( tempSystem );
    m_veModel->SetSubSystem( tempSystem );
    mDataBufferEngine->ParseSystem( tempSystem );

    Network * network = m_canvas->GetActiveNetwork();

    //if( network->modules.empty() )
    //{
    //network->Load( nw_str, true );
    m_canvas->AddSubNetworks( );
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
    CommandPtr aspenDynFile( new Command() );
    aspenDynFile->SetCommandName( "Aspen_Dynamics_Preferences" );
    aspenDynFile->AddDataValuePair( vendorData );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "DYNFileName",
        ConvertUnicode( dynFileName.GetFullName().c_str() ) );
    aspenDynFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "Aspen_Dynamics_Preferences", aspenDynFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    mAspenMenu->Enable( ADPLUGIN_SET_UNIT, false );
    mAspenMenu->Enable( ADPLUGIN_OPEN_SIM, false );
    mAspenMenu->Enable( ADPLUGIN_CLOSE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( ADPLUGIN_SHOW_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( ADPLUGIN_HIDE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( ADPLUGIN_RUN_ASPEN_NETWORK, true );
    mAspenMenu->Enable( ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
    mAspenMenu->Enable( ADPLUGIN_STEP_ASPEN_NETWORK, true );
    mAspenMenu->Enable( ADPLUGIN_SAVE_SIMULATION, true );
    mAspenMenu->Enable( ADPLUGIN_SAVEAS_SIMULATION, true );

    mIsSheetOpen = true;

    ///Submit job to xplorer
    //wxCommandEvent event;
    //SubmitToServer( event );
    //AspenSimOpen = true;
    //}
    //else
    //{
    //    Log( "Simulation is already open.\n" );
    //}
}////////////////////////////////////////////////////////////////////////////////
void ADPlugin::ShowAspenSimulation( wxCommandEvent& event  )
{
    UIPLUGIN_CHECKID( event )
    //Log( "Show Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "showSimulation" );
    returnState->AddDataValuePair( vendorData );
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
void ADPlugin::HideAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Log( "Hide Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "hideSimulation" );
    returnState->AddDataValuePair( vendorData );
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
void ADPlugin::CloseAspenSimulation( void )
{    
    //Log( "Close Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "closeSimulation" );
    returnState->AddDataValuePair( vendorData );
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
    mIsSheetOpen = false;
    SetName( _("ADPlugin") );
    wxCommandEvent event;
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    mAspenMenu->Enable( ADPLUGIN_CLOSE_ASPEN_SIMULATION, false );
    mAspenMenu->Enable( ADPLUGIN_SHOW_ASPEN_SIMULATION, false );
    mAspenMenu->Enable( ADPLUGIN_HIDE_ASPEN_SIMULATION, false );
    mAspenMenu->Enable( ADPLUGIN_RUN_ASPEN_NETWORK, false );
    mAspenMenu->Enable( ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
    mAspenMenu->Enable( ADPLUGIN_STEP_ASPEN_NETWORK, false );
    mAspenMenu->Enable( ADPLUGIN_SAVE_SIMULATION, false );
    mAspenMenu->Enable( ADPLUGIN_SAVEAS_SIMULATION, false );
}
////////////////////////////////////////////////////////////////////////////////
void ADPlugin::OnCloseAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    CloseAspenSimulation();
}
////////////////////////////////////////////////////////////////////////////////
void ADPlugin::RunAspenNetwork( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Log( "Run Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "runNetwork" );
    returnState->AddDataValuePair( vendorData );
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
void ADPlugin::ReinitializeAspenSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Log( "Reinitialize Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "reinitNetwork" );
    returnState->AddDataValuePair( vendorData );
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
void ADPlugin::StepAspenNetwork( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Log( "Run Simulation.\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "stepNetwork" );
    returnState->AddDataValuePair( vendorData );
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
void ADPlugin::SaveSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Log( "Saving Simulation...\n" );
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "saveSimulation" );
    returnState->AddDataValuePair( vendorData );
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
void ADPlugin::SaveAsSimulation( wxCommandEvent& event )
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
    returnState->AddDataValuePair( vendorData );
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
    aspenAPWFile->AddDataValuePair( vendorData );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "BKPFileName",
                   ConvertUnicode( saveFileName.GetFullName().c_str() ) );
    aspenAPWFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "Aspen_Plus_Preferences", aspenAPWFile );
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* ADPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mAspenMenu )
    {
        return baseMenu;
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );

    mAspenMenu = new wxMenu();
    mAspenMenu->Append( ADPLUGIN_SET_UNIT, _( "Unit Name" ) );
    mAspenMenu->Enable( ADPLUGIN_SET_UNIT, true );
    mAspenMenu->Append( ADPLUGIN_OPEN_SIM, _( "Open" ) );
    mAspenMenu->Enable( ADPLUGIN_OPEN_SIM, true );
    mAspenMenu->Append( ADPLUGIN_CLOSE_ASPEN_SIMULATION, _( "Close" ) );
    mAspenMenu->Append( ADPLUGIN_SHOW_ASPEN_SIMULATION, _( "Show" ) );
    mAspenMenu->Append( ADPLUGIN_HIDE_ASPEN_SIMULATION, _( "Hide" ) );
    mAspenMenu->Append( ADPLUGIN_RUN_ASPEN_NETWORK, _( "Run" ) );
    mAspenMenu->Append( ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION, _( "Reinitialize" ) );
    mAspenMenu->Append( ADPLUGIN_STEP_ASPEN_NETWORK, _( "Step" ) );
    mAspenMenu->Append( ADPLUGIN_SAVE_SIMULATION, _( "Save" ) );
    mAspenMenu->Append( ADPLUGIN_SAVEAS_SIMULATION, _( "SaveAs" ) );
    baseMenu->Insert( 0, ADPLUGIN_ASPEN_MENU,   _( "Aspen" ), mAspenMenu,
                    _( "Used in conjunction with Aspen" ) );
    baseMenu->Enable( ADPLUGIN_ASPEN_MENU, true );

    if( GetVEModel()->GetSubSystem() != NULL )
    {
        mAspenMenu->Enable( ADPLUGIN_CLOSE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( ADPLUGIN_SHOW_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( ADPLUGIN_HIDE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( ADPLUGIN_RUN_ASPEN_NETWORK, true );
        mAspenMenu->Enable( ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION, true );
        mAspenMenu->Enable( ADPLUGIN_STEP_ASPEN_NETWORK, true );
        mAspenMenu->Enable( ADPLUGIN_SAVE_SIMULATION, true );
        mAspenMenu->Enable( ADPLUGIN_SAVEAS_SIMULATION, true );
    }
    else
    {
        mAspenMenu->Enable( ADPLUGIN_CLOSE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( ADPLUGIN_SHOW_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( ADPLUGIN_HIDE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( ADPLUGIN_RUN_ASPEN_NETWORK, false );
        mAspenMenu->Enable( ADPLUGIN_REINITIALIZE_ASPEN_SIMULATION, false );
        mAspenMenu->Enable( ADPLUGIN_STEP_ASPEN_NETWORK, false );
        mAspenMenu->Enable( ADPLUGIN_SAVE_SIMULATION, false );
        mAspenMenu->Enable( ADPLUGIN_SAVEAS_SIMULATION, false );
    }

    return baseMenu;
}

void ADPlugin::SetUnitName( std::string name )
{
    m_unitName = name;
}
