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

#include "DWPlugin.h"
#include "DWOpenDialog.h"
#include "DWVarDialog.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/xpm/AspenPlus2DIcons/dwsim.xpm>
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

BEGIN_EVENT_TABLE( DWPlugin, ves::conductor::UIPluginBase )
    EVT_MENU( DWPLUGIN_SET_UNIT, DWPlugin::OnUnitName )
    EVT_MENU( DWPLUGIN_OPEN_SIM, DWPlugin::OnOpen )
    EVT_MENU( DWPLUGIN_INPUTS, DWPlugin::GetInputs )
    EVT_MENU( DWPLUGIN_OUTPUTS, DWPlugin::GetOutputs )
    //EVT_MENU( DWPLUGIN_SET_INPUTS, DWPlugin::SetInputs )
    EVT_MENU( DWPLUGIN_RUN_NETWORK, DWPlugin::RunSimulation )
    EVT_MENU( DWPLUGIN_CLOSE_SIMULATION, DWPlugin::OnCloseSimulation )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( DWPlugin, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
DWPlugin::DWPlugin() :
    UIPluginBase(),
    mMenu( 0 )
{
    mPluginName = wxString( "DWSIM", wxConvUTF8 );
    mDescription = wxString( "DWSIM Plugin", wxConvUTF8 );
    m_pluginType = "DWPlugin";
    m_unitName = "VE-PSI";

    iconFilename = "dwsim";
    wxImage my_img( dwsim );
    SetImage( my_img );
    mIsSheetOpen = false;
}
////////////////////////////////////////////////////////////////////////////////
DWPlugin::~DWPlugin()
{
    if( mIsSheetOpen )
    {
        CloseSimulation();
    }
}
/////////////////////////////////////////////////////////////////////////////
wxString DWPlugin::GetConductorName()
{
    return wxString( "DWSIM", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
void DWPlugin::OnUnitName( wxCommandEvent& event )
{    
    wxTextEntryDialog newUnitName( 0,
                                 _( "Enter the name for your unit:" ),
                                 _( "Set Unit Name..." ),
                                 "VE-PSI", wxOK | wxCANCEL );
    //check for existing unit

    if( newUnitName.ShowModal() == wxID_OK )
    {
        SetUnitName( newUnitName.GetValue().c_str() );
    }
}
/////////////////////////////////////////////////////////////////////////////
void DWPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

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
            CloseSimulation();
        }
    }

    //set the unit name
    GetVEModel()->SetVendorName( m_unitName );
    mMenu->Enable( APPLUGIN_SET_UNIT, false );

    DWOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName dwFileName;
    dwFileName.ClearExt();
    dwFileName.SetName( fd.GetFilename() + wxT(".dwsim") );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "openSimulation" );
    returnState->AddDataValuePair( vendorData );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "openSimulation" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "DWFileName",  ConvertUnicode( dwFileName.GetFullName().c_str() ) );
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
        wxMessageDialog md( m_canvas, wxT("No VE-PSI connected.\nPlease launch VE-PSI."), wxT("Error"), wxOK);
        md.ShowModal();
        return;
    }

    // If there is nothing on the CE
    if( nw_str.compare("DWDNE") == 0 )
    {
        return;
    }    

    event.SetId( UPDATE_HIER_TREE );
    ::wxPostEvent( m_canvas, event );

    CommandPtr dwsimFile( new Command() );
    dwsimFile->SetCommandName( "DWSIM_Preferences" );
    dwsimFile->AddDataValuePair( vendorData );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "DWFileName",
        ConvertUnicode( dwFileName.GetFullName().c_str() ) );
    dwsimFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "DWSIM_Preferences", dwsimFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    mMenu->Enable( DWPLUGIN_SET_UNIT, false );
    mMenu->Enable( DWPLUGIN_OPEN_SIM, false );
    mMenu->Enable( DWPLUGIN_CLOSE_SIMULATION, true );
    mMenu->Enable( DWPLUGIN_RUN_NETWORK, true );
    mMenu->Enable( DWPLUGIN_INPUTS, true );
    mMenu->Enable( DWPLUGIN_OUTPUTS, true );
    //mMenu->Enable( DWPLUGIN_SET_INPUTS, true );

    mIsSheetOpen = true;
}

////////////////////////////////////////////////////////////////////////////////
void DWPlugin::CloseSimulation( void )
{    
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
    mIsSheetOpen = false;
    SetName( _("DWPlugin") );
    wxCommandEvent event;
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );

    mMenu->Enable( DWPLUGIN_CLOSE_SIMULATION, false );
    mMenu->Enable( DWPLUGIN_RUN_NETWORK, false );
    mMenu->Enable( DWPLUGIN_INPUTS, false );
    mMenu->Enable( DWPLUGIN_OUTPUTS, false );
    //mMenu->Enable( DWPLUGIN_SET_INPUTS, false );
}
////////////////////////////////////////////////////////////////////////////////
void DWPlugin::OnCloseSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    CloseSimulation();
}
////////////////////////////////////////////////////////////////////////////////
void DWPlugin::RunSimulation( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
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

////////////////////////////////////////////////////////////////////////////////
wxMenu* DWPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mMenu )
    {
        return baseMenu;
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, false );

    mMenu = new wxMenu();
    mMenu->Append( DWPLUGIN_SET_UNIT, _( "Unit Name" ) );
    mMenu->Enable( DWPLUGIN_SET_UNIT, true );
    mMenu->Append( DWPLUGIN_OPEN_SIM, _( "Open" ) );
    mMenu->Enable( DWPLUGIN_OPEN_SIM, true );
    mMenu->Append( DWPLUGIN_INPUTS, _( "Inputs" ) );
    mMenu->Enable( DWPLUGIN_INPUTS, false );
    //mMenu->Append( DWPLUGIN_SET_INPUTS, _( "Set Inputs" ) );
    //mMenu->Enable( DWPLUGIN_SET_INPUTS, false );
    mMenu->Append( DWPLUGIN_OUTPUTS, _( "Outputs" ) );
    mMenu->Enable( DWPLUGIN_OUTPUTS, false );
    mMenu->Append( DWPLUGIN_RUN_NETWORK, _( "Run" ) );
    mMenu->Enable( DWPLUGIN_RUN_NETWORK, false );
    mMenu->Append( DWPLUGIN_CLOSE_SIMULATION, _( "Close" ) );
    mMenu->Enable( DWPLUGIN_CLOSE_SIMULATION, false );
    baseMenu->Insert( 0, DWPLUGIN_MENU,   _( "DWSIM" ), mMenu,
                    _( "Used in conjunction with DWSIM" ) );
    baseMenu->Enable( DWPLUGIN_MENU, true );

    if( GetVEModel()->GetSubSystem() != NULL )
    {
        mMenu->Enable( DWPLUGIN_CLOSE_SIMULATION, true );
        mMenu->Enable( DWPLUGIN_SHOW_SIMULATION, true );
        mMenu->Enable( DWPLUGIN_HIDE_SIMULATION, true );
        mMenu->Enable( DWPLUGIN_RUN_NETWORK, true );
        mMenu->Enable( DWPLUGIN_REINITIALIZE_SIMULATION, true );
        mMenu->Enable( DWPLUGIN_STEP_NETWORK, true );
        mMenu->Enable( DWPLUGIN_SAVE_SIMULATION, true );
        mMenu->Enable( DWPLUGIN_SAVEAS_SIMULATION, true );
    }
    else
    {
        mMenu->Enable( DWPLUGIN_CLOSE_SIMULATION, false );
        mMenu->Enable( DWPLUGIN_SHOW_SIMULATION, false );
        mMenu->Enable( DWPLUGIN_HIDE_SIMULATION, false );
        mMenu->Enable( DWPLUGIN_RUN_NETWORK, false );
        mMenu->Enable( DWPLUGIN_REINITIALIZE_SIMULATION, false );
        mMenu->Enable( DWPLUGIN_STEP_NETWORK, false );
        mMenu->Enable( DWPLUGIN_SAVE_SIMULATION, false );
        mMenu->Enable( DWPLUGIN_SAVEAS_SIMULATION, false );
    }

    return baseMenu;
}
////////////////////////////////////////////////////////////////////////////////
void DWPlugin::GetInputs( wxCommandEvent& event )
{
    //read inputs.xml
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "readInputs" );
    returnState->AddDataValuePair( vendorData );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "readInputs" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";

    //create a dialog - can be editted
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );

    //loop over all pairs
    DWVarDialog* params = new DWVarDialog( GetPluginParent() );
    params->SetServiceList( serviceList );
    int numdvps = cmd->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numdvps; i++ )
    {
        std::string name;
        std::vector< std::string > in_prop;
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
        name = pair->GetDataName( );
        std::vector< std::string > temp_vector;
        pair->GetData( temp_vector );
        params->SetData( wxString( name.c_str(), wxConvUTF8 ), wxString( temp_vector[1].c_str(), wxConvUTF8 ),
            wxString( temp_vector[2].c_str(), wxConvUTF8 ), wxString( temp_vector[3].c_str(), wxConvUTF8 ) );
    }
    params->UpdateSizes();
    params->ShowModal();
    params->Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void DWPlugin::GetOutputs( wxCommandEvent& event )
{
    //read outputs.xml
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "readOutputs" );
    returnState->AddDataValuePair( vendorData );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "readOutputs" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";

    //create a dialog
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );

    //loop over all pairs
    DWVarDialog* params = new DWVarDialog( GetPluginParent() );
    int numdvps = cmd->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numdvps; i++ )
    {
        std::string name;
        std::vector< std::string > in_prop;
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
        name = pair->GetDataName( );
        std::vector< std::string > temp_vector;
        pair->GetData( temp_vector );
        params->SetData( wxString( name.c_str(), wxConvUTF8 ), wxString( temp_vector[1].c_str(), wxConvUTF8 ),
            wxString( temp_vector[2].c_str(), wxConvUTF8 ), wxString( temp_vector[3].c_str(), wxConvUTF8 ) );
    }
    params->UpdateSizes();
    params->ShowModal();
    params->Destroy();
}
////////////////////////////////////////////////////////////////////////////////
/*void DWPlugin::SetInputs( wxCommandEvent& event )
{
    //read inputs.xml
    CommandPtr returnState( new Command() );
    returnState->SetCommandName( "setInputs" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "setInputs" );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status ) + "\n";

    //create a dialog - can be editted
}*/
void DWPlugin::SetUnitName( std::string name )
{
    m_unitName = name;
}