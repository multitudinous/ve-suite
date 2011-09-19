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

#include "OpcPlugin.h"
#include <ves/conductor/util/OpcUOVarDialog.h>
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/DynamicsDataBuffer.h>

#include <ves/conductor/xpm/AspenPlus2DIcons/opc.xpm>

#include <ves/open/xml/model/Model.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( OpcPlugin, UIPluginBase )
    EVT_MENU( OPCPLUGIN_SET_UNIT, OpcPlugin::OnUnitName )
    EVT_MENU( OPCPLUGIN_ALL_VAR, OpcPlugin::QueryForAllVariables )
    EVT_BUTTON( OPC_VAR_ID_MONITORBUTTON, OpcPlugin::OnMonitorVariable )
    EVT_MENU( OPCPLUGIN_CONNECT, OpcPlugin::OnConnect )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( OpcPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
OpcPlugin::OpcPlugin() :
    UIPluginBase(),
    mOpcMenu( 0 )
{
    mPluginName = wxString( "Opc", wxConvUTF8 );
    mDescription = wxString( "OPC Unit Operation Plugin", wxConvUTF8 );
    m_pluginType = "OpcPlugin";
    iconFilename = "opc";
    wxImage my_img( opc );
    SetImage( my_img );
    m_monValue = "NA";
    m_monValueExists = false;
    m_monitoring = false;

    m_unitName = "VE-PSI";
    m_veModel->SetVendorName( m_unitName );
    vendorData = DataValuePairPtr( new DataValuePair() );
    vendorData->SetData( "vendorUnit", m_unitName );
}
////////////////////////////////////////////////////////////////////////////////
OpcPlugin::~OpcPlugin()
{
}

////////////////////////////////////////////////////////////////////////////////
wxString OpcPlugin::GetConductorName()
{
    return wxString( "OPC", wxConvUTF8 );
}

/////////////////////////////////////////////////////////////////////////////
void OpcPlugin::OnUnitName( wxCommandEvent& event )
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

////////////////////////////////////////////////////////////////////////////////
wxMenu* OpcPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mOpcMenu )
    {
        return baseMenu;
    }
    
    //set the vendor name of the current plugin to the parents
    //if( GetVEModel()->GetParentModel() )
    //{
        //m_unitName = m_veModel->GetParentModel()->GetVendorName();
        //m_unitName = "VE-PSI";
        //m_veModel->SetVendorName( m_unitName );
        //vendorData = DataValuePairPtr( new DataValuePair() );
        //vendorData->SetData( "vendorUnit", m_unitName );
    //}

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    mOpcMenu = new wxMenu();
    mOpcMenu->Append( OPCPLUGIN_SET_UNIT, _( "Unit Name" ) );
    mOpcMenu->Enable( OPCPLUGIN_SET_UNIT, true );
    mOpcMenu->Append( OPCPLUGIN_CONNECT, _( "Connect to OPC") );
    mOpcMenu->Enable( OPCPLUGIN_CONNECT, true );
    mOpcMenu->Append( OPCPLUGIN_ALL_VAR, _( "ALL VAR" ) );
    mOpcMenu->Enable( OPCPLUGIN_ALL_VAR, true );

    //using the icon file name to determine if the opc plugin
    //represents a switch, valve or tank
    //create options for the CAD dialogs
    /*if( iconFilename.find("BREAKER") != std::string::npos )
    {
        mOpcMenu->Append( OPCPLUGIN_SWITCH_CAD, _( "CAD" ) );
        mOpcMenu->Enable( OPCPLUGIN_SWITCH_CAD, true );
    }
    else if (iconFilename.find("valve") != std::string::npos)
    {
        mOpcMenu->Append( OPCPLUGIN_VALVE_CAD, _( "CAD" ) );
        mOpcMenu->Enable( OPCPLUGIN_VALVE_CAD, true );
    }
    else if (iconFilename.find("tank") != std::string::npos)
    {
        mOpcMenu->Append( OPCPLUGIN_TANK_CAD, _( "CAD" ) );
        mOpcMenu->Enable( OPCPLUGIN_TANK_CAD, true );
    }*/
    baseMenu->Insert( 0, OPCPLUGIN_MENU,   _( "OPC" ), mOpcMenu,
                    _( "Used in conjunction with OPC" ) );
    baseMenu->Enable( OPCPLUGIN_MENU, true );
    return baseMenu;
}

void OpcPlugin::DrawValue( wxDC* dc )
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

    dc->GetTextExtent( wxString( m_monValue.c_str(), wxConvUTF8), &w, &h );
    dc->DrawText( wxString( m_monValue.c_str(), wxConvUTF8 ), int( x - w / 2 + xoff ), pos.y + int( y * 4.0 ) );
}

///////////////////////////////////////////////////////////////////////////////
void OpcPlugin::ReadValue( )
{    
    //This functions reads data through DynamicsDataBuffer
    //is it the active network ie is it being drawn
    if( m_canvas->GetActiveNetworkID() == m_network->GetNetworkID() )
    {        
        const CommandPtr opcData =
            DynamicsDataBuffer::instance()->GetCommand( "OPCData" );
        if( opcData->GetCommandName() == "NULL" )
        {
            return;
        }

        //std::string compName = GetVEModel()->GetPluginName();
        std::string tempData;
        //DataValuePairPtr tempDVP = opcData->GetDataValuePair( compName );
        DataValuePairPtr tempDVP = opcData->GetDataValuePair( ConvertUnicode( mPluginName.c_str() ) );
        //dynValue = "NA";
        if( tempDVP )
        {
            tempDVP->GetData( tempData );
            m_monValue = tempData;
            m_monValueExists = true;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
void OpcPlugin::DrawPlugin( wxDC* dc )
{
    //if hidden
    if(nameFlag)
    {
        DrawIcon( dc );
        DrawID( dc );
        DrawName( dc );
        if( m_monitoring )
        {
            ReadValue();
            if( m_monValueExists )
            {
                DrawValue( dc );
            }
        }
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

///////////////////////////////////////////////////////////////////////////////
void OpcPlugin::QueryForAllVariables( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Query Unit for all opc variables available
    //std::string compName = GetVEModel()->GetPluginName();
    //mPluginName = GetVEModel()->GetPluginName().c_str();
    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getAllOPCVariables" );
    returnState->AddDataValuePair( vendorData );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), ConvertUnicode( mPluginName.c_str() ) );
    returnState->AddDataValuePair( data );
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status );

    if( nw_str.compare("Error") == 0 || nw_str.compare("NULL") == 0 )
    {
        wxString title( _("Error") );
        wxString desc( _("Connection Issue.") );
        wxMessageDialog( m_canvas, desc, title ).ShowModal();
        return;
    }

    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector =
        networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd =
        boost::dynamic_pointer_cast<ves::open::xml::Command>
        ( objectVector.at( 0 ) );

    OpcUOVarDialog* params = new OpcUOVarDialog( GetPluginParent(), this, m_unitName );
    params->SetComponentName( mPluginName );
    params->SetServiceList( serviceList );

    //loop over all pairs
    int numdvps = cmd->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numdvps; i++ )
    {
        std::string name;
        std::string value;
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
        name = pair->GetDataName( );
        pair->GetData( value );
        params->SetData( wxString( name.c_str(), wxConvUTF8 ), wxString( value.c_str(), wxConvUTF8 ) );
    }
    
    //populate dialog
    params->ShowModal();
    params->Destroy();
}
///////////////////////////////////////////////////////////////////////////////
void OpcPlugin::OnShowAllVar( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    OpcUOVarDialog* params = new OpcUOVarDialog( GetPluginParent(), this, m_unitName );
    params->SetComponentName( mPluginName );
    //params->SetComponentName( wxString( compName.c_str(), wxConvUTF8 ) );
    params->SetServiceList( serviceList );
    //int numdvps = cmd->GetNumberOfDataValuePairs();
    //for( size_t i = 0; i < numdvps; i++ )
    //{
    //    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
    //    std::vector< std::string > temp_vector;
    //    pair->GetData( temp_vector );
    //    params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ), wxString( temp_vector[1].c_str(), wxConvUTF8 ),
    //        wxString( temp_vector[2].c_str(), wxConvUTF8 ), wxString( temp_vector[3].c_str(), wxConvUTF8 ) );
    //    //params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ) );
    //}
    //params->UpdateSizes();
    params->ShowModal();
    params->Destroy();
}
void OpcPlugin::OnMonitorVariable ( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    m_monitoring = true;
}
////////////////////////////////////////////////////////////////////////////////
bool OpcPlugin::ShowAvailable()
{
    return true;
}

////////////////////////////////////////////////////////////////////////////////
void OpcPlugin::OnConnect( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    ves::open::xml::CommandPtr monitor( new ves::open::xml::Command() );
    monitor->SetCommandName("connectToOPC");
    monitor->AddDataValuePair( vendorData );

    //ves::open::xml::DataValuePairPtr
    //    variables( new ves::open::xml::DataValuePair() );
    //variables->SetData( "variables", m_selectedOpcList );
    //monitor->AddDataValuePair( variables );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr,
        std::string >( monitor, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    std::string nw_str = serviceList->Query( status );

    DynamicsDataBuffer::instance()->Enable();

    mOpcMenu->Enable( OPCPLUGIN_CONNECT, false );
}

///////////////////////////////////////////////////////////////////////////////
void OpcPlugin::SetUnitName( std::string name )
{
    m_unitName = name;
    m_veModel->SetVendorName( m_unitName );
    vendorData = DataValuePairPtr( new DataValuePair() );
    vendorData->SetData( "vendorUnit", m_unitName );
}
