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
#include "ValveDlg.h"
#include "SwitchDlg.h"
#include "TankDlg.h"
#include <ves/conductor/util/OpcUOVarDialog.h>
//#include "DynamicDataDlg.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/DynamicsDataBuffer.h>

#include <ves/conductor/xpm/square.xpm>

#include <ves/open/xml/model/Model.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( OpcUOPlugin, UIPluginBase )
    EVT_MENU( OPCUOPLUGIN_SHOW_VALUE, OpcUOPlugin::OnShowValue )
    //EVT_TIMER( OPCUOPLUGIN_TIMER_ID, OpcUOPlugin::OnTimer )
    //EVT_MENU( OPCUOPLUGIN_START_TIMER, OpcUOPlugin::StartTimer )
    //EVT_MENU( OPCUOPLUGIN_STOP_TIMER, OpcUOPlugin::StopTimer )
    //EVT_MENU( OPCUOPLUGIN_ALL_VAR, OpcUOPlugin::OnShowAllVar )
    EVT_MENU( OPCUOPLUGIN_ALL_VAR, OpcUOPlugin::QueryForAllVariables )
    EVT_BUTTON( OPC_VAR_ID_MONITORBUTTON, OpcUOPlugin::OnMonitorVariable )
    EVT_MENU( OPCUOPLUGIN_VALVE_CAD, OpcUOPlugin::OnValveCAD )
    EVT_MENU( OPCUOPLUGIN_SWITCH_CAD, OpcUOPlugin::OnSwitchCAD )
    //EVT_MENU( OPCUOPLUGIN_TANK_CAD, OpcUOPlugin::OnTankCAD )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( OpcUOPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
OpcUOPlugin::OpcUOPlugin() :
    UIPluginBase(),
    mOpcMenu( 0 )
{
    mPluginName = wxString( "OpcUO", wxConvUTF8 );
    mDescription = wxString( "OPC Unit Operation Plugin", wxConvUTF8 );
    m_pluginType = "OpcUOPlugin";
    iconFilename = "square";
    wxImage my_img( square_xpm );
    SetImage( my_img );
    m_monValue = "NA";
    m_monValueExists = false;
    m_monitoring = false;
}
////////////////////////////////////////////////////////////////////////////////
OpcUOPlugin::~OpcUOPlugin()
{
    //m_timer->Stop();
    //delete m_timer;
    //m_timer = 0;
}
////////////////////////////////////////////////////////////////////////////////
wxString OpcUOPlugin::GetConductorName()
{
    return wxString( "DynSim_OpcUnitOp", wxConvUTF8 );
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
wxMenu* OpcUOPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mOpcMenu )
    {
        return baseMenu;
    }
    
    //set the vendor name of the current plugin to the parents
    if( GetVEModel()->GetParentModel() )
    {
        m_unitName = m_veModel->GetParentModel()->GetVendorName();
        m_veModel->SetVendorName( m_unitName );
        vendorData = DataValuePairPtr( new DataValuePair() );
        vendorData->SetData( "vendorUnit", m_unitName );
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    mOpcMenu = new wxMenu();
    //mOpcMenu->Append( OPCUOPLUGIN_SHOW_VALUE, _( "Value" ) );
    //mOpcMenu->Enable( OPCUOPLUGIN_SHOW_VALUE, true );
    //mOpcMenu->Append( OPCUOPLUGIN_START_TIMER, _( "Start Timer" ) );
    //mOpcMenu->Enable( OPCUOPLUGIN_START_TIMER, true );
    //mOpcMenu->Append( OPCUOPLUGIN_STOP_TIMER, _( "Stop Timer" ) );
    //mOpcMenu->Enable( OPCUOPLUGIN_STOP_TIMER, true );
    mOpcMenu->Append( OPCUOPLUGIN_ALL_VAR, _( "ALL VAR" ) );
    mOpcMenu->Enable( OPCUOPLUGIN_ALL_VAR, true );

    //using the icon file name to determine if the opc plugin
    //represents a switch, valve or tank
    //create options for the CAD dialogs
    if( iconFilename.find("BREAKER") != std::string::npos )
    {
        mOpcMenu->Append( OPCUOPLUGIN_SWITCH_CAD, _( "CAD" ) );
        mOpcMenu->Enable( OPCUOPLUGIN_SWITCH_CAD, true );
    }
    else if (iconFilename.find("valve") != std::string::npos)
    {
        mOpcMenu->Append( OPCUOPLUGIN_VALVE_CAD, _( "CAD" ) );
        mOpcMenu->Enable( OPCUOPLUGIN_VALVE_CAD, true );
    }
    else if (iconFilename.find("tank") != std::string::npos)
    {
        mOpcMenu->Append( OPCUOPLUGIN_TANK_CAD, _( "CAD" ) );
        mOpcMenu->Enable( OPCUOPLUGIN_TANK_CAD, true );
    }
    baseMenu->Insert( 0, OPCUOPLUGIN_SIM_MENU,   _( "OPC" ), mOpcMenu,
                    _( "Used in conjunction with OPC" ) );
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

    dc->GetTextExtent( wxString( m_monValue.c_str(), wxConvUTF8), &w, &h );
    dc->DrawText( wxString( m_monValue.c_str(), wxConvUTF8 ), int( x - w / 2 + xoff ), pos.y + int( y * 4.0 ) );
}

//this function does a read by querying the unit
/*void OpcUOPlugin::ReadValue( )
{
    std::string compName = GetVEModel()->GetPluginName();
    //compName = "Data.Blocks." + compName;

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOPCValue" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    //hardcode the "D1_"  This will need to be parsed from the .tree file
    //data->SetData( std::string( "ModuleName" ), "D1_"+compName );
    data->SetData( std::string( "ModuleName" ), compName );
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
}*/
///////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::ReadValue( )
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
void OpcUOPlugin::OnTimer( wxTimerEvent& event )
{
    //UIPLUGIN_CHECKID( event )
    //if( m_canvas != NULL && m_network != NULL )
    //{
        //UIPLUGIN_CHECKID( event )
    //    ReadValue();
    //    m_canvas->Refresh( true );
    //}
}
///////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::DrawPlugin( wxDC* dc )
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
void OpcUOPlugin::StartTimer( float msec )
{
    //UIPLUGIN_CHECKID( event )
    //if( m_timer != NULL )
    //{
    //    m_timer->Stop();
    //    delete m_timer;
    //    m_timer = NULL;
    //}
    //m_timer = new wxTimer( this, OPCUOPLUGIN_TIMER_ID );
    //m_timer->Start(msec);
    //dynValue = "NA";
}
///////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::StopTimer( wxCommandEvent& event )
{
    //UIPLUGIN_CHECKID( event )
    //m_timer->Stop();
}
///////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::QueryForAllVariables( wxCommandEvent& event )
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
void OpcUOPlugin::OnShowAllVar( wxCommandEvent& event )
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
void OpcUOPlugin::OnMonitorVariable ( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    m_monitoring = true;
}
////////////////////////////////////////////////////////////////////////////////
bool OpcUOPlugin::ShowAvailable()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::OnValveCAD( wxCommandEvent& event )
{
	ValveDlg* vd = new ValveDlg( 0, serviceList );
	vd->ShowModal( );
}
////////////////////////////////////////////////////////////////////////////////
void OpcUOPlugin::OnSwitchCAD( wxCommandEvent& event )
{
	SwitchDlg* sd = new SwitchDlg( 0, serviceList );
	sd->ShowModal( );
}
////////////////////////////////////////////////////////////////////////////////
//void OpcUOPlugin::OnTankCAD( wxCommandEvent& event )
//{
//	TankDlg* td = new TankDlg( 0, serviceList );
//	td->ShowModal( );
//}