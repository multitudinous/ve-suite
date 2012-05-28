/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include "ADUOPlugin.h"
#include "ADUOVarDialog.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/DynamicsDataBuffer.h>

#include <ves/conductor/xpm/square.xpm>

#include <ves/open/xml/model/Model.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ADUOPlugin, UIPluginBase )
    EVT_MENU( ADUOPLUGIN_SHOW_ASPEN_NAME, ADUOPlugin::OnShowAspenName )
    EVT_MENU( ADUOPLUGIN_QUERY_DYNAMICS, ADUOPlugin::OnQueryDynamics )
    EVT_TIMER( ADUOPLUGIN_TIMER_ID, ADUOPlugin::OnTimer )
    EVT_MENU( ADUOPLUGIN_STOP_TIMER, ADUOPlugin::StopTimer )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( ADUOPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
ADUOPlugin::ADUOPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "AspenDynamicsUO", wxConvUTF8 );
    mDescription = wxString( "Aspen Dynamics Unit Operation Plugin", wxConvUTF8 );
    m_pluginType = "ADUOPlugin";
    iconFilename = "square";
    wxImage my_img( square_xpm );
    SetImage( my_img );
    m_monValue = "NA";
    m_monValueExists = false;
    m_monitoring = false;
}
////////////////////////////////////////////////////////////////////////////////
ADUOPlugin::~ADUOPlugin()
{
    m_timer->Stop();
    delete m_timer;
    m_timer = 0;
}
////////////////////////////////////////////////////////////////////////////////
wxString ADUOPlugin::GetConductorName()
{
    return wxString( "Aspen_Dynamics_ADUnitOp", wxConvUTF8 );
}
////////////////////////////////////////////////////////////////////////////////
void  ADUOPlugin::OnShowAspenName( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    ves::open::xml::model::ModelPtr veModel = GetVEModel();
    wxString title;
    title << wxT( "Aspen Name" );
    wxString desc( veModel->GetPluginName().c_str(), wxConvUTF8 );
    wxMessageDialog( m_canvas, desc, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
//void  ADUOPlugin::OnShowIconChooser( wxCommandEvent& event )
//{
//    UIPLUGIN_CHECKID( event )
//    serviceList->GetMessageLog()->SetMessage( "Icon Chooser\n" );
/*    ADUOPlugin* tempPlugin = this;
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
void ADUOPlugin::OnQueryDynamics( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetPluginName();
    //compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = m_veModel;

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
    returnState->AddDataValuePair( vendorData );
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
    ADUOVarDialog* params = new ADUOVarDialog( GetPluginParent() );
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
wxMenu* ADUOPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mAspenMenu )
    {
        return baseMenu;
    }

    //set the vendor name of the current plugin to the parents
    if( GetVEModel()->GetParentModel() )
    {
        m_unitName = GetVEModel()->GetParentModel()->GetVendorName();
        m_veModel->SetVendorName( m_unitName );
        vendorData = DataValuePairPtr( new DataValuePair() );
        vendorData->SetData( "vendorUnit", m_unitName );
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    mAspenMenu = new wxMenu();
    mAspenMenu->Append( ADUOPLUGIN_SHOW_ASPEN_NAME, _( "Name" ) );
    mAspenMenu->Enable( ADUOPLUGIN_SHOW_ASPEN_NAME, true );
    mAspenMenu->Append( ADUOPLUGIN_QUERY_DYNAMICS, _( "All Variables" ) );
    mAspenMenu->Enable( ADUOPLUGIN_QUERY_DYNAMICS, true );
    baseMenu->Insert( 0, ADUOPLUGIN_ASPEN_MENU,   _( "Aspen" ), mAspenMenu,
                      _( "Used in conjunction with Aspen" ) );
    baseMenu->Enable( ADUOPLUGIN_ASPEN_MENU, true );
    return baseMenu;
}
///////////////////////////////////////////////////////////////////////////////
void ADUOPlugin::DrawPlugin( wxDC* dc )
{
    //if hidden
    if( nameFlag )
    {
        DrawIcon( dc );
        DrawID( dc );
        DrawName( dc );
        if( m_monValueExists )
        {
            DrawValue( dc );
        }
    }

    //if highlighted
    if( highlightFlag )
    {
        if( nameFlag )
        {
            HighlightSelectedIcon( dc );
        }
        DrawPorts( true, dc );
    }
}
void ADUOPlugin::DrawValue( wxDC* dc )
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

    dc->GetTextExtent( wxString( m_monValue.c_str(), wxConvUTF8 ), &w, &h );
    dc->DrawText( wxString( m_monValue.c_str(), wxConvUTF8 ), int( x - w / 2 + xoff ), pos.y + int( y * 4.0 ) );
}
///////////////////////////////////////////////////////////////////////////////
void ADUOPlugin::ReadValue( )
{
    //This functions reads data through DynamicsDataBuffer
    //is it the active network ie is it being drawn
    if( m_canvas->GetActiveNetworkID() == m_network->GetNetworkID() )
    {
        const CommandPtr adData =
            DynamicsDataBuffer::instance()->GetCommand( "ADData" );
        if( adData->GetCommandName() == "NULL" )
        {
            return;
        }

        //std::string compName = GetVEModel()->GetPluginName();
        std::string tempData;
        //DataValuePairPtr tempDVP = opcData->GetDataValuePair( compName );
        DataValuePairPtr tempDVP = adData->GetDataValuePair( ConvertUnicode( mPluginName.c_str() ) );
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
void ADUOPlugin::OnTimer( wxTimerEvent& event )
{
    if( m_canvas != NULL && m_network != NULL )
    {
        //UIPLUGIN_CHECKID( event )
        ReadValue();
        m_canvas->Refresh( true );
    }
}
///////////////////////////////////////////////////////////////////////////////
void ADUOPlugin::StartTimer( float msec )
{
    //UIPLUGIN_CHECKID( event )
    //if( m_timer != NULL )
    //{
    //    m_timer->Stop();
    //    delete m_timer;
    //    m_timer = NULL;
    //}
    m_timer = new wxTimer( this, OPCUOPLUGIN_TIMER_ID );
    m_timer->Start( msec );
}
///////////////////////////////////////////////////////////////////////////////
void ADUOPlugin::StopTimer( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    m_timer->Stop();
}
///////////////////////////////////////////////////////////////////////////////
void ADUOPlugin::OnMonitorVariable( wxCommandEvent& event )
{
    m_monitoring = true;
}
////////////////////////////////////////////////////////////////////////////////
bool ADUOPlugin::ShowAvailable()
{
    return true;
}
///////////////////////////////////////////////////////////////////////////////
