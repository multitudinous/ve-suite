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

#include "ADUOPlugin.h"
#include <plugins/ConductorPluginEnums.h>
#include "ADUOVarDialog.h"

#include <ves/conductor/xpm/square.xpm>

#include <ves/open/xml/model/Model.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ADUOPlugin, UIPluginBase )
    EVT_MENU( ADUOPLUGIN_SHOW_ASPEN_NAME, ADUOPlugin::OnShowAspenName )
    EVT_MENU( ADUOPLUGIN_QUERY_DYNAMICS, ADUOPlugin::OnQueryDynamics )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( ADUOPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
ADUOPlugin::ADUOPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "AspenDynamicsUO", wxConvUTF8 );
    GetVEModel()->SetPluginType( "ADUOPlugin" );

}
////////////////////////////////////////////////////////////////////////////////
ADUOPlugin::~ADUOPlugin()
{
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
    ADUOVarDialog* params = new ADUOVarDialog( m_canvas );
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
    
    mAspenMenu = new wxMenu();
    mAspenMenu->Append( ADUOPLUGIN_SHOW_ASPEN_NAME, _( "Aspen Name" ) );
    mAspenMenu->Enable( ADUOPLUGIN_SHOW_ASPEN_NAME, true );
    mAspenMenu->Append( ADUOPLUGIN_QUERY_DYNAMICS, _( "Query Dynamics" ) );
    mAspenMenu->Enable( ADUOPLUGIN_QUERY_DYNAMICS, true );
    baseMenu->Insert( 0, ADUOPLUGIN_ASPEN_MENU,   _( "Aspen" ), mAspenMenu,
                    _( "Used in conjunction with Aspen" ) );
    baseMenu->Enable( ADUOPLUGIN_ASPEN_MENU, true );
    return baseMenu;
}
