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

#include "APUOPlugin.h"
#include "APUOVarDialog.h"
#include "../ConductorPluginEnums.h"

#include <ves/conductor/xpm/square.xpm>
#include <ves/conductor/util/ParamsDlg.h>

#include <ves/open/xml/model/Model.h>

#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/menu.h>

#include <wx/msgdlg.h>
#include <wx/image.h>
#include <wx/scrolwin.h>
#include <wx/window.h>
#include <wx/filedlg.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

#define edge_size 10

BEGIN_EVENT_TABLE( APUOPlugin, UIPluginBase )
    EVT_MENU( APUOPLUGIN_SHOW_ASPEN_NAME, APUOPlugin::OnShowAspenName )
    EVT_MENU( APUOPLUGIN_QUERY_INPUTS, APUOPlugin::OnQueryInputs )
    EVT_MENU( APUOPLUGIN_QUERY_OUTPUTS, APUOPlugin::OnQueryOutputs )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( APUOPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
APUOPlugin::APUOPlugin() :
    UIPluginBase()
{
    mPluginName = wxString( "AspenPlusUO", wxConvUTF8 );
    //GetVEModel()->SetPluginType( "APUOPlugin" );

    wxImage my_img( square_xpm );
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

    //Aspen Menu
    wxMenu * aspen_menu = new wxMenu();
    aspen_menu->Append( APUOPLUGIN_SHOW_ASPEN_NAME, _( "Aspen Name" ) );
    aspen_menu->Enable( APUOPLUGIN_SHOW_ASPEN_NAME, true );
    aspen_menu->Append( APUOPLUGIN_QUERY_INPUTS, _( "Query Inputs" ) );
    aspen_menu->Enable( APUOPLUGIN_QUERY_INPUTS, true );
    aspen_menu->Append( APUOPLUGIN_QUERY_OUTPUTS, _( "Query Outputs" ) );
    aspen_menu->Enable( APUOPLUGIN_QUERY_OUTPUTS, true );
    aspen_menu->Append( APUOPLUGIN_REINIT_BLOCK, _( "Reinitialize" ) );
    aspen_menu->Enable( APUOPLUGIN_REINIT_BLOCK, true );
    mPopMenu->Insert( 0, APUOPLUGIN_ASPEN_MENU,   _( "Aspen" ), aspen_menu,
                     _( "Used in conjunction with Aspen" ) );
    mPopMenu->Enable( APUOPLUGIN_ASPEN_MENU, true );
}

////////////////////////////////////////////////////////////////////////////////
APUOPlugin::~APUOPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
wxString APUOPlugin::GetConductorName()
{
    return wxString( "Aspen_Plus_APUnitOp", wxConvUTF8 );
}
////////////////////////////////////////////////////////////////////////////////
void  APUOPlugin::OnShowAspenName( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    ves::open::xml::model::ModelPtr veModel = GetVEModel();
    wxString title;
    title << wxT( "Aspen Name" );
    wxString desc( veModel->GetPluginName().c_str(), wxConvUTF8 );
    wxMessageDialog( m_canvas, desc, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
//void  APUOPlugin::OnShowIconChooser( wxCommandEvent& event )
//{
//    UIPLUGIN_CHECKID( event )
//    serviceList->GetMessageLog()->SetMessage( "Icon Chooser\n" );
/*    APUOPlugin* tempPlugin = this;
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
void  APUOPlugin::OnQueryInputs( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetPluginName();
    compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = GetVEModel();//parentModel.lock();
    if( parentTraverser != NULL )
    {
        while( parentTraverser->GetParentModel() != NULL )
        {
            //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
            parentTraverser = parentTraverser->GetParentModel();
            compName = "Data.Blocks." + parentTraverser->GetPluginName() + "." + compName;
        }
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getInputModuleParamList" );
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
    wxString title( compName.c_str(), wxConvUTF8 );
    //TextResultDialog * results = new TextResultDialog(this, title);
    //QueryInputsDlg * results = new QueryInputsDlg(this);
    APUOVarDialog* params = new APUOVarDialog( m_canvas );
    //params->SetPosition( wxPoint(dialogSize.x, dialogSize.y) );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    //serviceList->GetMessageLog()->SetMessage(nw_str.c_str());
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    //std::ostringstream output;
    //output << objectVector.size()<<std::endl;
    //serviceList->GetMessageLog()->SetMessage(output.str().c_str());
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    std::vector< std::string > temp_vector;
    pair->GetData( temp_vector );

    params->SetCompName( compName.c_str() );
    params->SetServiceList( serviceList );
    params->SetDialogType( "input" );
    for( size_t i = 0; i < temp_vector.size(); i++ )
        params->AppendList( temp_vector[i].c_str() );
    params->ShowModal();
    params->Destroy();
    //serviceList->GetMessageLog()->SetMessage("gather");
    //gather requested inputs
    //std::vector< std::string > temp_vector2;
    //for(int testing = 0; testing < results->GetDataSize(); testing++)
    // temp_vector2.push_back(std::string(results->GetDataString(testing).c_str()));

    //serviceList->GetMessageLog()->SetMessage("submit or not");
    //if it is submit launch request
    //if(results->IsSubmit())
    // this->OnQueryInputModuleProperties(temp_vector2, compName);
}

////////////////////////////////////////////////////////////////////////////////
void  APUOPlugin::OnQueryOutputs( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetPluginName();
    compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    if( parentTraverser != NULL )
    {
        while( parentTraverser->GetParentModel() != NULL )
        {
            //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
            compName = "Data.Blocks." + parentTraverser->GetPluginName() + "." + compName;
            parentTraverser = parentTraverser->GetParentModel();
        }
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOutputModuleParamList" );
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
    wxString title( compName.c_str(), wxConvUTF8 );
    //QueryInputsDlg * results = new QueryInputsDlg(this);
    APUOVarDialog * params = new APUOVarDialog( m_canvas );
    //params->SetPosition( wxPoint(dialogSize.x, dialogSize.y) );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    std::vector< std::string > temp_vector;
    pair->GetData( temp_vector );

    params->SetCompName( compName.c_str() );
    params->SetServiceList( serviceList );
    params->SetDialogType( "output" );
    for( size_t i = 0; i < temp_vector.size(); i++ )
        params->AppendList( temp_vector[i].c_str() );
    params->ShowModal();
    params->Destroy();
    //std::vector< std::string > temp_vector2;
    //for(int testing = 0; testing < results->GetDataSize(); testing++)
    // temp_vector2.push_back(std::string(results->GetDataString(testing).c_str()));

    //if(results->IsSubmit())
    // this->OnQueryOutputModuleProperties(temp_vector2, compName);
}
////////////////////////////////////////////////////////////////////////////////
/*void  APUOPlugin::OnReinitBlocks( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        compName = parentTraverser->GetModelName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "reinitBlock" );
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
    serviceList->Query( status );
}*/