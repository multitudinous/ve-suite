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
#include "APVarDialog.h"

#include <ves/conductor/xpm/square.xpm>
#include <ves/conductor/util/ParamsDlg.h>

#include <ves/open/xml/model/Model.h>

#include <ves/open/xml/XMLReaderWriter.h>

#include <wx/menu.h>

#include <wx/msgdlg.h>
#include <wx/image.h>
#include <wx/scrolwin.h>
#include <wx/window.h>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

#define edge_size 10

/*BEGIN_EVENT_TABLE( APPlugin, wxEvtHandler )
    EVT_MENU( SHOW_ASPEN_NAME, APPlugin::OnShowAspenName )
    EVT_MENU( QUERY_INPUTS, APPlugin::OnQueryInputs )
    EVT_MENU( QUERY_OUTPUTS, APPlugin::OnQueryOutputs )
    EVT_MENU( REINIT_BLOCK, APPlugin::OnReinitBlocks )
END_EVENT_TABLE()*/

IMPLEMENT_DYNAMIC_CLASS( APPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
APPlugin::APPlugin() :
    UIPluginBase()
{
    name = wxString( "AspenPlusPlugin", wxConvUTF8 );

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

    //create the menu
    mPopMenu = new wxMenu();

    //Aspen Menu
    mPopMenu->Append( SHOW_ASPEN_NAME, _( "Aspen Name" ) );
    mPopMenu->Enable( SHOW_ASPEN_NAME, true );
    mPopMenu->Append( QUERY_INPUTS, _( "Query Inputs" ) );
    mPopMenu->Enable( QUERY_INPUTS, true );
    mPopMenu->Append( QUERY_OUTPUTS, _( "Query Outputs" ) );
    mPopMenu->Enable( QUERY_OUTPUTS, true );
    mPopMenu->Append( REINIT_BLOCK, _( "Reinitialize" ) );
    mPopMenu->Enable( REINIT_BLOCK, true );
}

////////////////////////////////////////////////////////////////////////////////
APPlugin::~APPlugin()
{
}

////////////////////////////////////////////////////////////////////////////////
void  APPlugin::OnShowAspenName( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    ves::open::xml::model::ModelPtr veModel = GetVEModel();
    wxString title;
    title << wxT( "Aspen Name" );
    wxString desc( veModel->GetModelName().c_str(), wxConvUTF8 );
    //wxMessageDialog( m_canvas, desc, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
//void  APPlugin::OnShowIconChooser( wxCommandEvent& event )
//{
//    UIPLUGIN_CHECKID( event )
//    serviceList->GetMessageLog()->SetMessage( "Icon Chooser\n" );
/*    APPlugin* tempPlugin = this;
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
void  APPlugin::OnQueryInputs( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();
    compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        compName = "Data.Blocks." + parentTraverser->GetModelName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
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
    APVarDialog* params = new APVarDialog( m_canvas );
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
void  APPlugin::OnQueryOutputs( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();
    compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        compName = "Data.Blocks." + parentTraverser->GetModelName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
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
    ParamsDlg * params = new ParamsDlg( m_canvas );
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
void  APPlugin::OnReinitBlocks( wxCommandEvent& event )
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
}