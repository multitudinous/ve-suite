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

#include "APUOPlugin.h"
#include "APUOVarDialog.h"
#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>

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

BEGIN_EVENT_TABLE( APUOPlugin, UIPluginBase )
    EVT_MENU( APUOPLUGIN_SHOW_ASPEN_NAME, APUOPlugin::OnShowAspenName )
    EVT_MENU( APUOPLUGIN_QUERY_INPUTS, APUOPlugin::OnQueryInputs )
    EVT_MENU( APUOPLUGIN_QUERY_OUTPUTS, APUOPlugin::OnQueryOutputs )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( APUOPlugin, UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
APUOPlugin::APUOPlugin() :
    UIPluginBase(),
    mAspenMenu( 0 )
{
    mPluginName = wxString( "AspenPlusUO", wxConvUTF8 );
    mDescription = wxString( "Aspen Plus Unit Operation Plugin", wxConvUTF8 );
    m_pluginType = "APUOPlugin" ;
    iconFilename = "square";
    wxImage my_img( square_xpm );
    SetImage( my_img );
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
    ves::open::xml::model::ModelPtr parentTraverser = m_veModel;
    if( parentTraverser != NULL )
    {
        while( parentTraverser->GetParentModel() != NULL &&
            parentTraverser->GetParentModel()->GetPluginType().compare( "APPlugin" ) != 0 )
        {
            //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
            parentTraverser = parentTraverser->GetParentModel();
            compName = "Data.Blocks." + parentTraverser->GetPluginName() + "." + compName;
        }
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getInputModuleParamList" );
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
    if( nw_str.empty() )
    {
        wxMessageDialog( m_canvas, wxT( "Aspen Unit Unavailable." ),
            wxT( "Error" ), wxOK | wxICON_ERROR ).ShowModal();
        return;
    }

    wxString title( compName.c_str(), wxConvUTF8 );
    //TextResultDialog * results = new TextResultDialog(this, title);
    //QueryInputsDlg * results = new QueryInputsDlg(this);
    APUOVarDialog* params = new APUOVarDialog( GetPluginParent(), m_unitName );
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
    ves::open::xml::model::ModelPtr parentTraverser = m_veModel;
    if( parentTraverser != NULL )
    {
        while( parentTraverser->GetParentModel() != NULL &&
            parentTraverser->GetParentModel()->GetPluginType().compare( "APPlugin" ) != 0 )
        {
            //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
            parentTraverser = parentTraverser->GetParentModel();
            compName = "Data.Blocks." + parentTraverser->GetPluginName() + "." + compName;

        }
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOutputModuleParamList" );
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
    if( nw_str.empty() )
    {
        wxMessageDialog( m_canvas, wxT( "Aspen Unit Unavailable." ),
            wxT( "Error" ), wxOK | wxICON_ERROR ).ShowModal();
        return;
    }

    wxString title( compName.c_str(), wxConvUTF8 );
    //QueryInputsDlg * results = new QueryInputsDlg(this);
    APUOVarDialog * params = new APUOVarDialog( GetPluginParent(), m_unitName );
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
////////////////////////////////////////////////////////////////////////////////
wxMenu* APUOPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mAspenMenu )
    {
        return baseMenu;
    }
    
    //set the vendor name of the current plugin to the parents
    if( GetVEModel()->GetParentModel() == NULL )
    {
        m_unitName = m_veModel->GetParentModel()->GetVendorName();
        m_veModel->SetVendorName( m_unitName );
        vendorData = DataValuePairPtr( new DataValuePair() );
        vendorData->SetData( "vendorUnit", m_unitName );
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    mAspenMenu = new wxMenu();
    mAspenMenu->Append( APUOPLUGIN_SHOW_ASPEN_NAME, _( "Name" ) );
    mAspenMenu->Enable( APUOPLUGIN_SHOW_ASPEN_NAME, true );
    mAspenMenu->Append( APUOPLUGIN_QUERY_INPUTS, _( "Inputs" ) );
    mAspenMenu->Enable( APUOPLUGIN_QUERY_INPUTS, true );
    mAspenMenu->Append( APUOPLUGIN_QUERY_OUTPUTS, _( "Results" ) );
    mAspenMenu->Enable( APUOPLUGIN_QUERY_OUTPUTS, true );
    mAspenMenu->Append( APUOPLUGIN_REINIT_BLOCK, _( "Reinitialize" ) );
    mAspenMenu->Enable( APUOPLUGIN_REINIT_BLOCK, true );
    
    baseMenu->Insert( 0, APUOPLUGIN_ASPEN_MENU,   _( "Aspen" ), mAspenMenu,
                    _( "Used in conjunction with Aspen" ) );
    baseMenu->Enable( APUOPLUGIN_ASPEN_MENU, true );
    return baseMenu;
}
////////////////////////////////////////////////////////////////////////////////
bool APUOPlugin::ShowAvailable()
{
    return true;
}