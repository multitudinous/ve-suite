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

#include "DSPlugin.h"
#include "DSOpenDialog.h"
#include "OPCDlg.h"

#include <plugins/ConductorPluginEnums.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <ves/conductor/xpm/AspenPlus2DIcons/sim.xpm>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/conductor/DynamicsDataBuffer.h>
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

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( DSPlugin, ves::conductor::UIPluginBase )
    EVT_MENU( DSPLUGIN_OPEN_SIM, DSPlugin::OnOpen )
    EVT_MENU( DSPLUGIN_CREATE_OPC_LIST, DSPlugin::OnCreateOPCList )
	EVT_MENU( DSPLUGIN_CONNECT, DSPlugin::OnConnect )
	EVT_TIMER( DSPLUGIN_TIMER_ID, DSPlugin::OnTimer )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( DSPlugin, ves::conductor::UIPluginBase )

/////////////////////////////////////////////////////////////////////////////
DSPlugin::DSPlugin() :
    UIPluginBase(),
    mDynSimMenu( 0 )
{
    mPluginName = wxString( "DynSim", wxConvUTF8 );
    mDescription = wxString( "DynSim Plugin", wxConvUTF8 );
    GetVEModel()->SetPluginType( "DSPlugin" );
    GetVEModel()->SetVendorName( "SIMSCI" );

    iconFilename = "sim";
    wxImage my_img( sim );
    SetImage( my_img );

	m_timer = new wxTimer( this, DSPLUGIN_TIMER_ID );
}
////////////////////////////////////////////////////////////////////////////////
DSPlugin::~DSPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
wxString DSPlugin::GetConductorName()
{
    return wxString( "DynSim_DS", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
void DSPlugin::OnOpen( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

		DSOpenDialog fd( m_canvas );
    fd.SetPopulateFilenames( );

    if( fd.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName xmlFileName;
    xmlFileName.ClearExt();
    xmlFileName.SetName( fd.GetFilename() + wxT(".xml") );

    CommandPtr returnState ( new Command() );
    returnState->SetCommandName( "getNetwork" );
    DataValuePairPtr data( new DataValuePair() );
    data->SetData( "NetworkQuery", "getNetwork" );
    returnState->AddDataValuePair( data );

    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "XMLFileName",  ConvertUnicode( xmlFileName.GetFullName().c_str() ) );
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
        wxMessageDialog md( m_canvas, wxT("VE-PSI not connected.\nPlease launch VE-PSI."), wxT("Error"), wxOK);
        md.ShowModal();
        return;
    }

    // If there is nothing on the CE
    if( nw_str.compare("XMLDNE") == 0 )
    {
        wxMessageDialog md(m_canvas, wxT("VE-PSI is unable to find the xml file.\nDid you select the correct directory in VE-PSI?" ), wxT("Error"), wxOK );
        md.ShowModal();
        //Log( "XML File Does NOT exist.\n" );
        return;
    }    
    else if( nw_str.compare("simDNE") == 0 )
    {
        wxMessageDialog md( m_canvas, wxT("VE-PSI Unit is unable to find the sim file.\nDid you select the correct directory in VE-PSI?" ), wxT("Error"), wxOK);
        md.ShowModal();
        //Log( "sim File Does NOT exist.\n" );
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
    ves::open::xml::model::ModelPtr dynSimModel;
    //set parent model on topmost level
    for( int modelCount = 0; modelCount < tempSystem->GetNumberOfModels(); modelCount++)
    {
        ModelPtr tempModel = tempSystem->GetModel( modelCount );
		tempModel->SetParentModel( dynSimModel );
		
		//go through models to find opc
		SystemPtr tempSubSystem = tempModel->GetSubSystem();
		for( int subModelCount = 0; 
			subModelCount < tempSubSystem->GetNumberOfModels(); 
			subModelCount++)
		{
			//add OPC plugins to a list
			std::string tempType = tempSubSystem->GetModel( subModelCount )->
				GetPluginType( );
			if(tempType.compare("OpcUOPlugin") == 0)
			{
				m_opcList.push_back(tempSubSystem->GetModel( subModelCount )->
					GetPluginName());
			}
		}
    }

    //dynSimModel->SetSubSystem( tempSystem );
    GetVEModel()->SetSubSystem( tempSystem );
    mDataBufferEngine->ParseSystem( tempSystem );

    //Network * network = m_canvas->GetActiveNetwork();

    //if( network->modules.empty() )
    //{
    //network->Load( nw_str, true );
    m_canvas->AddSubNetworks( );

	//m_subNetwork = m_canvas->GetNetwork( );

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
    CommandPtr dynSimXMLFile( new Command() );
    dynSimXMLFile->SetCommandName( "sim_Preferences" );
    data = DataValuePairPtr( new DataValuePair() );
    data->SetData( "XMLFileName",
                   ConvertUnicode( xmlFileName.GetFullName().c_str() ) );
    dynSimXMLFile->AddDataValuePair( data );
    mUserPrefBuffer->SetCommand( "sim_Preferences", dynSimXMLFile );

    SetName( fd.GetFilename() );
    event.SetId( UIPLUGINBASE_SET_UI_PLUGIN_NAME );
    GlobalNameUpdate( event );
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* DSPlugin::GetPluginPopupMenu( wxMenu* baseMenu )
{
    if( mDynSimMenu )
    {
        return baseMenu;
    }

    baseMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );
    (baseMenu->FindItemByPosition( 0 ))->GetSubMenu()->Enable( 
        UIPLUGINBASE_MODEL_INPUTS, false );
    (baseMenu->FindItemByPosition( 0 ))->GetSubMenu()->Enable( 
        UIPLUGINBASE_MODEL_RESULTS, false );
    (baseMenu->FindItemByPosition( 0 ))->GetSubMenu()->Enable( 
        UIPLUGINBASE_SHOW_ICON_CHOOSER, false );

    mDynSimMenu = new wxMenu();
    mDynSimMenu->Append( DSPLUGIN_OPEN_SIM, _( "Open" ) );
    mDynSimMenu->Enable( DSPLUGIN_OPEN_SIM, true );
    mDynSimMenu->Append( DSPLUGIN_CREATE_OPC_LIST, _( "Create List") );
    mDynSimMenu->Enable( DSPLUGIN_CREATE_OPC_LIST, true );
    mDynSimMenu->Append( DSPLUGIN_CONNECT, _( "Connect to OPC") );
    mDynSimMenu->Enable( DSPLUGIN_CONNECT, true );
    baseMenu->Insert( 0, DSPLUGIN_DYNSIM_MENU,   _( "DynSim" ), mDynSimMenu,
                    _( "Used in conjunction with DynSim" ) );
    baseMenu->Enable( DSPLUGIN_DYNSIM_MENU, true );
    return baseMenu;
}
////////////////////////////////////////////////////////////////////////////////
void DSPlugin::OnCreateOPCList( wxCommandEvent& event )
{
	//create dialog with list of available opc variables
	OPCDlg * opcDlg = new OPCDlg( m_canvas );

	//populate the dialog with available variables and selected
	//opcDlg->PopulateLists( m_opcList, m_selectedOpcList );
	opcDlg->SetParentPlugin( this );

	//display the dialog
	opcDlg->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void DSPlugin::OnConnect( wxCommandEvent& event )
{
    std::string compName = GetVEModel()->GetPluginName();
	ves::open::xml::CommandPtr monitor( new ves::open::xml::Command() );
    monitor->SetCommandName("connectWithList");

    ves::open::xml::DataValuePairPtr
        variables( new ves::open::xml::DataValuePair() );
    variables->SetData( "variables", m_selectedOpcList );
    monitor->AddDataValuePair( variables );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr,
		std::string >( monitor, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

	std::string nw_str = serviceList->Query( status );

	m_timer->Start( 4000 );
}

std::vector< std::string > DSPlugin::GetAvailableVariables()
{
	return m_opcList;
}

std::vector< std::string > DSPlugin::GetSelectVariables()
{
	return m_selectedOpcList;
}

void DSPlugin::SetSelectVariables( std::vector< std::string> selectedVariables )
{
	m_selectedOpcList = selectedVariables;
}

//this is a temporary function
//this timer event is used to update the dynamicsdatabuffer
//this timer event would make more sense in the buffer but is a difficult task
//in the end the buffer will be filled in another fashion
void DSPlugin::OnTimer( wxTimerEvent& event )
{
	DynamicsDataBuffer::instance()->Update();
}