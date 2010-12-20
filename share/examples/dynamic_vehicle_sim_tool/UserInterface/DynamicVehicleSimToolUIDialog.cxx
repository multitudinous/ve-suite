/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "DynamicVehicleSimToolUIDialog.h"
#include "CADListCreator.h"

// --- VE-Suite Includes --- //
#include <ves/conductor/util/spinctld.h>
#include <ves/conductor/UIPluginBase.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>

#include <ves/open/xml/model/Model.h>

// --- wxWidgets Includes --- //
#include <wx/statline.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/statbox.h>
#include <wx/frame.h>
#include <wx/textctrl.h>
#include <wx/notebook.h>
#include <wx/combobox.h>

#include <wx/filename.h>
#include <wx/choicdlg.h>

#include <wx/filedlg.h>
#include <wx/textdlg.h>

#include <fstream>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>

#include <boost/lexical_cast.hpp>

#include <Poco/SharedPtr.h>
#include <Poco/Tuple.h>
#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>

using namespace Poco::Data;


using namespace dynamicvehicletool;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolUIDialog::DynamicVehicleSimToolUIDialog()
    :
    dvst::DynamicVehicleSimToolBase( 0 ),
    mServiceList( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolUIDialog::DynamicVehicleSimToolUIDialog( 
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    dvst::DynamicVehicleSimToolBase( parent, id ),
    mServiceList( service )
{    
    CenterOnParent();
    //SetTitle( _("Deere Analytics") );
}
////////////////////////////////////////////////////////////////////////////////
DynamicVehicleSimToolUIDialog::~DynamicVehicleSimToolUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool DynamicVehicleSimToolUIDialog::TransferDataToWindow()
{
    PopulateDialogs();
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool DynamicVehicleSimToolUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnComputerNameEnter( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr computerNameText( new ves::open::xml::DataValuePair() );
    computerNameText->SetData( "ComputerName", ConvertUnicode( m_textCtrl1->GetValue().c_str() ) );
    ves::open::xml::DataValuePairPtr computerPortText( new ves::open::xml::DataValuePair() );
    computerPortText->SetData( "ComputerPort", ConvertUnicode( m_textCtrl2->GetValue().c_str() ) );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( computerNameText );
    command->AddDataValuePair( computerPortText );
    const std::string commandName = "Computer Info Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnPortNumberEnter( wxCommandEvent& event )
{
    OnComputerNameEnter( event );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnStartStopButton( wxCommandEvent& WXUNUSED( event ) )
{
    bool state = m_toggleBtn1->GetValue();
    std::string simState = "Stop";
    if( state )
    {
        simState = "Start";
    }
    ves::open::xml::DataValuePairPtr simText( new ves::open::xml::DataValuePair() );
    simText->SetData( "Simulator State", simState );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( simText );
    const std::string commandName = "Simulator Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnResetSimulation( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr simText( new ves::open::xml::DataValuePair() );
    simText->SetData( "Simulator State", "Reset" );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( simText );
    const std::string commandName = "Simulator Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnGeometryDataMapping( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnAddGeometryGroupButton( wxCommandEvent& WXUNUSED( event ) )
{
    size_t newNum = m_geomChoiceList.size();
	wxBoxSizer* bSizer;
	bSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticText* staticText = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxString::Format( _("%d"), newNum), wxDefaultPosition, wxDefaultSize, 0 );
	staticText->Wrap( -1 );
	bSizer->Add( staticText, 0, wxALIGN_CENTER, 5 );
	
    ves::open::xml::cad::CADNodePtr rootNode = mUIPluginBase->GetVEModel()->GetGeometry();
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    std::vector< ves::open::xml::cad::CADNodePtr > nodeList = 
        nodeListCreator.GetNodeList();
	wxArrayString m_choice11Choices;
    for( size_t i = 0; i < nodeList.size(); ++i )
    {
        m_choice11Choices.Add( wxString( nodeList.at( i )->GetNodeName().c_str(), wxConvUTF8 ) );
    }
	wxChoice* choice = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice11Choices, m_choice11Choices.GetCount() );
	choice->SetSelection( 0 );
	bSizer->Add( choice, 0, wxALIGN_CENTER, 5 );
	
	bSizer9->Add( bSizer, 0, 0, 5 );
    m_scrolledWindow1->Layout();
    m_geomChoiceList.push_back( choice );
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnRemoveGeometryGroupButton( wxCommandEvent& WXUNUSED( event ) )
{
    wxSizerItemList& list = bSizer9->GetChildren();
    size_t num = list.size();
    if( num > 0 )
    {
        ///Remove the last item;
        list.back()->DeleteWindows();
        bSizer9->Remove( num - 1 );
        //m_geomChoiceList.back()->Destroy();
        m_geomChoiceList.pop_back();
    }
    m_scrolledWindow1->Layout();
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnConstrainedGeometrySelection( wxCommandEvent& WXUNUSED( event ) )
{
    ves::open::xml::DataValuePairPtr constrainedText( new ves::open::xml::DataValuePair() );
    constrainedText->SetData( "Contrainted Geometry", ConvertUnicode( m_choice3->GetStringSelection().c_str() ) );
    
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 
    command->AddDataValuePair( constrainedText );
    const std::string commandName = "Geometry Map Update";
    command->SetCommandName( commandName );
    mServiceList->SendCommandStringToXplorer( command );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnApplyButton( wxCommandEvent& event )
{
    OnComputerNameEnter( event );
    OnConstrainedGeometrySelection( event );
    UpdateModelData();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::OnOKButton( wxCommandEvent& event )
{
    OnApplyButton( event );
    //Do not do anything and close the dialog
    Close();
}
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::UpdateModelData()
{
    ves::open::xml::model::ModelPtr tempModel = mUIPluginBase->GetVEModel();

    ves::open::xml::CommandPtr toolCommand( new ves::open::xml::Command() );
    toolCommand->SetCommandName( "Tool Info" );

    ves::open::xml::DataValuePairPtr constrainedText( new ves::open::xml::DataValuePair() );
    constrainedText->SetData( "Contrainted Geometry", ConvertUnicode( m_choice3->GetStringSelection().c_str() ) );
    toolCommand->AddDataValuePair( constrainedText );
    
    ves::open::xml::DataValuePairPtr computerNameText( new ves::open::xml::DataValuePair() );
    computerNameText->SetData( "ComputerName", ConvertUnicode( m_textCtrl1->GetValue().c_str() ) );
    toolCommand->AddDataValuePair( computerNameText );

    ves::open::xml::DataValuePairPtr computerPortText( new ves::open::xml::DataValuePair() );
    computerPortText->SetData( "ComputerPort", ConvertUnicode( m_textCtrl2->GetValue().c_str() ) );
    toolCommand->AddDataValuePair( computerPortText );
    tempModel->SetInput( toolCommand );
    mServiceList->SendCommandStringToXplorer( toolCommand );

    ves::open::xml::CommandPtr geomCommand( new ves::open::xml::Command() );
    geomCommand->SetCommandName( "Geometry Data Map" );
    for( size_t i = 0; i < m_geomChoiceList.size(); ++i )
    {
        std::string dvpName = "Geometry_" + boost::lexical_cast<std::string>( i );

        ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
        geomDVP->SetData( dvpName, ConvertUnicode( m_geomChoiceList.at( i )->GetStringSelection().c_str() ) );
        geomCommand->AddDataValuePair( geomDVP );
    }

    if( m_geomChoiceList.size() == 0 )
    {
        ves::open::xml::DataValuePairPtr geomDVP( new ves::open::xml::DataValuePair() );
        geomDVP->SetData( "No Geometry Selected", "No Geom" );
        geomCommand->AddDataValuePair( geomDVP );
    }
    tempModel->SetInput( geomCommand );
    mServiceList->SendCommandStringToXplorer( geomCommand );
 }
////////////////////////////////////////////////////////////////////////////////
void DynamicVehicleSimToolUIDialog::PopulateDialogs()
{
    ves::open::xml::model::ModelPtr tempModel = mUIPluginBase->GetVEModel();
    
    ves::open::xml::CommandPtr toolCommand = tempModel->GetInput( "Tool Info" );
    std::string constrainedGeom;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "Contrainted Geometry" )->GetData( constrainedGeom );
    }
    
    ves::open::xml::cad::CADNodePtr rootNode = tempModel->GetGeometry();
    dynamicvehicletool::CADListCreator nodeListCreator( rootNode );
    std::vector< ves::open::xml::cad::CADNodePtr > nodeList = 
        nodeListCreator.GetNodeList();
	wxArrayString m_choice11Choices;
    for( size_t i = 0; i < nodeList.size(); ++i )
    {
        m_choice11Choices.Add( wxString( nodeList.at( i )->GetNodeName().c_str(), wxConvUTF8 ) );
    }
	m_choice3->Append( m_choice11Choices );
    m_choice3->SetSelection( 0 );
    m_choice3->SetStringSelection( wxString( constrainedGeom.c_str(), wxConvUTF8 ) );
    
    std::string computerName;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "ComputerName" )->GetData( computerName );
    }
    m_textCtrl1->ChangeValue( wxString( computerName.c_str(), wxConvUTF8 ) );

    std::string computerPort;
    if( toolCommand )
    {
        toolCommand->GetDataValuePair( "ComputerPort" )->GetData( computerPort );
    }
    m_textCtrl2->ChangeValue( wxString( computerPort.c_str(), wxConvUTF8 ) );

    toolCommand = tempModel->GetInput( "Geometry Data Map" );
    if( !toolCommand )
    {
        return;
    }

    size_t numDVPs = toolCommand->GetNumberOfDataValuePairs();
    wxCommandEvent event;
    std::string nodeName;
    for( size_t i = 0; i < numDVPs; ++i )
    {
        OnAddGeometryGroupButton( event );
        ves::open::xml::DataValuePairPtr geomDVP = 
            toolCommand->GetDataValuePair( i );
        geomDVP->GetData( nodeName );
        m_geomChoiceList.at( i )->SetStringSelection( wxString( nodeName.c_str(), wxConvUTF8 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
